#' @title Batch calculation of 'uta_index' for a single city
#'
#' @description Repeatedly calculates values from the \link{uta_index} function
#' for a sub-set of all origin vertices within nominated city, and saves results
#' to a local file. Finally results can generally be reliably interpolated, and
#' it is thus generally not necessarily to calculate \link{uta_index} for every
#' origin point. Instead, it is recommended to only calculate values for a fixed
#' proportion specified by the parameter, `coverage`.
#'
#' @note Importantly, this function can safely be interrupted at any time, and
#' will simply restart at the point where it was stopped.
#'
#' @param city Name of city, used to name and define local path to
#' pre-calculated street networks and transport times with \pkg{m4ra} package.
#' @param gtfs_path Path to `.Rds` version of GTFS feed for specified city.
#' @param popdens_geotif Optional path to local 'geotiff' file with population
#' density estimates. If provided, all transport indices are adjusted to account
#' for effects of local population density.
#' @param results_path Local path to directory where results are to be written.
#' @param initial_mode Initial mode of transport from each 'from' point to
#' public transport system (or to destination points, where single-model
#' transport is faster).
#' @param final_mode Final mode of transport from each public transport
#' destination point to all other network nodes.
#' @param soc Socio-demographic data with an \pkg{sf}-format column of polygons
#' for each observed value of target variable.
#' @param soc_var Name of target variable in `soc` data set.
#' @param dlims Vector of distance limits in kilometres over which average
#' values of transport index should be calculated. One value of "uta_index" is
#' then derived for each value of `dlims`.
#' @param batch_size Number of vertices to calculate in each batch before saving
#' to local file. Increases in this parameter can greatly increase the sizes of
#' intermediate results, leading to memory problems. Note that batch runs will
#' only be able to stopped and restarted if this parameter is kept constant.
#' Changes to this parameter will cause re-started runs to revert to the
#' beginning, and will generate results which overlap with any previous results
#' still retained in `results_path`.
#' @param coverage Proportion of all points for which values are to be
#' calculated. As described above, coverage can generally safely be set to
#' values less than 1, with complete coverage then generated through the more
#' computationally efficient \link{uta_interpolate} function.
#' @param nthreads Optional parameter to specify number of threads to be used in
#' multi-threaded calculations. Default is maximum number of available threads
#' (from `RcppParallel::defaultNumThreads()`).
#'
#' @export

uta_index_batch <- function (city,
                             gtfs_path,
                             popdens_geotif = NULL,
                             results_path = NULL,
                             initial_mode = "foot",
                             final_mode = "foot",
                             soc = NULL,
                             soc_var = NULL,
                             dlims = c (5, 10),
                             batch_size = 1000,
                             coverage = 1 / 4,
                             nthreads = NULL) {

    # most parameters are asserted in 'uta_index' function
    if (!is.null (nthreads)) {
        checkmate::assert_integer (nthreads, lower = 1L, len = 1L)
        requireNamespace ("RcppParallel")
        RcppParallel::setThreadOptions (numThreads = nthreads)
    }
    checkmate::assert_integer (batch_size, lower = 1L, len = 1L)
    checkmate::assert_numeric (coverage, lower = 0.01, upper = 1, len = 1L)
    checkmate::assert_numeric (coverage, lower = 0.01, upper = 1, len = 1L)

    # Parameters used before passing to 'uta_index':
    checkmate::assert_character (city, len = 1L)
    city <- tolower (gsub ("\\s+", "-", city))
    checkmate::assert_character (initial_mode, len = 1L)
    dodgr_modes <- unique (dodgr::weighting_profiles$weighting_profiles$name)
    initial_mode <- match.arg (tolower (initial_mode), dodgr_modes)

    v <- get_batch_vertices (soc, city, initial_mode)

    tmp <- get_vertex_indices (v, batch_size, coverage, city, results_path)
    vsp <- tmp$vsp
    index <- tmp$index
    rm (tmp)

    t_start <- batch_start_message ()

    for (i in seq_along (vsp)) {

        f <- get_batch_filename (results_path, city, index [[i]])

        from <- vsp [[i]]$id
        pt0 <- proc.time ()

        s <- uta_index (
            city = city,
            gtfs_path = gtfs_path,
            popdens_geotif = popdens_geotif,
            from = from,
            initial_mode = initial_mode,
            final_mode = final_mode,
            soc = soc,
            soc_var = soc_var,
            quiet = TRUE
        )
        saveRDS (s, f)

        batch_progress_message (i, vsp, pt0, t_start)
    }

    return (batch_collate_results (results_path, city))
}

#' Return all vertices of network weighted for nominated `mode`, ordered to
#' regularly sample all polygons in `soc`.
#'
#' @param soc Social-demograph data as an \pkg{sf} `data.frame` object with
#' polygons in which variables are measured.
#' @param mode Mode of transport for which origin vertices are to be sampled.
#' @param seed Should not be changed! Leaving at fixed value ensures that
#' vertices will always be returned in same order, allowing batch runs to be
#' re-started at exact points where they are stopped.
#' @noRd
get_batch_vertices <- function (soc, city, mode, seed = 1L) {

    v <- uta_vertices (soc, city, mode)

    # order vertices to reguarly sample all polygons in 'soc':
    vxy <- sfheaders::sf_point (v [, c ("x", "y")])
    sf::st_crs (vxy) <- 4326
    index0 <- unlist (sf::st_within (vxy, soc))

    # 'cpp_index_sort' returns the direct index back into original, rearranged
    # to evenly sample each polygon in succession.
    index <- cpp_index_sort (index0) + 1

    return (v [index, ])
}

#' Split vertices into list items of 'batch_size' each, along with corresponding
#' indices into the original vertex table.
#' @noRd
get_vertex_indices <- function (v, batch_size, coverage, city, results_path) {

    n <- ceiling (nrow (v) / batch_size)
    index <- rep (seq_len (n), each = batch_size) [seq_len (nrow (v))]
    vsp <- split (v, f = as.factor (index))
    index <- split (seq_len (nrow (v)), f = as.factor (index))

    index2 <- seq_len (ceiling (length (vsp) * coverage))
    vsp <- vsp [index2]
    index <- index [index2]

    # Then remove any which have previously been done:
    ivals <- vapply (index, max, integer (1L))
    fnames <- paste0 (city, "-", sprintf ("%06i", ivals), ".Rds")
    fnames <- fs::path_abs (fs::path (results_path, city, fnames))
    index2 <- which (!fs::file_exists (fnames))

    cli::cli_alert_info (cli::col_cyan (paste0 (
        "Skipping [",
        length (fnames) - length (index2),
        "] previously calculated files, with [",
        length (index2),
        "] still to go."
    )))

    vsp <- vsp [index2]
    index <- index [index2]

    return (list (vsp = vsp, index = index))
}

batch_start_message <- function () {

    msg <- paste0 ("Staring processing at ", Sys.time ())
    cli::cli_h1 (cli::col_green (msg))
    cat ("\n")

    return (proc.time ())
}

#' @param index One list element of the full `index` generated by
#' `get_vertex_indices()`.
#' @noRd
get_batch_filename <- function (results_path, city, index) {

    ival <- max (index)
    f <- paste0 (city, "-", sprintf ("%06i", ival), ".Rds")
    city_dir <- fs::path_abs (fs::path (results_path, city))
    if (!fs::dir_exists (city_dir)) {
        fs::dir_create (city_dir)
    }
    f <- fs::path_abs (fs::path (city_dir, f))

    return (f)
}

batch_progress_message <- function (i, vsp, pt0, t_start) {

    cli::cli_h1 (cli::col_yellow (paste0 (
        i, " / ", length (vsp), " = ",
        round (100 * i / length (vsp), digits = 2), "%"
    )))

    dur1 <- hms::hms (as.integer ((proc.time () - pt0) [3]))
    dur_tot <- hms::hms (as.integer ((proc.time () - t_start) [3]))
    tremaining <- (length (vsp) - i) * dur_tot / i
    tremaining <- hms::hms (as.integer (tremaining))

    cli::cli_alert_info (cli::col_green (paste0 (
        "Calculation time: ", dur1,
        "; total = ", dur_tot,
        "; remaining = ", tremaining
    )))
}

batch_collate_results <- function (results_path, city) {

    results_path <- fs::path_abs (fs::path (results_path, city))

    flist <- fs::dir_ls (results_path, regexp = city, fixed = TRUE)

    res <- do.call (rbind, unname (lapply (flist, readRDS)))

    return (res)
}

#' Export results to the form required for the front-end app
#'
#' @note All exported variables are transformed such that lower/negative values
#' are good, while higher/positive values are bad. This requires inverting these
#' variables:
#' \itemize{
#' \item bike_index
#' \item natural
#' }
#' Those are both on unit scales, so require simple '1 - x' transforms.
#'
#' @inheritParams uta_index_batch
#' @param soc An \pkg{sf} `data.frame` object with polygons defining areas in
#' which socio-demographic variables were collated, and a column called
#' "social_index" containing the socio-demographic variable of interest. If not
#' provided, results are not aggregated and simply returned for each original
#' point.
#' @param dlim Return results for specified value(s) of dlim only. If only one
#' value specified, names of results will not be labelled with actual value.
#' @return The input table, `soc`, with additional columns of UTA index values
#' attached. This table can then be saved and used directly in the UTA
#' front-end.
#' @export

uta_export <- function (city, results_path, soc = NULL, dlim = 10) {

    if (!is.null (soc)) {
        if (!"social_index" %in% names (soc)) {
            stop ("'soc' must include a 'social_index' column", call. = FALSE)
        }
    }

    res <- batch_collate_results (results_path, city)

    dlim_fmt <- paste0 ("\\_d", sprintf ("%02i", dlim), "$")
    check <- vapply (dlim_fmt, function (i) {
        any (grepl (i, names (res)))
    }, logical (1L))
    if (any (!check)) {
        stop (
            "dlim value of [",
            paste0 (dlim [which (!check)], collapse = ", "),
            "] is not included in data.",
            call. = FALSE
        )
    }
    index_all <- grep ("\\_d[0-9]+$", names (res))
    index_dlim <- lapply (dlim_fmt, function (i) grep (i, names (res)))
    index_dlim <- sort (unique (unlist (index_dlim)))
    index_not <- index_all [which (!index_all %in% index_dlim)]
    if (length (index_not) > 0L) {
        res <- res [, -index_not]
    }
    if (length (dlim) == 1L) {
        names (res) <- gsub (dlim_fmt, "", names (res))
        dlim_grep <- ""
    } else {
        dlim_grep <- "\\_d"
    }

    vars <- c ("times_rel", "times_abs", "transfers", "intervals")
    vars <- lapply (vars, function (v) {
        grep (paste0 (v, dlim_grep, "$"), names (res), value = TRUE)
    })
    vars <- unique (unlist (vars))

    index <- which (!names (res) %in% c (vars, "osm_id", "x", "y", "soc_var"))
    extra_vars <- names (res) [index]
    # Generally "popdens", "school_dist", "bike_index", "natural"

    if (!is.null (soc)) {

        res_xy <- sfheaders::sf_point (res [, c ("x", "y")])
        sf::st_crs (res_xy) <- 4326
        pt_index <- unlist (sf::st_within (res_xy, soc))

        for (v in c (vars, extra_vars)) {
            soc [[v]] <- NA
        }

        for (i in unique (pt_index)) {
            index <- which (pt_index == i)
            for (v in c (vars, extra_vars)) {
                if (v %in% c ("parking", "school_dist")) {
                    p <- res [[v]] [index]
                    p <- p [which (p > 0 & !is.nan (p))]
                    soc [[v]] [i] <- 10^mean (log10 (p), na.rm = TRUE)
                } else {
                    soc [[v]] [i] <- mean (res [[v]] [index], na.rm = TRUE)
                }
            }
        }

        soc <- soc [which (!is.na (soc$social_index)), ]

    } else {
        soc <- res
        extra_vars <- c (extra_vars, c ("osm_id", "x", "y"))
    }

    vars2keep <- c (
        vars,
        extra_vars,
        "social_index",
        "soc_var",
        grep ("^geom", names (soc), value = TRUE)
    )
    soc <- soc [, which (names (soc) %in% vars2keep)]

    # Code to transform bike_index, natural by inverting, but this is no longer
    # done.
    # soc$bike_index <- 1 - soc$bike_index
    # soc$natural <- 1 - soc$natural
    #
    # names (soc) [which (names (soc) == "bike_index")] <- "anti_bike"
    # names (soc) [which (names (soc) == "natural")] <- "anti_nature"

    # Log-transform school distances:
    soc$school_dist [soc$school_dist < 1] <- 1 # 1 metre!
    soc$school_dist <- log10 (soc$school_dist)

    if ("soc_var" %in% names (soc)) {
        names (soc) [which (names (soc) == "soc_var")] <- "social_index"
    }

    return (soc)
}
