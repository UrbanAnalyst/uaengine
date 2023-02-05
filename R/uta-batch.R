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
    fnames <- fs::path_abs (fs::path (results_path, fnames))
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
    f <- fs::path_abs (fs::path (results_path, f))

    return (f)
}

batch_progress_message <- function (i, vsp, pt0, t_start) {

    cli::cli_h1 (cli::col_yellow (paste0 (
        i, " / ", length (vsp), " = ",
        round (100 * i / length (vsp), digits = 2), "%"
    )))

    pt1 <- proc.time () - pt0
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

    results_path <- fs::path_abs (results_path)

    flist <- fs::dir_ls (results_path, regexp = city, fixed = TRUE)

    res <- do.call (rbind, unname (lapply (flist, readRDS)))

    return (res)
}

#' Export results to the form required for the front-end app
#'
#' @inheritParams uta_index_batch
#' @param soc An \pkg{sf} `data.frame` object with polygons defining areas in
#' which socio-demographic variables were collated, and a column called
#' "social_index" containing the socio-demographic variable of interest.
#' @return The input table, `soc`, with additional columns of UTA index values
#' attached. This table can then be saved and used directly in the UTA
#' front-end.
#' @export

uta_export <- function (city, soc, results_path) {

    if (!"social_index" %in% names (soc)) {
        stop ("'soc' must include a 'social_index' column", call. = FALSE)
    }

    res <- batch_collate_results (results_path, city)

    vars_rel <- grep ("^int_d[0-9]+\\_pop\\_adj$", names (res), value = TRUE)
    vars_abs <- grep ("^times\\_limit", names (res), value = TRUE)

    pt_index <- unlist (sf::st_within (res, soc))

    dlims <- vapply (vars_abs, function (i) {
        regmatches (i, gregexpr ("[0-9]+$", i)) [[1]]
    }, character (1L), USE.NAMES = FALSE)

    trans_vars_rel <- paste0 ("trans_rel_d", dlims)
    trans_vars_abs <- paste0 ("trans_abs_d", dlims)
    for (i in c (trans_vars_rel, trans_vars_abs)) {
        soc [[i]] <- NA
    }

    for (i in unique (pt_index)) {
        index <- which (pt_index == i)
        for (d in dlims) {
            var_abs <- grep (d, vars_abs, value = TRUE)
            soc [[grep (d, trans_vars_abs, value = TRUE)]] [i] <-
                mean (res [[var_abs]] [index], na.rm = TRUE) / 60
            var_rel <- grep (d, vars_rel, value = TRUE)
            soc [[grep (d, trans_vars_rel, value = TRUE)]] [i] <-
                mean (res [[var_rel]] [index], na.rm = TRUE)
        }
    }
    soc <- soc [which (!is.na (soc$social_index)), ]

    uta_vars_rel <- paste0 ("uta_index_rel_d", dlims)
    uta_vars_abs <- paste0 ("uta_index_abs_d", dlims)
    for (i in c (uta_vars_rel, uta_vars_abs)) {
        soc [[i]] <- NA
    }

    for (i in c (trans_vars_rel, trans_vars_abs)) {

        transport_sd <- stats::sd (soc [[i]], na.rm = TRUE)
        transport_mn <- mean (soc [[i]], na.rm = TRUE)
        social_index <- transport_mn +
            (soc$social_index - mean (soc$social_index, na.rm = TRUE)) *
                transport_sd
        nm <- gsub ("^trans", "uta_index", i)
        x <- soc [[i]] * social_index
        soc [[nm]] <- sign (x) * sqrt (abs (x))
    }

    return (soc)
}
