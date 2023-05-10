#' Calculate "UTA_index" for a specified set of origin locations.
#'
#' @param city Name of city, used to name and define local path to
#' pre-calculated street networks and transport times with \pkg{m4ra} package.
#' @param gtfs_path Path to `.Rds` version of GTFS feed for specified city.
#' @param osm_path Path to OSM data processed by \link{uta_extract_osm} function
#' (only needed first time to call internal `uta_prepare_data()` function).
#' @param popdens_geotif Optional path to local 'geotiff' file with population
#' density estimates. If provided, all transport indices are adjusted to account
#' for effects of local population density.
#' @param from Character vector of Open Street Map IDs of vertices from which to
#' calculate travel times.
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
#' @param duration_max Parameter to control maximal duration examined in
#' multi-modal routing queries, specified in minutes. This value must be
#' strictly greater than the maximal value of `dlims`, and is used to speed up
#' multi-modal queries. May be set to `NULL` to return full results from all
#' possible durations / distances.
#' @param quiet If `FALSE`, display progress information on screen.
#'
#' @return An \pkg{sf} `data.frame`, with geometries of points defined by
#' `from`, and values for transport indices at specified distances, both raw
#' values and values adjusted for local population densities, along with
#' corresponding values of the UTA index at each value of 'dlim'.
#'
#' @export

uta_index <- function (city,
                       gtfs_path,
                       osm_path = NULL,
                       popdens_geotif = NULL,
                       from = NULL,
                       initial_mode = "foot",
                       final_mode = "foot",
                       soc = NULL,
                       soc_var = NULL,
                       dlims = c (5, 10),
                       duration_max = 15,
                       quiet = FALSE) {

    checkmate::assert_character (city, len = 1L)
    city <- tolower (gsub ("\\s+", "-", city))

    checkmate::assert_file_exists (gtfs_path)
    if (!is.null (popdens_geotif)) {
        checkmate::assert_file_exists (popdens_geotif)
    }
    checkmate::assert_character (from, min.len = 1L)

    checkmate::assert_character (initial_mode, len = 1L)
    checkmate::assert_character (final_mode, len = 1L)
    dodgr_modes <- unique (dodgr::weighting_profiles$weighting_profiles$name)
    initial_mode <- match.arg (tolower (initial_mode), dodgr_modes)
    final_mode <- match.arg (tolower (final_mode), dodgr_modes)

    if (is.null (soc)) {
        stop ("'soc' must be provided", call. = FALSE)
    }
    checkmate::assert_character (soc_var, min.len = 1L, max.len = 1L)
    checkmate::assert_numeric (dlims, lower = 0, min.len = 1L)
    if (!is.null (duration_max)) {
        checkmate::assert_numeric (duration_max, lower = 0, len = 1L)
        checkmate::assert_true (duration_max > max (dlims))
        duration_max <- duration_max * 60
    }

    cache_dir <- fs::path (m4ra_cache_dir (), city)
    f_extra <- NULL
    if (fs::dir_exists (cache_dir)) {

        f_extra <- fs::dir_ls (cache_dir, regex = "\\-(natural|school)\\-")
        if (length (f_extra) != 2L) {

            if (!is.null (osm_path)) {

                f_extra <- uta_prepare_data (
                    osm_path,
                    water_dist = 20,
                    quiet = quiet
                )
            }
        }

        f_natural <- grep ("natural", f_extra, value = TRUE)
    }

    if (!quiet) {
        cli::cli_alert_info (cli::col_blue ("Caclulating multi-modal times"))
        st0 <- proc.time ()
    }

    dat <- m4ra::m4ra_times_mm_car (
        gtfs = gtfs_path,
        city_name = city,
        from = from,
        initial_mode = initial_mode,
        final_mode = final_mode,
        duration_max = duration_max,
        quiet = quiet
    )

    if (!quiet) {
        st <- hms::hms (round ((proc.time () - st0) [3]))
        msg <- "Caclulated multi-modal times in "
        cli::cli_alert_success (cli::col_green (msg, st))
    }

    s <- travel_time_statistics (dat, dlims = dlims, quiet)
    rm (dat)
    if (!is.null (popdens_geotif)) {
        s <- add_popdens_to_stats (s, popdens_geotif)
    }
    s <- add_socio_var_to_stats (s, soc, soc_var)
    s <- add_dist_to_schools (s, city)
    s <- add_bike_infrastructure (s, city, dlimit = 5000)
    if (length (f_natural) > 0L) {
        s <- add_natural_space_index (s, city, f_natural, dlimit = 2000)
    }
    s <- add_parking (s, city, initial_mode)

    return (s)
}

#' Convert travel time matrices to summary statistics at each 'from' point
#' @noRd
travel_time_statistics <- function (dat, dlims = c (5, 10), quiet) {

    if (!quiet) {
        msg <- "Calculating summary statistics at each origin point"
        cli::cli_alert_info (cli::col_blue (msg))
        st0 <- proc.time ()
    }

    num_cores <- get_num_cores () # m4ra internal fn

    stats <- parallel::mclapply (seq_len (nrow (dat$dist)), function (i) {
        df <- data.frame (
            d = dat$dist [i, ],
            ratio = dat$ratio [i, ],
            mm_times = dat$mm_times [i, ] / 60, # minutes
            mm_transfers = dat$mm_transfers [i, ],
            mm_intervals = dat$mm_intervals [i, ] / 60 # minutes
        )
        df <- df [which (
            !is.na (df$ratio) & !is.nan (df$ratio) & is.finite (df$ratio)
        ), ]
        if (nrow (df) < (ncol (dat$ratio) / 2)) {
            return (c (NA, NA))
        }

        index <- which (df$d <= max (dlims))
        predict1 <- function (fml, df, index, dlims) {

            mod <- tryCatch (
                stats::lm (fml, data = df [index, ]),
                error = function (e) NULL
            )
            if (is.null (mod)) {
                res <- rep (NA, length (dlims))
            } else {
                res <- stats::predict (mod, newdata = data.frame (d = dlims))
            }
            return (res)
        }

        fmls <- c (
            ratio ~ d,
            mm_times ~ d,
            mm_transfers ~ d,
            mm_intervals ~ d
        )
        res <- lapply (fmls, function (f) predict1 (f, df, index, dlims))
        res <- do.call (c, res)

        dlim_p <- sprintf ("%02d", dlims)
        names (res) <- c (
            paste0 ("times_rel_d", dlim_p),
            paste0 ("times_abs_d", dlim_p),
            paste0 ("transfers_d", dlim_p),
            paste0 ("intervals_d", dlim_p)
        )

        return (res)
    }, mc.cores = num_cores)

    stats <- data.frame (do.call (rbind, stats))

    stats$x <- dat$v_from$x
    stats$y <- dat$v_from$y
    # stats <- stats [which (!is.na (stats$intercept_all)), ]

    stats <- cbind ("osm_id" = rownames (dat$dist), stats)

    if (!quiet) {
        st <- hms::hms (round ((proc.time () - st0) [3]))
        msg <- "Caclulated summary statistics at each origin point in "
        cli::cli_alert_success (cli::col_green (msg, st))
    }

    return (stats)
}

add_popdens_to_stats <- function (s, popdens_geotif) {

    nms <- names (s)
    s <- pop2point (s, popdens_geotif, normalise = FALSE)
    names (s) [which (!names (s) %in% nms)] <- "popdens"

    return (s)
}

add_socio_var_to_stats <- function (s, soc, soc_var) {

    sxy <- sfheaders::sf_point (s [, c ("x", "y")])
    sxy <- sf::st_sf (sxy)
    sf::st_crs (sxy) <- 4326

    index <- unlist (sf::st_within (sxy, soc))
    s$soc_var <- soc [[soc_var]] [index]

    return (s)
}
