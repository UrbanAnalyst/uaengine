#' Calculate "UTA_index" for a specified set of origin locations.
#'
#' @param city Name of city, used to name and define local path to
#' pre-calculated street networks and transport times with \pkg{m4ra} package.
#' @param gtfs_path Path to `.Rds` version of GTFS feed for specified city.
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
                       popdens_geotif = NULL,
                       from = NULL,
                       initial_mode = "foot",
                       final_mode = "foot",
                       soc = NULL,
                       soc_var = NULL,
                       dlims = c (5, 10),
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
    s <- add_uta_index (s, dlims)

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

    stats <- lapply (seq_len (nrow (dat$dist)), function (i) {
        df <- data.frame (
            d = dat$dist [i, ],
            ratio = dat$ratio [i, ],
            mm_times = dat$mm_times [i, ]
        )
        df <- df [which (
            !is.na (df$ratio) & !is.nan (df$ratio) & is.finite (df$ratio)
        ), ]
        if (nrow (df) < (ncol (dat$ratio) / 2)) {
            return (c (NA, NA))
        }
        mod0 <- stats::lm (ratio ~ d, data = df)
        mod1 <- stats::lm (ratio ~ d, data = df [df$d <= 10.0, ])
        res <- c (
            summary (mod0)$coefficients [1:2],
            summary (mod1)$coefficients [1:2]
        )

        # Then mm_times vs dist
        mod2 <- stats::lm (mm_times ~ d, data = df)
        mod3 <- stats::lm (mm_times ~ d, data = df [df$d <= 10.0, ])
        res <- c (
            res,
            stats::predict (mod2, newdata = data.frame (d = dlims)),
            stats::predict (mod3, newdata = data.frame (d = dlims))
        )

        return (res)
    })

    stats <- data.frame (do.call (rbind, stats))
    dlim_p <- sprintf ("%02d", dlims)
    pred_nms <- c (paste0 ("times_d", dlim_p), paste0 ("times_limit_d", dlim_p))
    names (stats) <- c (
        "intercept_all", "slope_all", "intercept_d10", "slope_d10", pred_nms
    )
    stats$x <- dat$v_from$x
    stats$y <- dat$v_from$y
    # stats <- stats [which (!is.na (stats$intercept_all)), ]

    stats <- cbind ("osm_id" = rownames (dat$dist), stats)

    for (d in dlims) {
        par_name <- paste0 ("integral_d", sprintf ("%02d", d))
        stats [[par_name]] <- stats$intercept_d10 + stats$slope_d10 * d
    }

    if (!quiet) {
        st <- hms::hms (round ((proc.time () - st0) [3]))
        msg <- "Caclulated summary statistics at each origin point in "
        cli::cli_alert_success (cli::col_green (msg, st))
    }

    return (stats)
}

add_popdens_to_stats <- function (s, geotif) {

    s <- pop2point (s, geotif, normalise = FALSE)
    dlims <- grep ("^integral\\_d", names (s), value = TRUE)
    for (d in dlims) {
        mod <- stats::lm (stats::as.formula (paste0 (d, " ~ layer")), data = s)
        par_name <- paste0 ("int", gsub ("^integral", "", d), "_pop_adj")
        # 'mod' will exclude any NA values of 'par_name', so need to index back
        # into 's':
        index <- match (mod$model [[d]], s [[d]])
        s [[par_name]] [index] <- mean (s [[d]], na.rm = TRUE) + mod$residuals
    }

    return (s)
}

add_socio_var_to_stats <- function (s, soc, soc_var) {

    dlims <- grep ("^integral\\_d", names (s), value = TRUE)
    dlims <- gsub ("^integral\\_", "", dlims)

    vars <- c (
        "osm_id",
        paste0 ("integral_", dlims),
        paste0 ("int_", dlims, "_pop_adj"),
        paste0 ("times_limit_", dlims)
    )
    vars <- vars [which (vars %in% names (s))]

    s <- cbind (s [vars], sfheaders::sf_point (s [, c ("x", "y")]))
    s <- sf::st_sf (s)
    sf::st_crs (s) <- 4326

    index <- unlist (sf::st_within (s, soc))
    s$soc_var <- soc [[soc_var]] [index]
    s$soc_var <- s$soc_var / mean (s$soc_var, na.rm = TRUE)

    return (s)
}

add_uta_index <- function (s, dlims) {

    for (d in dlims) {

        # Index from ratios relative to car times:
        d_fmt <- sprintf ("%02d", d)
        tr_index <- paste0 ("int_d", d_fmt, "_pop_adj")
        if (!tr_index %in% names (s)) { # no population density data
            tr_index <- paste0 ("integral_d", d_fmt)
        }
        par_name <- paste0 ("uta_index_rel_d", d_fmt)
        s [[par_name]] <- s$soc_var * s [[tr_index]]

        # Index from absolute multi-modal journey times:
        tt_index <- paste0 ("times_limit_d", d_fmt)
        par_name <- paste0 ("uta_index_abs_d", d_fmt)
        s [[par_name]] <- s$soc_var * s [[tt_index]]
    }

    return (s)
}
