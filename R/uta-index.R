
#' Calculate "UTA_index" for a specified set of origin locations.
#'
#' @param city Name of city, used to name and define local path to
#' pre-calculated street networks and transport times with \pkg{m4ra} package.
#' @param gtfs Path to `.Rds` version of GTFS feed for specified city.
#' @param popdens_geotif Path to local 'geotiff' file with population density
#' estimates.
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
#' @param quiet If `FALSE`, display progress information on screen.
#'
#' @export

uta_index <- function (city,
                       gtfs,
                       popdens_geotif,
                       from = NULL,
                       initial_mode = "foot",
                       final_mode = "foot",
                       soc = NULL,
                       soc_var = NULL,
                       quiet = FALSE) {

    city <- tolower (gsub ("\\s+", "-", city))

    checkmate::assert_file_exists (gtfs)
    checkmate::assert_file_exists (popdens_geotif)
    checkmate::assert_character (from, min.len = 1L)
    if (is.null (soc)) {
        stop ("'soc' must be provided", call. = FALSE)
    }
    checkmate::assert_character (soc_var, min.len = 1L, max.len = 1L)

    if (!quiet) {
        cli::cli_alert_info (cli::col_blue ("Caclulating multi-modal times"))
        st0 <- proc.time ()
    }

    dat <- m4ra::m4ra_times_mm_car (
        gtfs = gtfs,
        city_name = city,
        from = from,
        initial_mode = initial_mode,
        final_mode = final_mode,
        quiet = TRUE
    )

    if (!quiet) {
        st <- hms::hms (round ((proc.time () - st0) [3]))
        cli::cli_alert_success (cli::col_green ("Caclulated multi-modal times in ", st))
    }

    dlims <- c (5, 10)

    s <- travel_time_statistics (dat, dlims = c (5, 10), quiet)
    s <- add_popdens_to_stats (s, popdens_geotif)
    s <- add_socio_var_to_stats (s, soc, soc_var)
    s <- add_uta_index (s, dlims)

    return (s)
}

#' Convert travel time matrices to summary statistics at each 'from' point
#' @noRd
travel_time_statistics <- function (dat, dlims = c (5, 10), quiet) {

    if (!quiet) {
        cli::cli_alert_info (cli::col_blue ("Calculating summary statistics at each origin point"))
        st0 <- proc.time ()
    }

    stats <- lapply (seq_len (nrow (dat$dist)), function (i) {
        df <- data.frame (d = dat$dist [i, ], ratio = dat$ratio [i, ])
        df <- df [which (!is.na (df$ratio) & !is.nan (df$ratio)), ]
        if (nrow (df) < (ncol (dat$ratio) / 2)) {
            return (c (NA, NA))
        }
        mod0 <- stats::lm (ratio ~ d, data = df)
        mod1 <- stats::lm (ratio ~ d, data = df [df$d <= 10.0, ])
        c (
            summary (mod0)$coefficients [1:2],
            summary (mod1)$coefficients [1:2]
        )
    })

    stats <- data.frame (do.call (rbind, stats))
    names (stats) <- c ("intercept_all", "slope_all", "intercept_d10", "slope_d10")
    stats$x <- dat$v_from$x
    stats$y <- dat$v_from$y
    stats <- stats [which (!is.na (stats$intercept_all)), ]

    for (d in dlims) {
        par_name <- paste0 ("integral_d", sprintf ("%02d", d))
        stats [[par_name]] <- stats$intercept_d10 + stats$slope_d10 * d
    }

    if (!quiet) {
        st <- hms::hms (round ((proc.time () - st0) [3]))
        cli::cli_alert_success (cli::col_green ("Caclulated summary statistics at each origin point in ", st))
    }

    return (stats)
}

add_popdens_to_stats <- function (s, geotif) {

    s <- pop2point (s, geotif, normalise = FALSE)
    dlims <- grep ("^integral\\_d", names (s), value = TRUE)
    for (d in dlims) {
        mod <- stats::lm (as.formula (paste0 (d, " ~ layer")), data = s)
        par_name <- paste0 ("int", gsub ("^integral", "", d), "_pop_adj")
        s [[par_name]] <- mean (s [[d]]) + mod$residuals
    }

    return (s)
}

add_socio_var_to_stats <- function (s, soc, soc_var) {

    dlims <- grep ("^integral\\_d", names (s), value = TRUE)
    dlims <- gsub ("^integral\\_", "", dlims)

    vars <- c (paste0 ("integral_", dlims), paste0 ("int_", dlims, "_pop_adj"))

    s <- cbind (s [vars], sfheaders::sf_point (s [, c ("x", "y")]))
    s <- sf::st_sf (s)
    sf::st_crs (s) <- 4326

    index <- unlist (sf::st_within (s, soc))
    s$soc_var <- soc [[soc_var]] [index]
    s$soc_var <- s$soc_var / mean (s$soc_var)

    return (s)
}

add_uta_index <- function (s, dlims) {

    for (d in dlims) {
        d_fmt <- sprintf ("%02d", d)
        tr_index <- paste0 ("int_d", d_fmt, "_pop_adj")
        par_name <- paste0 ("uta_index_d", d_fmt)
        s [[par_name]] <- s$soc_var * s [[tr_index]]
    }

    return (s)
}
