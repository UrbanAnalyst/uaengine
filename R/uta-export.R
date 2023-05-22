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

        # Log-transform school distances, but only in aggregated results;
        # non-aggregated results are retained in original scale
        soc$school_dist [soc$school_dist < 1] <- 1 # 1 metre!
        soc$school_dist <- log10 (soc$school_dist)

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

    if ("soc_var" %in% names (soc)) {
        names (soc) [which (names (soc) == "soc_var")] <- "social_index"
    }

    return (soc)
}
