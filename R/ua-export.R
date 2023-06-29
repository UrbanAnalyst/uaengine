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
#' @inheritParams ua_index_batch
#' @param soc An \pkg{sf} `data.frame` object with polygons defining areas in
#' which socio-demographic variables were collated, and a column called
#' "social_index" containing the socio-demographic variable of interest. If not
#' provided, results are not aggregated and simply returned for each original
#' point.
#' @param dlim Return results for specified value(s) of dlim only. If only one
#' value specified, names of results will not be labelled with actual value.
#' @param pairwise If `TRUE`, results are aggregated for each pair of variables
#' instead of individually.
#' @return The input table, `soc`, with additional columns of UA index values
#' attached. This table can then be saved and used directly in the UA
#' front-end.
#' @export

ua_export <- function (city,
                       results_path,
                       soc = NULL,
                       dlim = 10,
                       pairwise = FALSE) {

    if (!is.null (soc)) {
        if (!"social_index" %in% names (soc)) {
            stop ("'soc' must include a 'social_index' column", call. = FALSE)
        }
    }

    res <- batch_collate_results (results_path, city)
    # Transform popdens at the start to yield sensible scales for pairwise
    # combinations of variables:
    res$popdens <- res$popdens / 1000
    # And transform bike_index, natural by inverting
    res$bike_index <- 1 - res$bike_index
    res$natural <- 1 - res$natural

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
    vars <- c (unique (unlist (vars)), "transport")

    # Add compound "transport" variable:
    suppressWarnings (
        res$transport <- res$times_abs * res$transfers * log10 (res$intervals)
    )

    # And re-order columns:
    index <- match (c ("osm_id", vars), names (res))
    index2 <- seq_len (ncol (res)) [-index]
    res <- res [, c (index, index2)]

    index <- which (!names (res) %in% c (vars, "osm_id", "x", "y", "soc_var"))
    extra_vars <- names (res) [index]
    # Generally "popdens", "school_dist", "bike_index", "natural"

    log_vars <- c ("parking", "school_dist", "intervals")

    if (pairwise) {

        index <- match (c (vars, extra_vars, "soc_var"), names (res))
        res0 <- res [, -index]
        res_v <- res [, index]
        # scale parking to avoid negative log-values:
        res_v$parking <- res_v$parking * 10000
        for (v in log_vars) {
            res_v [[v]] <- log10 (res_v [[v]])
            res_v [[v]] [!is.finite (res_v [[v]])] <- NA
        }
        var_pairs <- t (utils::combn (names (res_v), 2))
        res <- data.frame (apply (var_pairs, 1, function (i) {
            res_v [[i [1]]] * res_v [[i [2]]]
        }))
        vars <- apply (gsub ("\\_", "", var_pairs), 1, paste, collapse = "_")
        vars <- gsub ("bikeindex", "bike", vars)
        vars <- gsub ("socvar", "social", vars)
        names (res) <- vars
        extra_vars <- NULL

        res <- cbind (res0, res)
    }

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

                if (!pairwise &&
                    v %in% c ("parking", "school_dist", "intervals")) {

                    p <- res [[v]] [index]
                    p <- p [which (p > 0 & !is.nan (p))]
                    soc [[v]] [i] <- 10^mean (log10 (p), na.rm = TRUE)

                } else {

                    soc [[v]] [i] <- mean (res [[v]] [index], na.rm = TRUE)
                    soc [[v]] [is.nan (soc [[v]])] <- NA
                }
            }
        }

        soc <- soc [which (!is.na (soc$social_index)), ]

        # Log-transform these variables only in aggregated results;
        # non-aggregated results are retained in original scale
        if (!pairwise) {
            lt_vars <- c ("school_dist", "intervals")
            soc$school_dist [soc$school_dist < 1] <- 1 # 1 metre!
            for (lt in lt_vars) {
                soc [[lt]] <- log10 (soc [[lt]])
            }
        }

    } else {
        soc <- res
        extra_vars <- c (extra_vars, c ("osm_id", "x", "y"))
    }

    vars2keep <- c (vars, extra_vars)
    if (!pairwise) {
        vars2keep <- c (vars2keep, "social_index", "soc_var")
    }
    vars2keep <- c (vars2keep, grep ("^geom", names (soc), value = TRUE))
    soc <- soc [, which (names (soc) %in% vars2keep)]

    if ("soc_var" %in% names (soc)) {
        names (soc) [which (names (soc) == "soc_var")] <- "social_index"
    }

    return (soc)
}
