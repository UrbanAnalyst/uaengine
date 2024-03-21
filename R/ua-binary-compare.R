#' Statistically compare UA values between two spatial groups
#'
#' Spatial groups must be distinguished by a named variable in the "soc"
#' data.table.
#'
#' @inheritParams ua_index_batch
#' @param bin_var The same of the variable in "soc" used to distinguish binary
#' groups. This must be a logical variable (values as `TRUE` or `FALSE` only).
#' @param d One value of 'dlim' parameters used in \link{ua_index} or
#' \link{ua_index_batch} call to generate UA data. Resultant plot will be
#' based on data for this value.
#' @export

ua_binary_compare <- function (city, results_path, soc, bin_var, d = 10) {

    checkmate::assert_character (bin_var, len = 1L)
    checkmate::assert_logical (soc [[bin_var]])

    comp_vars <- c ("transport", "social", "ua_rel", "ua_abs")

    res <- batch_collate_results (results_path, city)

    var_abs <- sprintf ("times_limit_d%02d", d)
    var_rel <- sprintf ("int_d%2d_pop_adj", d)
    vars_ua <- paste0 (gsub ("ua_", "ua_index_", comp_vars))

    pt_index <- unlist (
        sf::st_within (reproj_sph_merc (res), reproj_sph_merc (soc))
    )

    soc$trans_rel <- soc$trans_abs <- NA

    for (i in unique (pt_index)) {
        index <- which (pt_index == i)
        soc$trans_abs [i] <- mean (res [[var_abs]] [index], na.rm = TRUE) / 60
        soc$trans_rel [i] <- mean (res [[var_rel]] [index], na.rm = TRUE)
    }
    soc <- soc [which (!is.na (soc$social_index)), ]

    # UA index is too heavily influenced by the greater scale (= SD) of the
    # social index. These lines re-scale so that transport has equal influence
    # on result as the social index.
    soc$ua_index_abs <- soc$ua_index_rel <- NA
    for (i in c ("trans_abs", "trans_rel")) {

        transport_sd <- stats::sd (soc [[i]], na.rm = TRUE)
        transport_mn <- mean (soc [[i]], na.rm = TRUE)
        social_index <- transport_mn +
            (soc$social_index - mean (soc$social_index, na.rm = TRUE)) *
                transport_sd
        nm <- gsub ("^trans", "ua_index", i)
        soc [[nm]] <- sqrt (soc [[i]] * social_index)
    }

    comp_vars <- c (
        "social_index", "trans_abs", "trans_rel",
        "ua_index_abs", "ua_index_rel"
    )

    for (v in comp_vars) {
        tt <- stats::t.test (
            soc [[v]] [which (soc [[bin_var]])],
            soc [[v]] [which (!soc [[bin_var]])]
        )
        delta <- as.numeric (diff (tt$estimate))
        cli::cli_alert_info (cli::col_cyan (
            v,
            paste0 (
                rep (" ", max (nchar (comp_vars)) - nchar (v) + 1),
                collapse = ""
            ),
            ": diff (",
            bin_var,
            ": TRUE -> FALSE) = ",
            ifelse (delta < 0, "", " "),
            sprintf ("%.4f", delta),
            "; p = ",
            sprintf ("%.4f", tt$p.value)
        ))
    }
}
