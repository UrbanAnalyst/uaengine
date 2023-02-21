#' Generate an interactive plot of results from 'uta_interpolate' projected on
#' to street network.
#'
#' @param graph A street network with values of UTA index calculated from
#' \link{uta_index} and interpolated back onto network with
#' \link{uta_interpolate}.
#' @param var Which variable to plot.
#' @return Nothing; function called for its side-effect of opening an
#' interactive visualisation in local default browser.
#' @export

uta_plot_network <- function (graph, var = "uta_index_d10") {

    if (!var %in% names (graph)) {
        stop ("graph contains no variable named '", var, "'", call. = FALSE)
    }

    requireNamespace ("mapdeck")
    requireNamespace ("colourvalues")

    graph$width <- 10 * graph [[var]] / max (graph [[var]], na.rm = TRUE)

    ncols <- 256L # Has to be fixed for 'rgb' call
    vals <- sort (unique (round (graph [[var]])))
    ix <- cut (vals, breaks = ncols)
    cols <- grDevices::rgb (
        colourvalues::get_palette ("matlab_like2"),
        maxColorValue = 255
    )
    colvals <- cols [match (ix, levels (ix))]

    leg <- mapdeck::legend_element (
        variables = vals,
        colours = colvals,
        colour_type = "fill",
        variable_type = "gradient",
        title = var
    )

    mapdeck::mapdeck () |>
        mapdeck::add_line (
            data = graph,
            layer_id = "data",
            origin = c (".vx0_x", ".vx0_y"),
            destination = c (".vx1_x", ".vx1_y"),
            stroke_colour = var,
            stroke_width = "width",
            palette = "matlab_like2",
            legend = leg
        )
}


#' Generate an interactive plot of results in same polygons as original
#' socio-demographic data.
#'
#' @inheritParams uta_index_batch
#' @param d One value of 'dlim' parameters used in \link{uta_index} or
#' \link{uta_index_batch} call to generate UTA data. Resultant plot will be
#' based on data for this value.
#' @param what The value which is to be plotted, one of "transport", "social",
#' "uta_rel" or "uta_abs". The relative UTA index ("uta_rel") combines
#' socio-demographic variable, "soc", with the transport index relative to
#' equivalent automobile travel times. The absolute UTA index ("uta_abs")
#' combines uses as a transport index the absolute multi-modal travel times for
#' all modes excluding private automobile.
#' @param zoom Initial zoom level to use in resultant map.
#' @param alpha Transparency of polygons (0 = completely; 1 = not transparent).
#' @return Nothing; function called for its side-effect of opening an
#' interactive visualisation in local default browser.
#' @export

uta_plot_polygons <- function (city,
                               results_path, soc, d = 10,
                               what = c (
                                   "transport_rel",
                                   "transport_abs",
                                   "social",
                                   "uta_rel",
                                   "uta_abs"
                               ),
                               zoom = 10,
                               alpha = 0.5) {

    requireNamespace ("mapdeck")
    requireNamespace ("colourvalues")

    what <- match.arg (what)

    res <- batch_collate_results (results_path, city)

    is_abs <- grepl ("\\_abs$", what)
    var_transport <- ifelse (
        is_abs,
        sprintf ("times_limit_d%02d", d),
        sprintf ("int_d%2d_pop_adj", d)
    )
    var_uta <- sprintf (paste0 (gsub ("uta_", "uta_index_", what), "_d%02d"), d)

    soc$uta_index <- soc$transport <- NA

    pt_index <- unlist (sf::st_within (res, soc))

    for (i in unique (pt_index)) {
        index <- which (pt_index == i)
        soc$transport [i] <- mean (res [[var_transport]] [index], na.rm = TRUE)
        soc$uta_index [i] <- mean (res [[var_uta]] [index], na.rm = TRUE)
    }
    soc <- soc [which (!is.na (soc$social_index)), ]

    if (is_abs) {
        # convert scales from seconds to minutes:
        soc$transport <- soc$transport / 60
        soc$uta_index <- soc$uta_index / 60
    }

    # UTA index is too heavily influenced by the greater scale (= SD) of the
    # social index. These lines re-scale so that transport has equal influence
    # on result as the social index.
    if (grepl ("^uta", what)) {
        transport_sd <- stats::sd (soc$transport, na.rm = TRUE)
        transport_mn <- mean (soc$transport, na.rm = TRUE)
        social_index <- transport_mn +
            (soc$social_index - mean (soc$social_index, na.rm = TRUE)) *
                transport_sd
        soc$uta_index <- sqrt (soc$transport * social_index)
    }

    # Replace "uta_rel" or "uta_abs" with "uta_index":
    col_var <- ifelse (
        grepl ("^transport", what),
        "transport",
        paste0 (gsub ("\\_.*$", "", what), "_index")
    )

    xy0 <- sf::st_coordinates (sf::st_centroid (sf::st_union (soc)))
    tmp <- uta_plot_legend (
        soc,
        col_var,
        alpha = alpha,
        rel = !is_abs
    )

    m <- mapdeck::mapdeck (location = xy0, zoom = zoom)
    m <- mapdeck::add_polygon (
        m,
        tmp$soc,
        fill_colour = "colour",
        fill_opacity = 0.4,
        legend = tmp$legend,
        update_view = FALSE
    )
    print (m)
}

uta_plot_legend <- function (soc, col_var,
                             alpha = 0.5, ncols = 100L, nvals = 5L,
                             rel = TRUE) {

    soc <- soc [which (!is.na (soc [[col_var]])), ]

    alpha <- ceiling (255 * alpha)
    pal <- rev (colourvalues::colour_values (
        seq_len (ncols),
        palette = "inferno",
        alpha = alpha
    ))
    ix <- fit_palette_to_var (soc [[col_var]], ncols)
    # ix <- cut (x, breaks = ncols)
    index <- match (ix, levels (ix))
    soc$colour <- pal [index]

    brks <- levels (unique (cut (soc [[col_var]], breaks = nvals)))
    # regex patterns from ?cut
    lwr <- as.numeric (sub ("\\((.+),.*", "\\1", brks))
    upr <- as.numeric (sub ("[^,]*,([^]]*)\\]", "\\1", brks))
    brks <- pretty (c (lwr [1], upr))

    # Need to be rounded to avoid mapdeck printing trailing zeros:
    brk_diff <- stats::median (diff (brks))
    ndigits <- 0L
    tol <- 1e-3
    while (abs (brk_diff - round (brk_diff, digits = ndigits)) > tol) {
        ndigits <- ndigits + 1L
    }
    brks <- round (brks, digits = ndigits)

    x <- seq_len (length (brks))
    colvals <- rev (colourvalues::colour_values (
        x,
        palette = "inferno",
        alpha = alpha
    ))

    leg_title <- col_var
    if (grepl ("^(uta|transport)", leg_title)) {
        leg_title <- paste0 (
            leg_title,
            ifelse (rel, " relative", " absolute")
        )
    }

    l1 <- mapdeck::legend_element (
        variables = brks,
        colours = colvals,
        colour_type = "fill",
        variable_type = "gradient",
        title = leg_title
    )
    js <- mapdeck::mapdeck_legend (l1)

    return (list (soc = soc, legend = js))
}

#' Concatenate the distribution of `x` to prevent palettes being dominated by
#' extreme values.
#'
#' This works by tabulating `x` into `ncols` bins. Upper and lower tails with
#' only one entry are then progressively decreased/increased to fit within the
#' next bin, until all bins have at least 2 values. Additional code also
#' squashes distributional tails which produce zero values in the tables.
#'
#' @return A tabularized version of `x` which should much more regularly span a
#' desired palette.
#' @noRd
fit_palette_to_var <- function (x, ncols = 100L) {

    x <- trim_var_ranges (x, ncols = 100)

    return (cut (x, breaks = ncols))
}

# Adjust values until each group has multiple members:
trim_var_ranges <- function (x, ncols = 100L) {

    ix <- cut (x, breaks = ncols)

    tb <- table (ix)
    tb_i <- as.integer (tb)
    # Are there zeros in lower or upper halves of table?
    lower_zeros <- any (tb_i [seq_along (tb_i) < which.max (tb_i)] == 0L)
    upper_zeros <- any (tb_i [seq_along (tb_i) > which.max (tb_i)] == 0L)

    count <- 0
    while (tb_i [1] <= 1 || tb_i [length (tb_i)] <= 1 ||
        lower_zeros || upper_zeros) {

        if (which (tb_i > 1) [1] > 1 || lower_zeros) {
            # increase lower values to next interval
            lwr <- as.numeric (sub ("\\((.+),.*", "\\1", names (tb) [2]))
            x [x < lwr] <- lwr
        }

        # same for upper part of distribution
        if (tb_i [length (tb_i)] <= 1 || upper_zeros) {
            upr <- sub ("[^,]*,([^]]*)\\]", "\\1", names (tb) [length (tb) - 1])
            x [x > upr] <- as.numeric (upr)
        }

        ix <- cut (x, breaks = ncols)
        tb <- table (ix)
        tb_i <- as.integer (tb)

        lower_zeros <- any (tb_i [seq_along (tb_i) < which.max (tb_i)] == 0L)
        upper_zeros <- any (tb_i [seq_along (tb_i) > which.max (tb_i)] == 0L)

        count <- count + 1
        if (count > 100) {
            break
        }
    }

    return (x)
}
