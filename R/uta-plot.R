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
    cols <- grDevices::rgb (colourvalues::get_palette ("matlab_like2"), maxColorValue = 255)
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
#' @param what The value which is to be plotted.
#' @param zoom Initial zoom level to use in resultant map.
#' @return Nothing; function called for its side-effect of opening an
#' interactive visualisation in local default browser.
#' @export

uta_plot_polygons <- function (city, results_path, soc, d = 10,
                               what = c ("transport", "uta", "social"),
                               zoom = 10) {

    requireNamespace ("mapdeck")
    requireNamespace ("colourvalues")

    what <- match.arg (what, c ("transport", "uta", "social"))

    res <- batch_collate_results (results_path, city)

    var_transport <- sprintf ("int_d%2d_pop_adj", d)
    var_uta <- sprintf ("uta_index_d%2d", d)

    soc$transport <- soc$uta_index <- NA

    pt_index <- unlist (sf::st_within (res, soc))

    for (i in unique (pt_index)) {
        index <- which (pt_index == i)
        soc$transport [i] <- mean (res [[var_transport]] [index], na.rm = TRUE)
        soc$uta_index [i] <- mean (res [[var_uta]] [index], na.rm = TRUE)
    }
    soc <- soc [which (!is.na (soc$social_index)), ]

    col_var <- ifelse (what == "transport", what, paste0 (what, "_index"))

    xy0 <- sf::st_coordinates (sf::st_centroid (sf::st_union (soc)))
    tmp <- uta_plot_legend (soc, col_var)

    m <- mapdeck::mapdeck (location = xy0, zoom = zoom)
    m <- mapdeck::add_polygon (
        m,
        tmp$soc,
        fill_colour = "colour",
        fill_opacity = 0.7,
        legend = tmp$legend,
        update_view = FALSE
    )
    print (m)
}

uta_plot_legend <- function (soc, col_var, ncols = 100L, nvals = 5L) {

    soc <- soc [which (!is.na (soc [[col_var]])), ]

    pal <- colourvalues::colour_values (seq_len (ncols), palette = "inferno")
    pal <- rev (pal)
    ix <- cut (soc [[col_var]], breaks = ncols)
    index <- match (ix, levels (ix))
    soc$colour <- pal [index]

    brks <- levels (unique (cut (soc [[col_var]], breaks = nvals)))
    lwr <- as.numeric (sub ("\\((.+),.*", "\\1", brks))
    upr <- as.numeric (sub ("[^,]*,([^]]*)\\]", "\\1", brks))
    brks <- pretty (c (lwr [1], upr))

    # Need to be rounded to avoid mapdeck printing trailing zeros:
    brk_diff <- median (diff (brks))
    ndigits <- 0L
    tol <- 1e-3
    while (abs (brk_diff - round (brk_diff, digits = ndigits)) > tol) {
        ndigits <- ndigits + 1L
    }
    brks <- round (brks, digits = ndigits)

    colvals <- colourvalues::colour_values (seq_len (length (brks)), palette = "inferno")
    l1 <- mapdeck::legend_element (
        variables = brks,
        colours = colvals,
        colour_type = "fill",
        variable_type = "gradient",
        title = col_var
    )
    js <- mapdeck::mapdeck_legend (l1)

    return (list (soc = soc, legend = js))
}
