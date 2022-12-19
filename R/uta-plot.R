
#' Generate an interactive plot of results from 'uta_interpolate'
#'
#' @param graph A street network with values of UTA index calculated from
#' \link{uta_index} and interpolated back onto network with
#' \link{uta_interpolate}.
#' @param var Which variable to plot.
#' @return Nothing; function called for its side-effect of opening an
#' interactive visualisation in local default browser.
#' @export

uta_plot <- function (graph, var = "uta_index_d10") {

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
