
#' Extract vertices within perimiter of external data set
#'
#' @param x External data set, presumed to represent some socio-demographic
#' variable, containing values of defined variable and \pkg{sf}-format polygons
#' defining each observation of that variable.
#' @param city Name of city for which vertices are extracted; passed to
#' \pkg{m4ra} functions which store pre-processed networks in locations defined
#' by name of 'city'
#' @param mode Mode of transport used to generate network; this should be the
#' same as the 'initial_mode' parameter passed to \pkg{m4ra} functions.
#'
#' @return A vertex table of only those vertices within the bounding polygon
#' defined by `x`.
#' @export
uta_vertices <- function (x, city, mode) {

    graph_c <- m4ra::m4ra_load_cached_network (
        city = city,
        mode = mode,
        contracted = TRUE
    )
    v <- m4ra::m4ra_vertices (graph_c, city)

    vsf <- sfheaders::sf_point (v [, c ("x", "y")])
    sf::st_crs (vsf) <- 4326

    perimeter <- sf::st_union (x)
    index <- sf::st_within (vsf, perimeter)
    index <- vapply (index, function (i) length (i) > 0, logical (1L))

    return (v [which (index), ])
}
