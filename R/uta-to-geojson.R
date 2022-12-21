
#' Write UTA index data to local 'geojson' file
#'
#' Writing large graphs to 'geojson' output can take some time - typically on
#' the order of minutes per 100,000 graph edges.
#'
#' @param uta_dat Full network graph including UTA data returned from
#' \link{uta_index} and {uta_interpolate} functions.
#' @param ndigits Number of digits for rounding UTA data in 'geojson'
#' representation.
#' @param filename Name and/or full path to local file where 'geojson' data are
#' to be stored.
#' @export

uta_to_geojson <- function (uta_dat, ndigits = 2L, filename = NULL) {

    checkmate::assert_character (filename, min.len = 1L, max.len = 1L)
    if (!fs::dir_exists (fs::path_dir (filename))) {
        stop ("Directory does not exist")
    }

    requireNamespace ("geojsonio")

    x <- as.matrix (uta_dat [, c (".vx0_x", ".vx1_x")])
    x <- as.vector (t (x))
    y <- as.matrix (uta_dat [, c (".vx0_y", ".vx1_y")])
    y <- as.vector (t (y))

    xy <- data.frame (
        x = x,
        y = y,
        linestring_id = rep (seq_len (nrow (uta_dat)), each = 2)
    )
    xy <- sfheaders::sf_line (xy)

    cols <- grep ("\\d[0-9]+(\\_|$)", names (uta_dat), value = TRUE)
    for (ci in cols) {
        xy [[ci]] <- round (uta_dat [[ci]], digits = ndigits)
    }

    names (xy) <- gsub ("\\_pop\\_adj", "", names (xy))
    names (xy) <- gsub ("^int\\_", "transport_", names (xy))


    geojsonio::geojson_write (xy, file = filename)
}
