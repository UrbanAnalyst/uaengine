#' Write UA index data to local 'geojson' file
#'
#' Writing large graphs to 'geojson' output can take some time - typically on
#' the order of minutes per 100,000 graph edges.
#'
#' @param ua_dat Full network graph including UA data returned from
#' \link{ua_index} and {ua_interpolate} functions.
#' @param ndigits Number of digits for rounding UA data in 'geojson'
#' representation.
#' @param filename Name and/or full path to local file where 'geojson' data are
#' to be stored.
#' @export

ua_to_geojson <- function (ua_dat, ndigits = 2L, filename = NULL) {

    checkmate::assert_character (filename, min.len = 1L, max.len = 1L)
    if (!fs::dir_exists (fs::path_dir (filename))) {
        stop ("Directory does not exist")
    }

    requireNamespace ("geojsonio")

    x <- as.matrix (ua_dat [, c (".vx0_x", ".vx1_x")])
    x <- as.vector (t (x))
    y <- as.matrix (ua_dat [, c (".vx0_y", ".vx1_y")])
    y <- as.vector (t (y))

    xy <- data.frame (
        x = x,
        y = y,
        linestring_id = rep (seq_len (nrow (ua_dat)), each = 2)
    )
    xy <- sfheaders::sf_line (xy)

    cols <- grep ("\\d[0-9]+(\\_|$)", names (ua_dat), value = TRUE)
    for (ci in cols) {
        xy [[ci]] <- round (ua_dat [[ci]], digits = ndigits)
    }

    names (xy) <- gsub ("\\_pop\\_adj", "", names (xy))
    names (xy) <- gsub ("^int\\_", "transport_", names (xy))

    # Reduce down to maximal distance values only, if multiple distances used in
    # 'ua_dat'
    reduce_to_max <- function (xy, what = "transport") {
        index <- grep (paste0 ("^", what, "\\_"), names (xy))
        if (length (index) > 1) {
            xy <- xy [, -index [-length (index)]]
        }
        names (xy) [grep (paste0 ("^", what, "\\_"), names (xy))] <- what
        return (xy)
    }
    xy <- reduce_to_max (xy, "transport")
    xy <- reduce_to_max (xy, "ua_index")

    xy <- map_highway_types (xy, ua_dat)

    geojsonio::geojson_write (xy, file = filename)
}

map_highway_types <- function (xy, ua_dat) {

    hw_types <- list (
        local = c (
            "bridleway",
            "cycleway",
            "footway",
            "living_street",
            "path",
            "pedestrian",
            "steps",
            "track"
        ),
        tertiary = c (
            "residential",
            "tertiary",
            "tertiary_link",
            "unclassified"
        ),
        secondary = c (
            "secondary",
            "secondary_link"
        ),
        primary = c (
            "primary",
            "primary_link",
            "service",
            "trunk",
            "trunk_link"
        )
    )
    hw_types <- lapply (seq_along (hw_types), function (i) {
        data.frame (type = hw_types [[i]], hw_type = rep (names (hw_types) [i], length (hw_types [[i]])))
    })
    hw_types <- do.call (rbind, hw_types)

    xy <- xy [which (ua_dat$highway %in% hw_types$type), ]
    hw_type <- hw_types$hw_type [match (ua_dat$highway, hw_types$type)]

    zoom_levels <- data.frame (
        hw = c ("local", "tertiary", "secondary", "primary"),
        zoom = c (13L, 13L, 12L, 11L)
    )
    xy$zoom_level <- zoom_levels$zoom [match (hw_type, zoom_levels$hw)]

    return (xy)
}
