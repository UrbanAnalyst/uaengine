
#' Extract network data with osmium, convert to \pkg{sc} format, and collate all
#' results into a single \pkg{osmdata} object.
#'
#' @param City Name of city (used to name resultant files).
#' @param path_to_bz2 Path to a local `.bz2` file with OpenStreetMap data for
#' nominated city.
#' @param bbox Optional bounding box within which data are to be extracted. If
#' not given, result includes the entire network within the nominated '.bz2'
#' file. `bbox` can be either a matrix obtained from the \pkg{osmdata} function,
#' `getbb` (or equivalent), or an object from which a bounding box can be
#' extracted. Objects currently recognised are matrices or arrays, which should
#' have two columns (x and y / longitude and latitude), or an \pkg{sf} object
#' from which a bounding box can be extracted.
#' @param bbox_expand A proportional amount by which to extend the limits of the
#' bounding box defined by the `bbox` argument, defaulting to 5%.
#' @export

uta_extract_osm <- function (city, path_to_bz2, bbox = NULL, bbox_expand = 0.05) {

    requireNamespace ("fs")
    requireNamespace ("withr")

    checkmate::assert_character (city, min.len = 1L, max.len = 1L)
    checkmate::assert_file_exists (path_to_bz2)
    if (fs::path_ext (path_to_bz2) != "bz2") {
        stop ("path_to_bz2 must be to a '.bz2' file")
    }
    checkmate::assert_numeric (
        bbox_expand,
        lower = -1,
        upper = 1,
        min.len = 1L,
        max.len = 1L
    )

    if (!is.null (bbox)) {
        trim_bz2_to_bbox (city, path_to_bz2, bbox)
    }
}

get_uta_bbox <- function (bbox = NULL, bbox_expand = 0.05) {

    if (inherits (bbox, "sf")) {
        bbox <- sf::st_transform (bbox, 4326)
        xy <- sf::st_coordinates (bbox) [, 1:2]
    } else if (ncol (bbox) != 2L) {
        stop ("'bbox' must have 2 columns only.", call. = FALSE)
    } else if (!inherits (bbox, "matrix")) {
        stop ("bbox must be a matrix-like object.", call. = FALSE)
    } else if (nrow (bbox) < 2L) {
        stop ("bbox must have at least 2 rows.", call. = FALSE)
    }

    if (nrow (bbox) == 2L) {
        return (bbox)
    }

    res <- t (apply (xy, 2, range))
    rownames (res) <- c ("x", "y")
    colnames (res) <- c ("min", "max")

    # expand:
    for (i in 1:2) {
        res [i, ] <- mean (range (res [i, ])) +
            c (-0.5, 0.5) * diff (range (res [i, ])) * (1 + bbox_expand)
    }

    return (res)
}

trim_bz2_to_bbox <- function (city, path, bbox) {

    cli::cli_h3 (cli::col_green ("Reducing '.bz2' to specified 'bbox':"))

    bbox <- get_uta_bbox (bbox)
    bz_dir <- fs::path_dir (path)
    f <- paste0 (city, ".osm.pbf")
    if (fs::file_exists (fs::path (bz_dir, f))) {
        cli::cli_alert_info (cli::col_blue (
            "File '",
            fs::path (bz_dir, f),
            " already exists and will not be over-written."
        ))
        return ()
    }
    f0 <- fs::path_file (path)
    cmd <- paste ("osmium extract -b", paste0 (bbox, collapse = ","), f0, "-o", f)
    withr::with_dir (bz_dir, system (cmd))
}