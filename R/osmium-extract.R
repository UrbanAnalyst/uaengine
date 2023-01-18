
#' Extract network data with osmium, convert to \pkg{sc} format, and collate all
#' results into a single \pkg{osmdata} object.
#'
#' @param city Name of city (used to name resultant files).
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
        path_to_pbf <- trim_bz2_to_bbox (city, path_to_bz2, bbox, bbox_expand)
    } else {
        path_to_pbf <- convert_bz2_to_pbf (city, path_to_bz2)
    }

    extract_osm_keys (path_to_pbf)
}

#' Trim '.bz2' to 'bbox' and return converted 'pbf' output.
#'
#' @param city Directly from 'uta_extract_osm'
#' @param path The 'path_to_bz2' param from 'uta_extract_osm'
#' @param bbox Directly from 'uta_extract_osm'
#' @param bbox_expand Directly from 'uta_extract_osm'
#' @return Full path and file name of newly created '.pbf' file.
#' @noRd
trim_bz2_to_bbox <- function (city, path, bbox, bbox_expand) {

    bbox <- get_uta_bbox (bbox, bbox_expand = bbox_expand)
    pars <- get_osmium_convert_args (city, path)
    if (pars$f_exists) {
        return (fs::path (pars$bz_dir, pars$f))
    }

    cli::cli_h3 (cli::col_green ("Reducing '.bz2' to specified 'bbox':"))

    cmd <- paste ("osmium extract -b", paste0 (bbox, collapse = ","), pars$f0, "-o", pars$f)
    withr::with_dir (pars$bz_dir, system (cmd))

    return (fs::path (pars$bz_dir, pars$f))
}

#' Convert various form of 'bbox' parameter to an actual 2-by-2 matrix.
#'
#' @param bbox Directly from 'uta_extract_osm'
#' @param bbox_expand Directly from 'uta_extract_osm'
#' @return bbox as 2-by-2 matrix.
#' @noRd
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

#' Convert '.bz2' directly to 'pbf' without trimming.
#'
#' This function is called when no `bbox` parameter is passed to main
#' 'uta_extract_osm()` function.
#'
#' @param city Directly from 'uta_extract_osm'
#' @param path The 'path_to_bz2' param from 'uta_extract_osm'
#' @return Full path and file name of newly created '.pbf' file.
#' @noRd
convert_bz2_to_pbf <- function (city, path) {

    pars <- get_osmium_convert_args (city, path)
    if (!pars$f_exists) {
        cmd <- paste ("osmium cat ", pars$f0, "-o", pars$f)
        withr::with_dir (pars$bz_dir, system (cmd))
    }

    return (fs::path (pars$bz_dir, pars$f))
}

#' Helper function to return arguments used for 'osmium' calls, mostly by
#' removing paths.
#' @noRd
get_osmium_convert_args <- function (city, path) {

    bz_dir <- fs::path_dir (path)
    f <- paste0 (city, ".osm.pbf")
    f_exists <- FALSE
    if (fs::file_exists (fs::path (bz_dir, f))) {
        cli::cli_alert_info (cli::col_blue (
            "File '",
            fs::path (bz_dir, f),
            " already exists and will not be over-written."
        ))
        f_exists <- TRUE
    }
    f0 <- fs::path_file (path)

    list (bz_dir = bz_dir, f = f, f0 = f0, f_exists = f_exists)
}

#' Extract smaller files for a series of OSM keys and convert to '.osm' format
#'
#' @param path Path to single '.pbf' file returned from either
#' 'convert_bz2_to_pbf' or 'trim_bz2_to_bbox'.
#' @return Nothing.
#' @noRd
extract_osm_keys <- function (path) {

    path_dir <- fs::path_dir (path)
    f <- fs::path_file (path)

    cli::cli_h3 (cli::col_green ("Extracting OSM tags:"))

    tags <- c (
        "highway", "restriction", "access", "bicycle", "foot",
        "motorcar", "motor_vehicle", "vehicle", "toll"
    )

    for (tg in tags) {

        ft <- paste0 (gsub ("\\.osm\\.pbf$", "", f), "-", tg, ".osm")
        ft_full <- fs::path (path_dir, ft)
        if (fs::file_exists (ft_full)) {
            cli::cli_alert_info (cli::col_blue (
                "File '",
                ft_full,
                "' already exists."
            ))
            next
        }

        cli::cli_h1 (tg)
        cmd <- paste ("osmium tags-filter", f, paste0 ("w/", tg), "-o", ft)
        withr::with_dir (path_dir, system (cmd))
    }
}
