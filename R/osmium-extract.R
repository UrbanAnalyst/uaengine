#' Extract network data with osmium, convert to \pkg{sc} format, and collate all
#' results into a single \pkg{osmdata} object.
#'
#' @param city Name of city (used to name resultant files).
#' @param path Path to a local '.pbf' or '.bz2' file with OpenStreetMap data for
#' nominated city.
#' @param bbox Optional bounding box within which data are to be extracted. If
#' not given, result includes the entire network within the nominated OSM
#' file. `bbox` can be either a matrix obtained from the \pkg{osmdata} function,
#' `getbb` (or equivalent), or an object from which a bounding box can be
#' extracted. Objects currently recognised are matrices or arrays, which should
#' have two columns (x and y / longitude and latitude), or an \pkg{sf} object
#' from which a bounding box can be extracted. Alternatively, `bbox` can be a
#' local path to a 'geojson' file containing a single polygonal outline to be
#' used to trim the OSM data.
#' @param bbox_expand A proportional amount by which to extend the limits of the
#' bounding box defined by the `bbox` argument, defaulting to 5%.
#' @param osm_id In lieu of a bounding box, the ID of an Open Street Map object
#' (generally a relation) can be used to provide the boundary to trim the OSM
#' file.
#' @export

ua_extract_osm <- function (city, path, bbox = NULL, bbox_expand = 0.05,
                            osm_id = NULL) {

    requireNamespace ("fs")
    requireNamespace ("withr")

    checkmate::assert_character (city, min.len = 1L, max.len = 1L)
    city <- tolower (city)
    checkmate::assert_file_exists (path)
    if (!fs::path_ext (path) %in% c ("pbf", "bz2")) {
        stop ("'path' must be to a '.pbf' or '.bz2' file")
    }
    checkmate::assert_numeric (
        bbox_expand,
        lower = -1,
        upper = 1,
        min.len = 1L,
        max.len = 1L
    )

    if (!is.null (osm_id)) {
        path_to_pbf <- trim_to_osm_id (city, path, osm_id)
    } else if (!is.null (bbox)) {
        path_to_pbf <- trim_osm_to_bbox (city, path, bbox, bbox_expand)
    } else {
        path_to_pbf <- convert_bz2_to_pbf (city, path)
    }

    extract_osm_keys (path_to_pbf)
    ua_m4ra_parking_extraction (path_to_pbf)
    ua_osm_schools (path_to_pbf)
    ua_osm_nature (path_to_pbf)
}

#' Trim OSM file to an 'osm_id' and return converted 'pbf' output.
#'
#' @param city Directly from 'ua_extract_osm'
#' @param path The 'path' param from 'ua_extract_osm'
#' @param osm_id Directly from 'ua_extract_osm'
#' @return Full path and file name of newly created '.pbf' file.
#' @noRd
trim_to_osm_id <- function (city, path, osm_id) {

    pars <- get_osmium_convert_args (city, path)
    if (pars$f_exists) {
        return (fs::path (pars$osm_dir, pars$f))
    }

    cli::cli_h3 (cli::col_green ("Reducing file to specified 'bbox':"))

    # Get .osm file for specified 'osm_id':
    ftmp <- fs::path (fs::path_temp (), "boundary.osm")
    cmd <- paste ("osmium getid -r -t ", path, paste0 ("r", osm_id), "-o", ftmp)
    system (cmd)
    cmd <- paste ("osmium extract -p", ftmp, pars$f0, "-o", pars$f)
    withr::with_dir (pars$osm_dir, system (cmd))

    return (fs::path (pars$osm_dir, pars$f))
}


#' Trim OSM file to 'bbox' and return converted 'pbf' output.
#'
#' @param city Directly from 'ua_extract_osm'
#' @param path The 'path' param from 'ua_extract_osm'
#' @param bbox Directly from 'ua_extract_osm'
#' @param bbox_expand Directly from 'ua_extract_osm'
#' @return Full path and file name of newly created '.pbf' file.
#' @noRd
trim_osm_to_bbox <- function (city, path, bbox, bbox_expand) {

    bbox <- get_ua_bbox (bbox, bbox_expand = bbox_expand)
    pars <- get_osmium_convert_args (city, path)
    if (pars$f_exists) {
        return (fs::path (pars$osm_dir, pars$f))
    }

    cli::cli_h3 (cli::col_green ("Reducing file to specified 'bbox':"))

    pb_arg <- ifelse (methods::is (bbox, "character"), "-p", "-b")

    cmd <- paste (
        "osmium extract",
        pb_arg,
        paste0 (bbox, collapse = ","),
        pars$f0,
        "-o",
        pars$f
    )
    withr::with_dir (pars$osm_dir, system (cmd))

    # If osm_dir is country, create city-specific sub-dir, and move result
    # there:
    city_dir <- pars$osm_dir
    if (fs::path_file (city_dir) != city) {
        city_dir <- fs::path (city_dir, city)
        if (!fs::dir_exists (city_dir)) {
            fs::dir_create (city_dir, recurse = TRUE)
        }

        fs::file_move (
            fs::path (pars$osm_dir, pars$f),
            fs::path (city_dir, pars$f)
        )
    }

    return (fs::path (city_dir, pars$f))
}

#' Convert various form of 'bbox' parameter to an actual 2-by-2 matrix.
#'
#' @param bbox Directly from 'ua_extract_osm'
#' @param bbox_expand Directly from 'ua_extract_osm'
#' @return bbox as 2-by-2 matrix.
#' @noRd
get_ua_bbox <- function (bbox = NULL, bbox_expand = 0.05) {

    if (methods::is (bbox, "character")) {
        checkmate::assert_character (bbox, len = 1L)
        checkmate::assert_file_exists (bbox)
        return (bbox)
    }

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
#' 'ua_extract_osm()` function.
#'
#' @param city Directly from 'ua_extract_osm'
#' @param path The 'path' param from 'ua_extract_osm'
#' @return Full path and file name of newly created '.pbf' file.
#' @noRd
convert_bz2_to_pbf <- function (city, path) {

    pars <- get_osmium_convert_args (city, path)
    if (!pars$f_exists) {
        cmd <- paste ("osmium cat ", pars$f0, "-o", pars$f)
        withr::with_dir (pars$osm_dir, system (cmd))
    }

    return (fs::path (pars$osm_dir, pars$f))
}

#' Helper function to return arguments used for 'osmium' calls, mostly by
#' removing paths.
#' @noRd
get_osmium_convert_args <- function (city, path) {

    osm_dir <- fs::path_dir (path)
    f <- paste0 (city, ".osm.pbf")
    f_exists <- FALSE
    if (fs::file_exists (fs::path (osm_dir, f))) {
        cli::cli_alert_info (cli::col_blue (
            "File '",
            fs::path (osm_dir, f),
            " already exists and will not be over-written."
        ))
        f_exists <- TRUE
    }
    f0 <- fs::path_file (path)

    list (osm_dir = osm_dir, f = f, f0 = f0, f_exists = f_exists)
}

#' Extract smaller files for a series of OSM keys and convert to '.osm' format
#'
#' @param path Path to single '.pbf' file returned from either
#' 'convert_bz2_to_pbf' or 'trim_osm_to_bbox'.
#' @return Nothing.
#' @noRd
extract_osm_keys <- function (path) {

    path_dir <- fs::path_dir (path)
    f <- fs::path_file (path)

    ptn <- "\\.osm\\.(pbf|bz2)$"
    if (grepl ("\\-latest", f)) {
        ptn <- paste0 ("\\-latest", ptn)
    }
    ft <- paste0 (gsub (ptn, "", f), "-network.osm")
    ft_full <- fs::path (path_dir, ft)

    cli::cli_h3 (cli::col_green ("Extracting OSM tags:"))

    if (fs::file_exists (ft_full)) {

        cli::cli_alert_info (cli::col_blue (
            "File '",
            ft_full,
            "' already exists."
        ))

    } else {

        tags <- c (
            "highway", "restriction", "access", "foot", "motorcar",
            "motor_vehicle", "vehicle", "toll", "bicycle",
            "cycleway", "cycleway:left", "cycleway:right"
        )
        tags <- paste0 (paste0 ("wr/", tags), collapse = " ")

        cmd <- paste ("osmium tags-filter", f, tags, "-o", ft)
        withr::with_dir (path_dir, system (cmd))
    }
}

#' Modified version of \pkg{m4ra} functions to extract OSM data used for parking
#' analyses.
#'
#' This version uses 'osmium' instead of \pkg{osmdata} used there.
#'
#' @param path The `path_to_pbf` parameter generated in \link{ua_extract_osm}.
#' @return Path of a '.osm' file containing information needed for \pkg{m4ra}
#' parking analyses.
#' @noRd
ua_m4ra_parking_extraction <- function (path) {

    tags <- c (
        "nwr/parking",
        "nwr/amentiy=parking",
        "nwr/building=garage",
        "nwr/building=garages",
        "nwr/parking:lane:left",
        "nwr/parking:lane:right",
        "nwr/parking:lane:both",
        "nwr/amentiy=parking",
        "nwr/building=garage",
        "nwr/building=garages",
        "nwr/parking:lane:left",
        "nwr/parking:lane:right",
        "nwr/parking:lane:both"
    )

    path_dir <- fs::path_dir (path)
    f <- fs::path_file (path)

    cli::cli_h3 (cli::col_green ("Extracting parking data:"))

    ptn <- "\\.osm\\.(pbf|bz2)$"
    if (grepl ("\\-latest", f)) {
        ptn <- paste0 ("\\-latest", ptn)
    }
    ft <- paste0 (gsub (ptn, "", f), "-parking.osm")
    ft_full <- fs::path (path_dir, ft)
    if (fs::file_exists (ft_full)) {
        cli::cli_alert_info (cli::col_blue (
            "File '",
            ft_full,
            "' already exists."
        ))
    } else {
        cmd <- paste ("osmium tags-filter", f, paste0 (tags, collapse = " "), "-o", ft)
        withr::with_dir (path_dir, system (cmd))
    }

    return (ft_full)
}

#' Extract data on schools using osmium
#'
#' @param path The `path_to_pbf` parameter generated in \link{ua_extract_osm}.
#' @return Path of a '.osm' file containing information needed for \pkg{m4ra}
#' parking analyses.
#' @noRd
ua_osm_schools <- function (path) {

    tags <- c (
        "nwr/amenity=school",
        "nwr/school=elementary,primary,secondary"
    )

    path_dir <- fs::path_dir (path)
    f <- fs::path_file (path)

    cli::cli_h3 (cli::col_green ("Extracting school data:"))

    ptn <- "\\.osm\\.(pbf|bz2)$"
    if (grepl ("\\-latest", f)) {
        ptn <- paste0 ("\\-latest", ptn)
    }
    ft <- paste0 (gsub (ptn, "", f), "-schools.osm")
    ft_full <- fs::path (path_dir, ft)

    if (fs::file_exists (ft_full)) {

        cli::cli_alert_info (cli::col_blue (
            "File '",
            ft_full,
            "' already exists."
        ))

    } else {

        cmd <- paste ("osmium tags-filter", f, paste0 (tags, collapse = " "), "-o", ft)
        withr::with_dir (path_dir, system (cmd))
    }

    return (ft_full)
}

#' Extract data on natural spaces using osmium
#'
#' @param path The `path_to_pbf` parameter generated in \link{ua_extract_osm}.
#' @return Path of a '.osm' file containing information needed for \pkg{m4ra}
#' parking analyses.
#' @noRd
ua_osm_nature <- function (path) {

    tags <- c (
        "w/leisure=garden,park,nature_reserve,playground",
        "w/surface=grass",
        "w/landuse=forest,meadow,recreation_ground,village_green",
        "wr/natural"
    )

    path_dir <- fs::path_dir (path)
    f <- fs::path_file (path)

    cli::cli_h3 (cli::col_green ("Extracting natural spaces data:"))

    ptn <- "\\.osm\\.(pbf|bz2)$"
    if (grepl ("\\-latest", f)) {
        ptn <- paste0 ("\\-latest", ptn)
    }
    ft <- paste0 (gsub (ptn, "", f), "-natural.osm")
    ft_full <- fs::path (path_dir, ft)

    if (fs::file_exists (ft_full)) {

        cli::cli_alert_info (cli::col_blue (
            "File '",
            ft_full,
            "' already exists."
        ))

    } else {

        cmd <- paste ("osmium tags-filter", f, paste0 (tags, collapse = " "), "-o", ft)
        withr::with_dir (path_dir, system (cmd))
    }

    return (ft_full)
}
