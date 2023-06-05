#' Convert a series of '.osm' files to \pkg{silicate} format, and combine all
#' into a single object.
#'
#' This function saves a new '.Rds' file in the 'path' directory.
#'
#' @param path Path to the directory containing various '.osm' files produced by
#' \link{uta_extract_osm}.
#' @param city Name of city (used to name resultant files).
#' @return A single \pkg{silicate} 'SC' object containing combined data from all
#' individual '.osm' files. This file is also saved as a '.Rds' object in the
#' 'path' directory.
#'
#' @export

uta_osm_to_sc <- function (path, city) {

    requireNamespace ("fs")

    checkmate::assert_character (city, min.len = 1L, max.len = 1L)
    city <- tolower (city)

    f_sc <- fs::path (path, paste0 (city, "-sc.Rds"))
    if (fs::file_exists (f_sc)) {
        cli::cli_alert_info (cli::col_blue (
            "File '{f_sc}' already exists and will not be overwritten."
        ))
        return ()
    }

    q <- osmdata::opq (get_pbf_bbox (path, city))

    f <- fs::dir_ls (path, regexp = paste0 (city, ".*\\.osm$"))
    f <- f [which (!grepl ("\\-(parking|schools)\\.", f))]
    f <- grep ("network", f, value = TRUE)
    if (length (f) == 0L) {
        cli::cli_alert_warning (cli::col_red (
            "No '.osm' files found; did you run 'uta_extract_osm'?"
        ))
        return ()
    }
    if (length (f) > 1L) {
        cli::cli_alert_warning (cli::col_red (
            "Multiple network files found: {f}"
        ))
    }

    dat <- osmdata::osmdata_sc (q, doc = f [1])
    saveRDS (dat, f_sc)
    cli::cli_alert_success (cli::col_green (
        "All OSM data collated and written to '{f_sc}'"
    ))

    return (dat)
}

get_pbf_bbox <- function (path, city) {

    f <- fs::dir_ls (path, regexp = paste0 (city, ".*\\.osm\\.pbf$"))
    cmd <- paste0 ("osmium fileinfo -e ", f)
    out <- withr::with_dir (path, system (cmd, intern = TRUE))
    bb <- grep ("Bounding\\sbox\\:", out, value = TRUE)
    bb <- regmatches (bb, gregexpr ("[0-9]+\\.[0-9]+", bb)) [[1]]

    return (as.numeric (bb))
}
