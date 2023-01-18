
#' Convert a series of '.osm' files to \pkg{silicate} format, and combine all
#' into a single object.
#'
#' This function saves a new '.Rds' file in the 'path' directory.
#'
#' @param path Path to the directory containing various '.osm' files produced by
#' \link{uta_extract_osm}.
#' @param city Name of city (used to name resultant files).
#' @param remove_osm_files If `TRUE` (default), remove the individual '.osm'
#' files for each key after conversion to \pkg{silicate} format.
#' @return A single \pkg{silicate} 'SC' object containing combined data from all
#' individual '.osm' files. This file is also saved as a '.Rds' object in the
#' 'path' directory.
#'
#' @export

uta_osm_to_sc <- function (path, city, remove_osm_files = TRUE) {

    requireNamespace ("fs")
    requireNamespace ("fs")

    checkmate::assert_character (city, min.len = 1L, max.len = 1L)
    city <- tolower (city)

    f_sc <- fs::path (path, paste0 (city, "-sc.Rds"))
    if (fs::file_exists (f_sc)) {
        cli::cli_alert_info (cli::col_blue (
            "File '",
            f_sc,
            "' already exists and will not be overwritten."
        ))
        return ()
    }

    q <- osmdata::opq (get_pbf_bbox (path, city))

    flist <- fs::dir_ls (path, regexp = paste0 (city, ".*\\.osm$"))
    flist <- flist [which (!grepl ("\\-parking\\.", flist))]
    if (length (flist) == 0L) {
        cli::cli_alert_warning (cli::col_red (
            "No '.osm' files found; did you run 'uta_extract_osm'?"
        ))
        return ()
    }

    dat <- osmdata::osmdata_sc (q, doc = flist [1])
    cli::cli_text (
        "[1 / ", length (flist), "]: ",
        gsub (paste0 ("^", city, "\\-"), "", flist [1])
    )

    count <- 2L
    for (f in flist [-1]) {
        cli::cli_text (
            "[", count, " / ", length (flist), "]: ",
            gsub (paste0 ("^", city, "\\-"), "", f)
        )
        count <- count + 1L
        i <- osmdata::osmdata_sc (q, doc = f)

        dat$nodes <- rbind (dat$nodes, i$nodes)
        dat$relation_members <- rbind (dat$relation_members, i$relation_members)
        dat$relation_properties <-
            rbind (dat$relation_properties, i$relation_properties)
        dat$object <- rbind (dat$object, i$object)
        dat$object_link_edge <- rbind (dat$object_link_edge, i$object_link_edge)
        dat$edge <- rbind (dat$edge, i$edge)
        dat$vertex <- rbind (dat$vertex, i$vertex)
    }

    dat$nodes <- unique (dat$nodes)
    dat$relation_members <- unique (dat$relation_members)
    dat$relation_properties <- unique (dat$relation_properties)
    dat$object <- unique (dat$object)
    dat$object_link_edge <- unique (dat$object_link_edge)
    dat$edge <- unique (dat$edge)
    dat$vertex <- unique (dat$vertex)

    if (remove_osm_files) {
        fs::file_delete (flist)
    }

    saveRDS (dat, f_sc)
    cli::cli_alert_success (cli::col_green (
        "All OSM data collated and written to '",
        f_sc,
        "'"
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
