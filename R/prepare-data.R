#' Additional data preparation beyond standard `m4ra` routines
#'
#' @param osm_path Path to directory containing data processed by the
#' \link{uta_extract_osm} function for desired city.
#' @param water_dist Distance below which any edges are considerd adjance to
#' water, and thus categorised as "blue space" edges.
#' @return Name of file holding integer index for nominated city identifying
#' which vertices of the foot-weighted network are within or adjacent to natural
#' spaces.
#' @noRd
uta_prepare_data <- function (osm_path, water_dist = 20) {

    checkmate::assert_character (osm_path, len = 1L)
    if (!fs::dir_exists (osm_path)) {
        stop ("'osm_path' must be a path to a directory")
    }
    files <- fs::dir_ls (osm_path)

    f_natural <- grep ("natural", files, value = TRUE)
    city <- strsplit (f_natural, split = "\\/") [[1]]
    city <- utils::tail (city, 2L) [1]

    f_natural <- prepare_natural (f_natural, city, water_dist = water_dist)

    f_schools <- grep ("schools", files, value = TRUE)
    f_schools <- prepare_schools (f_schools, city)

    return (c (f_natural, f_schools))
}

prepare_natural <- function (f, city, water_dist = 20) {

    mode <- "foot" # hard-code distances to nature to 'foot' distances
    net <- m4ra::m4ra_load_cached_network (city = city, mode = mode)

    cache_dir <- fs::path (m4ra_cache_dir (), city)
    hash <- substring (attr (net, "hash"), 1L, 6L)
    f_natural <- paste0 ("uta-", city, "-natural-index-foot-", hash, ".Rds")
    f_natural <- fs::path (cache_dir, f_natural)

    if (file.exists (f_natural)) {
        return (f_natural)
    }

    natural <- sf::st_read (f, layer = "multipolygons")
    polys <- lapply (natural$geometry, function (i) i [[1]] [[1]])
    v <- m4ra::m4ra_vertices (net, city)

    index <- cpp_pip (polys, v) + 1L

    water <- natural$geometry [which (natural$natural == "water")]
    water <- do.call (rbind, lapply (water, function (i) i [[1]] [[1]]))
    colnames (water) <- c ("x", "y")
    system.time ( # around 1.5 minutes
        index_min <- geodist::geodist_min (v [, c ("x", "y")], water)
    )
    dmin <- geodist::geodist (v [, c ("x", "y")], water [index_min, ], paired = TRUE)
    index [which (dmin <= 20.0)] <- -1L

    saveRDS (index, f_natural)

    return (f_natural)
}

prepare_schools <- function (f, city) {

    cache_dir <- fs::path (m4ra_cache_dir (), city)
    f_schools <- fs::path (cache_dir, paste0 ("uta-", city, "-school-verts.Rds"))

    if (fs::file_exists (f_schools)) {
        return (f_schools)
    }

    dat <- osmdata::osmdata_sc (doc = f)

    index1 <- which (dat$object$key == "amenity" & dat$object$value == "school")
    index2 <- which (
        dat$object$key == "school" &
            dat$object$value %in% c ("elementary", "primary", "secondary")
    )
    ids <- dat$object$object_ [sort (unique (c (index1, index2)))]
    edges <- dat$object_link_edge$edge_ [dat$object_link_edge$object_ %in% ids]
    index <- which (dat$edge$edge_ %in% edges)
    verts <- unique (c (dat$edge$.vx0 [index], dat$edge$.vx1 [index]))
    verts <- dat$vertex [which (dat$vertex$vertex_ %in% verts), ]

    saveRDS (verts, f_schools)

    return (f_schools)
}
