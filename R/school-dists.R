add_dist_to_schools <- function (s, city, gtfs_path) {

    files <- m4ra::m4ra_prepare_data (
        gtfs = gtfs_path,
        city_name = city,
        final_mode = "bicycle",
        quiet = TRUE
    )

    path <- fs::path (
        fs::path_dir (files [1]),
        paste0 ("m4ra-", city, "-schools.Rds")
    )
    if (!file.exists (path)) {
        stop ("No schools file found at [", path, "]", call. = FALSE)
    }

    schools <- readRDS (path)
    schools_xy <- sf::st_coordinates (schools$osm_points)

    graph <- m4ra::m4ra_load_cached_network (
        city = city,
        mode = "foot",
        contracted = FALSE
    )
    v <- m4ra::m4ra_vertices (graph, city)

    pts <- dodgr::match_points_to_verts (v, schools_xy, connected = TRUE)
    to <- v$id [pts]
    d <- dodgr::dodgr_dists_nearest (graph, from = s$osm_id, to = to)

    s$school_dist <- d$d

    return (s)
}
