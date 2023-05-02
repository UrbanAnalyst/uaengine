add_dist_to_schools <- function (s, city, gtfs_path) {

    cache_dir <- fs::path (m4ra_cache_dir (), city)
    f <- fs::dir_ls (cache_dir, regexp = "school")
    if (length (f) != 1L) {
        message (
            "No schools file found at [", cache_dir,
            "]. Distances to schools can not be calculated."
        )
        return (s)
    }

    schools <- readRDS (f) [, c ("x_", "y_")]

    graph <- m4ra::m4ra_load_cached_network (
        city = city,
        mode = "foot",
        contracted = FALSE
    )
    v <- m4ra::m4ra_vertices (graph, city)

    pts <- dodgr::match_points_to_verts (v, schools, connected = TRUE)
    to <- v$id [pts]
    d <- dodgr::dodgr_dists_nearest (graph, from = s$osm_id, to = to)

    s$school_dist <- d$d

    return (s)
}
