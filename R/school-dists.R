add_dist_to_schools <- function (s, city) {

    cache_dir <- m4ra_cache_dir (city)
    f <- fs::dir_ls (cache_dir, regexp = "school")
    if (length (f) != 1L) {
        message (
            "No schools file found at [", cache_dir,
            "]. Distances to schools can not be calculated."
        )
        return (s)
    }

    schools <- readRDS (f)

    graph <- m4ra::m4ra_load_cached_network (
        city = city,
        mode = "foot",
        contracted = FALSE
    )
    v <- m4ra::m4ra_vertices (graph, city)

    if (!"vert_index" %in% names (schools)) {

        schools_xy <- schools [, c ("x_", "y_")]
        pts <- dodgr::match_points_to_verts (v, schools_xy, connected = TRUE)
        schools$vert_index <- pts
        saveRDS (schools, f)
    }

    to <- v$id [schools$vert_index]
    d <- dodgr::dodgr_dists_nearest (graph, from = s$osm_id, to = to)

    s$school_dist <- d$d

    return (s)
}
