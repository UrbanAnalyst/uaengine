#' Natural space index is proportion of local foot ways which pass through green
#' space, or next to water.
#' @noRd
add_natural_space_index <- function (s, city, f, dlimit = 2000) {

    mode <- "foot"
    net <- m4ra::m4ra_load_cached_network (city = city, mode = mode)
    v <- m4ra::m4ra_vertices (net, city)

    index <- readRDS (f)

    green <- v$id [which (index > 0)]
    index_green <- which (net$.vx0 %in% green | net$.vx1 %in% green)
    water <- v$id [which (index < 0)]
    index_water <- which (net$.vx0 %in% water | net$.vx1 %in% water)

    net$edge_type <- "no"
    net$edge_type [index_green] <- "green"
    net$edge_type [index_water] <- "water"

    from <- s$osm_id
    index <- which (!from %in% net$.vx0)
    if (length (index) > 0L) {
        # Not a bicycle network, so match points to nearest bike points:
        v <- m4ra::m4ra_vertices (net, city)
        s_xy <- s [index, c ("x", "y")]
        vert_index <- dodgr::match_pts_to_verts (v, s_xy)
        from [index] <- v$id [vert_index]
    }

    d <- dodgr::dodgr_dists_categorical (
        net,
        from = from,
        proportions_only = TRUE,
        dlimit = dlimit
    )

    cols <- c ("green", "water")
    cols <- cols [which (cols %in% names (d))]
    s$natural <- rowSums (d [, cols]) / d$distance

    return (s)
}
