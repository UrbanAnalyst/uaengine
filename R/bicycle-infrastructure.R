add_bike_infrastructure <- function (s, city, dlimit = 5000) {

    mode <- "bicycle"
    net <- m4ra::m4ra_load_cached_network (city = city, mode = mode)

    # Code to add side-lane parking to maybe penalise residential streets with
    # parking?
    # parking <- osmdata::osmdata_sc (doc = "/data/data/OSM/bz2/berlin/berlin-parking.osm")

    # keys_parking <- c ("parking:lane:left", "parking:lane:right",
    #                   "parking:lane:both")
    # index <- which (parking$object$key %in% keys_parking)
    # ids <- unique (parking$object$object_ [index])
    # net$parking <- FALSE
    # net$parking [which (net$object_ %in% ids)] <- TRUE

    net$edge_type <- "none"

    # ----- weighting = 1
    net$edge_type [net$highway == "cycleway"] <- "full"
    cycleway_types <- c (
        "both",
        "lane",
        "opposite_lane",
        "sidepath",
        "track",
        "yes"
    )
    if ("cycleway" %in% names (net)) {
        net$edge_type [net$cycleway %in% cycleway_types] <- "full"
    }
    if ("cycleway:left" %in% names (net)) {
        net$edge_type [net$`cycleway:left` %in% cycleway_types] <- "full"
    }
    if ("cycleway:right" %in% names (net)) {
        net$edge_type [net$`cycleway:right` %in% cycleway_types] <- "full"
    }
    bicycle_types <- c (
        "designated",
        "sidepath",
        "use_sidepath"
    )
    net$edge_type [net$bicycle %in% bicycle_types] <- "full"

    # ----- weighting = 0.5
    highway_types <- c (
        "path",
        "living_street",
        "residential",
        "track"
    )
    net$edge_type [net$highway %in% highway_types] <- "half"
    cycleway_types <- c (
        "share_busway",
        "sidewalk"
    )
    if ("cycleway" %in% names (net)) {
        net$edge_type [net$cycleway %in% cycleway_types] <- "half"
    }

    # ----- weighting = 0.25
    highway_types <- c (
        "bridleway",
        "pedestrian"
    )
    net$edge_type [net$highway %in% highway_types] <- "quarter"
    bicycle_types <- c (
        "optional_sidepath",
        "permissive",
        "use_sidepath",
        "yes"
    )
    net$edge_type [net$bicycle %in% bicycle_types] <- "quarter"

    from <- s$osm_id
    index <- which (!from %in% net$.vx0)
    if (length (index) > 0L) {
        # Not a bicycle network, so match points to nearest bike points:
        v <- m4ra::m4ra_vertices (net, city)
        from_xy <- sf::st_coordinates (s)
        s_xy <- sf::st_coordinates (s) [index, ]
        vert_index <- dodgr::match_pts_to_verts (v, s_xy)
        from [index] <- v$id [vert_index]
    }

    d <- dodgr::dodgr_dists_categorical (
        net,
        from = from,
        proportions_only = TRUE,
        dlimit = dlimit
    )

    cols <- c ("full", "half", "quarter")
    wts <- c (1, 0.5, 0.25)
    index <- which (cols %in% names (d))
    cols <- cols [index]
    wts <- wts [index]
    wts_mat <- matrix (rep (wts, each = nrow (d)), ncol = length (wts))
    sums <- rowSums (d [, cols] * wts_mat)

    s$bike_index <- sums / d$distance

    return (s)
}
