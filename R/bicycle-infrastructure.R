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
    net$edge_type [net$cycleway %in% cycleway_types] <- "full"
    net$edge_type [net$`cycleway:left` %in% cycleway_types] <- "full"
    net$edge_type [net$`cycleway:right` %in% cycleway_types] <- "full"
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
    net$edge_type [net$cycleway %in% cycleway_types] <- "half"

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

    d <- dodgr::dodgr_dists_categorical (
        net,
        from = s$osm_id,
        proportions_only = TRUE,
        dlimit = dlimit
    )

    s$bike_index <- (d$full + 0.5 * d$half + 0.25 * d$quarter) / d$distance

    return (s)
}
