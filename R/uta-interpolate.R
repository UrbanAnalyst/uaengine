
#' Interpolate "UTA_index" between vertices on network, project those on to the
#' original network edges, and return the full network graph.
#'
#' @param city Name of city, used to name and define local path to
#' pre-calculated street networks and transport times with \pkg{m4ra} package.
#' @param initial_mode Initial mode of transport from each 'from' point to
#' public transport system (or to destination points, where single-model
#' transport is faster).
#' @param uta_dat Result of \link{uta_index} function.
#' @param soc Socio-demographic data with an \pkg{sf}-format column of polygons
#' for each observed value of target variable, as passed to \link{uta_index}
#' function.
#' @param npts Number of nearest neighbours to use to interpolate values at each
#' point.
#'
#' @return The full network graph with additional variables quantifying for each
#' edges the values defined in `uta_dat` as projected on to those edges.
#' @export

uta_interpolate <- function (city,
                             initial_mode = "foot",
                             uta_dat = NULL,
                             soc,
                             npts = 3L) {

    requireNamespace ("dodgr")
    requireNamespace ("fst")
    requireNamespace ("rappdirs") # for imported m4ra:::m4ra_cache_dir fn

    city <- tolower (gsub ("\\s+", "-", city))
    checkmate::assert_character (initial_mode, min.len = 1L, max.len = 1L)

    if (is.null (uta_dat)) {
        stop ("'uta_dat' must be provided.", call. = FALSE)
    }

    graph <- m4ra::m4ra_load_cached_network (
        city,
        initial_mode,
        contracted = TRUE
    )
    v <- m4ra::m4ra_vertices (graph, city)

    # reduce vertices to only those within polygon of 'uta_dat'
    v_sf <- sfheaders::sf_point (v [, c ("x", "y")])
    v_sf <- sf::st_sf (v_sf, crs = 4326)
    index <- sf::st_within (v_sf, sf::st_union (soc))
    index <- which (vapply (index, length, integer (1L)) > 0L)
    v_in <- v [index, ]

    index <- dodgr::match_points_to_verts (v_in, sf::st_coordinates (uta_dat))
    uta_vars <- grep (
        "(\\_pop\\_adj$|^uta\\_index\\_)",
        names (uta_dat),
        value = TRUE
    )

    for (u in uta_vars) {
        v [[u]] <- v_in [[u]] <- NA
        v_in [[u]] [index] <- uta_dat [[u]]
        v [[u]] [match (v_in$id, v$id)] <- v_in [[u]]
    }

    to <- v_in$id [index]
    from <- v_in$id [-index]

    # The `m4ra_dists_n_pts` routine has a glitch on my local Linux system,
    # apparently caused by Intel TBB, which causes some paralleled iterations to
    # return nothing, yet they return when re-requested as part of repeated,
    # smaller chunks. The following code repeats calls to the main function
    # until all `from` points return full result rows.
    res <- m4ra::m4ra_dists_n_pts (graph, from, to, npts = npts)
    nna <- apply (res$index_mat, 1, function (i) length (which (is.na (i))))
    index <- which (nna > 0)

    loop_count <- 0
    while (length (index) > 0 && loop_count < 100) {

        res2 <- m4ra::m4ra_dists_n_pts (
            graph,
            from = from [index],
            to,
            npts = npts
        )
        res$index_mat [index, ] <- res2$index_mat
        res$dist_mat [index, ] <- res2$dist_mat

        nna <- apply (res$index_mat, 1, function (i) length (which (is.na (i))))
        index <- which (nna > 0)

        loop_count <- loop_count + 1
    }

    # res$index_mat indexes into the full vertex table of the network, not the
    # reduced one in 'v_in'.

    dist_wts <- exp (-res$dist_mat / 1000)
    dist_wts <- dist_wts / rowSums (dist_wts, na.rm = TRUE)

    for (u in uta_vars) {
        uta_mat <- array (v [[u]] [res$index], dim = dim (res$dist_mat))
        uta_vals <- rowSums (uta_mat * dist_wts, na.rm = TRUE)
        # rowSums yields 0 if all values are NA, so:
        uta_vals [uta_vals == 0] <- NA

        v [[u]] [match (from, v$id)] <- uta_vals
    }

    index <- lapply (uta_vars, function (u) which (!is.na (v [[u]])))
    index <- sort (unique (unlist (index)))

    v <- v [index, ]

    graph <- graph [which (graph$.vx0 %in% v$id & graph$.vx1 %in% v$id), ]

    # Then project values at points back on to the graph edges:
    for (u in uta_vars) {
        val0 <- v [[u]] [match (graph$.vx0, v$id)]
        val1 <- v [[u]] [match (graph$.vx1, v$id)]
        graph [[u]] <- (val0 + val1) / 2
    }

    graph_full <- uta_uncontract_graph (graph, city, initial_mode)

    return (graph_full)
}

#' Modified from `dodgr:::uncontract_graph`
#'
#' @noRd
uta_uncontract_graph <- function (graph, city, initial_mode) {

    hash <- attr (graph, "hash")
    flist <- fs::dir_ls (fs::path (m4ra_cache_dir (), city), regexp = initial_mode)
    flist <- grep (substring (hash, 1, 6), flist, value = TRUE)

    edge_map <- fst::read_fst (grep ("edgemap", flist, value = TRUE))
    edge_map <- edge_map [which (edge_map$edge_new %in% graph$edge_), ]

    graph_full <- m4ra::m4ra_load_cached_network (
        city,
        initial_mode,
        contracted = FALSE
    )
    indx_to_full <- match (edge_map$edge_old, graph_full$edge_)
    indx_to_contr <- match (edge_map$edge_new, graph$edge_)

    # edge_map only has the contracted edges; flows from the original
    # non-contracted edges also need to be inserted
    edges <- graph$edge_ [which (!graph$edge_ %in% edge_map$edge_new)]
    indx_to_full <- c (indx_to_full, match (edges, graph_full$edge_))
    indx_to_contr <- c (indx_to_contr, match (edges, graph$edge_))

    index <- which (!names (graph) %in% names (graph_full))
    if (length (index) > 0) { # always the case here
        nms <- names (graph) [index]
        graph_full [nms] <- NA
        for (n in nms) {
            graph_full [[n]] [indx_to_full] <- graph [[n]] [indx_to_contr]
        }

        # `graph` has generally been reduced from cached version, to only those
        # vertices and hence edges within the polygons defined by `soc`. Any
        # edges beyond these limits then have NA values for the 'nms' variables.
        nna_index <- lapply (nms, function (n) which (!is.na (graph_full [[n]])))
        nna_index <- sort (unique (unlist (nna_index)))
        graph_full <- graph_full [nna_index, ]
    }

    return (graph_full)
}
