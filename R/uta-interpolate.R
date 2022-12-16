
#' Interpolate "UTA_index" between vertices on network
#'
#' @param city Name of city, used to name and define local path to
#' pre-calculated street networks and transport times with \pkg{m4ra} package.
#' @param from Character vector of Open Street Map IDs of vertices from which
#' values in `uta_dat` were calculated.
#' @param initial_mode Initial mode of transport from each 'from' point to
#' public transport system (or to destination points, where single-model
#' transport is faster).
#' @param uta_dat Result of \link{uta_index} function.
#' @param npts Number of nearest neighbours to use to interpolate values at each
#' point.
#'
#' @return A `data.frame` of all vertices in network, along with variables
#' defined in `uta_dat` as interpolated on to all vertices in the network.
#' @export

uta_interpolate <- function (city,
                             from = NULL,
                             initial_mode = "foot",
                             uta_dat = NULL,
                             npts = 3L) {

    requireNamespace ("dodgr")

    city <- tolower (gsub ("\\s+", "-", city))
    checkmate::assert_character (from, min.len = 1L)
    checkmate::assert_character (initial_mode, min.len = 1L, max.len = 1L)

    if (is.null (uta_dat)) {
        stop ("'uta_dat' must be provided.", call. = FALSE)
    }

    graph <- m4ra::m4ra_load_cached_network (city, initial_mode, contracted = TRUE)
    v <- m4ra::m4ra_vertices (graph, city)

    index <- dodgr::match_points_to_verts (v, sf::st_coordinates (uta_dat))
    uta_vars <- grep ("(\\_pop\\_adj$|^uta\\_index\\_)", names (uta_dat), value = TRUE)
    for (u in uta_vars) {
        v [[u]] <- NA
        v [[u]] [index] <- uta_dat [[u]]
    }

    to <- v$id [index]

    res <- m4ra::m4ra_dists_n_pts (graph, from, to, npts = npts)

    dist_wts <- exp (-res$dist_mat / 1000)
    dist_wts <- dist_wts / rowSums (dist_wts, na.rm = TRUE)

    for (u in uta_vars) {
        uta_mat <- array (v [[u]] [res$index], dim = dim (res$dist_mat))
        uta_vals <- rowSums (uta_mat * dist_wts, na.rm = TRUE)

        v [[u]] [match (from, v$id)] <- uta_vals
    }

    return (v)
}
