#' Assign raster cell population density values to points
#'
#' @param verts A table of vertices with longitude and latitude coordinates,
#' such as the "verts" component of a \pkg{silicate}-class object representing a
#' local street network, generated with \pkg{dodgr} function,
#' `dodgr_streetnet_sc()`.
#' @param geotiff Path to 'geotiff' file containing population density
#' estimates, and including the area defined by 'net_sc'.
#' @param normalise If `TRUE`, normalise results so such of densities at all
#' network vertices is equal to sum of original densities.
#' @return A \code{data.frame} containing all vertices of 'net_sc', and
#' corresponding point estimates of population density.
#' @export
pop2point <- function (verts, geotiff, normalise = TRUE) {

    checkmate::assert_file_exists (geotiff)

    ras <- raster::raster (geotiff)

    if (all (c ("x_", "y_") %in% names (verts))) {
        vxy <- verts [, c ("x_", "y_")]
    } else {
        vxy <- verts [, c ("x", "y")]
    }
    bb <- t (apply (vxy, 2, range))
    ras <- raster::crop (ras, raster::extent (bb))

    verts_matched <- assign_points (ras, verts, normalise)

    return (verts_matched)
}

assign_points <- function (ras, verts, normalise = TRUE) {

    ras_pts <- raster::rasterToPoints (ras)

    ras_match <- geodist::geodist_min (verts, ras_pts, measure = "cheap")

    # Then allocate density estimates equally between all verts which map onto
    # single raster density points:
    nm <- colnames (ras_pts) [which (!colnames (ras_pts) %in% c ("x", "y"))]
    verts [[nm]] <- ras_pts [ras_match, 3]

    if (normalise) {

        ras_match_tab <- table (ras_match)
        ras_match_counts <- ras_match_tab [match (ras_match, names (ras_match_tab))]

        verts [[nm]] <- verts [[nm]] / ras_match_counts
    }

    return (verts)
}
