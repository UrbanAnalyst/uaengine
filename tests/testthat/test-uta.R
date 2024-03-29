test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

# Set up test files:
z <- gtfsrouter::berlin_gtfs_to_zip ()
gtfs <- gtfsrouter::extract_gtfs (z)
gtfs <- gtfsrouter::gtfs_timetable (gtfs, day = "Monday")

net <- m4ra::m4ra_hampi
xlim <- range (net$vertex$x_)
ylim <- range (net$vertex$y_)

gtfs$stops$stop_lat <- (gtfs$stops$stop_lat - min (gtfs$stops$stop_lat)) /
    diff (range (gtfs$stops$stop_lat))
gtfs$stops$stop_lat <- xlim [1] + diff (xlim) * gtfs$stops$stop_lat
gtfs$stops$stop_lon <- (gtfs$stops$stop_lon - min (gtfs$stops$stop_lon)) /
    diff (range (gtfs$stops$stop_lon))
gtfs$stops$stop_lon <- ylim [1] + diff (ylim) * gtfs$stops$stop_lon

gtfs_path <- fs::path (fs::path_temp (), "gtfs.Rds")
if (!file.exists (gtfs_path)) {
    saveRDS (gtfs, gtfs_path)
}

net_path <- fs::path (fs::path_temp (), "net.Rds")
if (!file.exists (net_path)) {
    saveRDS (net, net_path)
}

Sys.setenv ("M4RA_CACHE_DIR" = fs::path_temp ())
Sys.setenv ("M4RA_NUM_CORES" = 1L)

files <- m4ra::m4ra_weight_networks (net, city = "hampi")
files <- m4ra::m4ra_prepare_data (
    net_path,
    gtfs_path,
    city_name = "hampi",
    day = "mo",
    start_time_limits = 12:13 * 3600,
    final_mode = "foot"
)

test_that ("ua errors", {

    expect_error (
        ua_index (city = NULL, gtfs = gtfs_path, from = NULL),
        "Assertion on 'city' failed: Must be of type 'character'"
    )
    expect_error (
        ua_index (city = "hampi", gtfs = gtfs_path, from = NULL),
        "Assertion on 'from' failed: Must be of type 'character'"
    )
    from <- paste0 (1:10)
    expect_error (
        ua_index (city = "hampi", gtfs = gtfs_path, from = from),
        "'soc' must be provided"
    )
})

test_that ("ua calculations", {

    # make polygons for socio-economic variable:
    hull <- chull (net$vertex [, c ("x_", "y_")])
    hull <- c (hull, hull [1])
    p <- sfheaders::sf_polygon (net$vertex [hull, c ("x_", "y_")])
    p <- sf::st_sf (p, crs = 4326)
    p <- sf::st_make_grid (p, cellsize = c (0.05, 0.05))
    a <- sf::st_sf (var = runif (length (p)), geometry = p)

    # Get origin vertices
    # v <- ua_vertices (a, city = "hampi", mode = "foot")
    # from <- sample (v$id, npts)
    net <- m4ra::m4ra_load_cached_network (city = "hampi", mode = "foot")
    set.seed (1L)
    npts <- 10L
    from <- sample (net$.vx0, size = npts)

    res <- ua_index (
        city = "hampi",
        gtfs = gtfs_path,
        from = from,
        initial_mode = "foot",
        final_mode = "foot",
        soc = a,
        soc_var = "var",
        duration_max = NULL
    )

    expect_s3_class (res, c ("sf", "data.frame"))
    expect_equal (nrow (res), npts)

    # Then 'ua_interpolate':
    requireNamespace ("rappdirs") # otherwise 'Loading' message is issued
    expect_silent (
        graph <- ua_interpolate (
            city = "hampi",
            initial_mode = "foot",
            ua_dat = res,
            soc = a
        )
    )

    expect_silent (
        graph_full <- m4ra::m4ra_load_cached_network (
            city = "hampi",
            mode = "foot",
            contracted = FALSE
        )
    )
    expect_equal (nrow (graph_full), nrow (graph))
    expect_equal (ncol (graph_full), ncol (graph))
    expect_equal (names (graph_full), names (graph))

    # ua_to_geojson:
    f <- fs::path (fs::path_temp (), "xy.json")
    expect_false (fs::file_exists (f))
    f <- ua_to_geojson (graph, filename = f)
    expect_s3_class (f, "geojson_file")
    expect_true (fs::file_exists (f$path))
    fs::file_delete (f$path)
})
