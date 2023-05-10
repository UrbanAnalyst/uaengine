add_parking <- function (s, city, initial_mode) {

    cache_dir <- fs::path (m4ra_cache_dir (), city)
    f <- fs::dir_ls (cache_dir, regexp = "parking")
    if (length (f) != 1L) {
        message (
            "No parking file found at [", cache_dir,
            "]. Parking densities can not be calculated."
        )
        return (s)
    }

    parking <- readRDS (f)
    p <- as.numeric (parking$parking / parking$building_volume)

    index <- geodist::geodist_min (s [, c ("x", "y")], parking [, c ("x", "y")])
    s$parking <- p [index]

    return (s)
}
