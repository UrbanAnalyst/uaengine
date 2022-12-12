
# Same as `m4ra::m4ra_cache_dir()`
m4ra_cache_dir <- function () {

    cache_dir <- Sys.getenv ("M4RA_CACHE_DIR")

    if (cache_dir == "") {

        cache_dir <- fs::path_expand (fs::path (
            rappdirs::user_cache_dir (),
            "R",
            "m4ra"
        ))

        if (!dir.exists (cache_dir)) {
            dir.create (cache_dir, recursive = TRUE)
        }
    }

    return (cache_dir)

}
