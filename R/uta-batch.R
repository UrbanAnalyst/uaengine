#' @title Batch calculation of 'uta_index' for a single city
#'
#' @description Repeatedly calculates values from the \link{uta_index} function
#' for a sub-set of all origin vertices within nominated city, and saves results
#' to a local file. Finally results can generally be reliably interpolated, and
#' it is thus generally not necessarily to calculate \link{uta_index} for every
#' origin point. Instead, it is recommended to only calculate values for a fixed
#' proportion specified by the parameter, `coverage`.
#'
#' @note Importantly, this function can safely be interrupted at any time, and
#' will simply restart at the point where it was stopped.
#'
#' @param city Name of city, used to name and define local path to
#' pre-calculated street networks and transport times with \pkg{m4ra} package.
#' @param gtfs_path Path to `.Rds` version of GTFS feed for specified city.
#' @param popdens_geotif Optional path to local 'geotiff' file with population
#' density estimates. If provided, all transport indices are adjusted to account
#' for effects of local population density.
#' @param results_path Local path to directory where results are to be written.
#' @param initial_mode Initial mode of transport from each 'from' point to
#' public transport system (or to destination points, where single-model
#' transport is faster).
#' @param final_mode Final mode of transport from each public transport
#' destination point to all other network nodes.
#' @param soc Socio-demographic data with an \pkg{sf}-format column of polygons
#' for each observed value of target variable.
#' @param soc_var Name of target variable in `soc` data set.
#' @param dlims Vector of distance limits in kilometres over which average
#' values of transport index should be calculated. One value of "uta_index" is
#' then derived for each value of `dlims`.
#' @param batch_size Number of vertices to calculate in each batch before saving
#' to local file. Increases in this parameter can greatly increase the sizes of
#' intermediate results, leading to memory problems. Note that batch runs will
#' only be able to stopped and restarted if this parameter is kept constant.
#' Changes to this parameter will cause re-started runs to revert to the
#' beginning, and will generate results which overlap with any previous results
#' still retained in `results_path`.
#' @param coverage Proportion of all points for which values are to be
#' calculated. As described above, coverage can generally safely be set to
#' values less than 1, with complete coverage then generated through the more
#' computationally efficient \link{uta_interpolate} function.
#' @param nthreads Optional parameter to specify number of threads to be used in
#' multi-threaded calculations. Default is maximum number of available threads
#' (from `RcppParallel::defaultNumThreads()`).
#'
#' @export

uta_index_batch <- function (city,
                             gtfs_path,
                             popdens_geotif = NULL,
                             results_path = NULL,
                             initial_mode = "foot",
                             final_mode = "foot",
                             soc = NULL,
                             soc_var = NULL,
                             dlims = c (5, 10),
                             batch_size = 1000,
                             coverage = 1 / 4,
                             nthreads = NULL) {

    # most parameters are asserted in 'uta_index' function
    if (!is.null (nthreads)) {
        checkmate::assert_integer (nthreads, lower = 1L, len = 1L)
        requireNamespace ("RcppParallel")
        RcppParallel::setThreadOptions (numThreads = nthreads)
    }
    checkmate::assert_integer (batch_size, lower = 1L, len = 1L)
    checkmate::assert_numeric (coverage, lower = 0.01, upper = 1, len = 1L)
    checkmate::assert_numeric (coverage, lower = 0.01, upper = 1, len = 1L)

    # Parameters used before passing to 'uta_index':
    checkmate::assert_character (city, len = 1L)
    city <- tolower (gsub ("\\s+", "-", city))
    checkmate::assert_character (initial_mode, len = 1L)
    dodgr_modes <- unique (dodgr::weighting_profiles$weighting_profiles$name)
    initial_mode <- match.arg (tolower (initial_mode), dodgr_modes)

    v <- get_batch_vertices (soc, city, initial_mode)

    tmp <- get_vertex_indices (v, batch_size, coverage, city, results_path)
    vsp <- tmp$vsp
    index <- tmp$index
    rm (tmp)

    t_start <- batch_start_message ()

    for (i in seq_along (vsp)) {

        f <- get_batch_filename (results_path, city, index [[i]])

        from <- vsp [[i]]$id
        pt0 <- proc.time ()

        batch_progress_message (i, vsp, pt0, t_start)
    }
}

#' Return all vertices of network weighted for nominated `mode`, and ordered in
#' a repeatable way to allow consistent sub-sampling.
#' @noRd
get_batch_vertices <- function (soc, city, mode, seed = 1L) {

    v <- uta_vertices (soc, city, mode)

    # Randomise vertex order so samples are sufficiently random:
    set.seed (seed)
    v <- v [sample (nrow (v), size = nrow (v)), ]

    return (v)
}

#' Split vertices into list items of 'batch_size' each, along with corresponding
#' indices into the original vertex table.
#' @noRd
get_vertex_indices <- function (v, batch_size, coverage, city, results_path) {

    n <- ceiling (nrow (v) / batch_size)
    index <- rep (seq_len (n), each = batch_size) [seq_len (nrow (v))]
    vsp <- split (v, f = as.factor (index))
    index <- split (seq_len (nrow (v)), f = as.factor (index))

    index2 <- seq_len (ceiling (length (vsp) * coverage))
    vsp <- vsp [index2]
    index <- index [index2]

    # Then remove any which have previously been done:
    ivals <- vapply (index, max, integer (1L))
    fnames <- paste0 (city, "-", sprintf ("%06i", ivals), ".Rds")
    fnames <- fs::path_abs (fs::path (results_path, fnames))
    index2 <- which (!fs::file_exists (fnames))

    cli::cli_alert_info (cli::col_cyan (paste0 (
        "Skipping [",
        length (fnames) - length (index2),
        "] previously calculated files, with [",
        length (index2),
        "] still to go."
    )))

    vsp <- vsp [index2]
    index <- index [index2]

    return (list (vsp = vsp, index = index))
}

batch_start_message <- function () {

    msg <- paste0 ("Staring processing at ", Sys.time ())
    cli::cli_h1 (cli::col_green (msg))
    cat ("\n")

    return (proc.time ())
}

#' @param index One list element of the full `index` generated by
#' `get_vertex_indices()`.
#' @noRd
get_batch_filename <- function (results_path, city, index) {

    ival <- max (index)
    f <- paste0 (city, "-", sprintf ("%06i", ival), ".Rds")
    f <- fs::path_abs (fs::path (results_path, f))

    return (f)
}

batch_progress_message <- function (i, vsp, pt0, t_start) {

    cli::cli_h1 (cli::col_yellow (paste0 (
        i, " / ", length (vsp), " = ",
        round (100 * i / length (vsp), digits = 2), "%"
    )))

    pt1 <- proc.time () - pt0
    dur1 <- hms::hms (as.integer ((proc.time () - pt0) [3]))
    dur_tot <- hms::hms (as.integer ((proc.time () - t_start) [3]))
    tremaining <- (length (vsp) - i) * dur_tot / i
    tremaining <- hms::hms (as.integer (tremaining))

    cli::cli_alert_info (cli::col_green (paste0 (
        "Calculation time: ", dur1,
        "; total = ", dur_tot,
        "; remaining = ", tremaining
    )))
}
