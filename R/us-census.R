#' Add variables on median rent and housing value from US census.
#'
#' @noRd
add_us_census_vars <- function (s, city, census_year = 2020) {
    if (!is_us_city (city)) {
        return (s)
    }

    requireNamespace ("tidycensus")
    key <- Sys.getenv ("CENSUS_API_KEY")
    if (!nzchar (key)) {
        stop ("CENSUS_API_KEY not found.", call. = FALSE)
    }
    suppressMessages (
        tidycensus::census_api_key (key)
    )
    options (tigris_use_cache = TRUE)

    states <- data.frame (
        city = c ("philadelphia", "washington"),
        state = c ("PA", "DC")
    )
    state <- states$state [match (city, states$city)]

    s_g <- sfheaders::sf_point (s [, c ("x", "y")]) |>
        sf::st_sf (crs = 4326)
    xy <- sf::st_coordinates (x)
    xy <- round (apply (xy, 2, mean)) [1:2]
    s_g <- reproj_equal_area (s_g, lon = xy [1], lat = xy [2])

    # Then reproj census data to same focal lon/lat:
    census_data <- get_census_variables (state, census_year) |>
        reproj_equal_area (lon = xy [1], lat = xy [2])

    index <- vapply (sf::st_within (s_g, census_data), function (i) {
        ifelse (length (i) == 0, NA_integer_, i)
    }, integer (1L))

    s$rent <- census_data$rent [index]
    s$value <- census_data$value [index]

    return (s)
}

is_us_city <- function (city) {
    city %in% c ("philadelphia", "washington")
}

get_census_variables <- function (state = NULL, census_year = 2020) {

    checkmate::assert_character (state, len = 1L)
    checkmate::assert_numeric (census_year, len = 1L)

    from_census <- FALSE
    if (from_census) {
        v20 <- tidycensus::load_variables (
            year = census_year, "acs5", cache = TRUE
        )
        v20 <- v20 [which (v20$geography == "block group"), ]

        code_rent <- v20$name [grep ("Median\\scontract\\srent", v20$label)]
        code_value <- v20$name [which (grepl ("Median\\svalue", v20$label) &
            !grepl ("MOBILE\\sHOMES", v20$concept))]
        ptn <- "Median\\snumber\\sof\\srooms"
        code_rooms_renter <-
            v20$name [grep (paste0 (ptn, ".*Renter"), v20$label)]
        code_rooms_owner <-
            v20$name [grep (paste0 (ptn, ".*Owner"), v20$label)]

        codes <- c (
            code_rent, code_value, code_rooms_renter, code_rooms_owner
        )
    } else {
        codes <- c ("B25058_001", "B25077_001", "B25021_003", "B25021_002")
    }

    x <- tidycensus::get_acs (
        geography = "block group",
        variables = codes,
        state = state,
        geometry = TRUE,
        year = census_year,
        output = "wide"
    )

    vars <- c ("rent", "value", "rooms_renter", "rooms_owner")
    codes <- paste0 (codes, "E") # For the actual estimates

    # Replace NA rented rooms with owner equilvalent:
    index <- which (is.na (x [[codes [vars == "rooms_renter"]]]))
    x [[codes [vars == "rooms_renter"]]] [index] <-
        x [[codes [vars == "rooms_owner"]]] [index]
    # And then NA owner with renter equivalent:
    index <- which (is.na (x [[codes [vars == "rooms_owner"]]]))
    x [[codes [vars == "rooms_owner"]]] [index] <-
        x [[codes [vars == "rooms_renter"]]] [index]

    x$rent <- x [[codes [vars == "rent"]]] / x [[codes [vars == "rooms_renter"]]]
    x$value <- x [[codes [vars == "value"]]] / x [[codes [vars == "rooms_owner"]]]

    suppressWarnings (
        x <- sf::st_cast (x, "POLYGON")
    )
    x <- x [which (vapply (x$geometry, length, integer (1L)) > 0L), ]

    cols <- c ("GEOID", "rent", "value", "geometry")
    x [, cols]
}
