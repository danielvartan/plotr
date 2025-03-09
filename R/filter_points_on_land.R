# library(checkmate)
# library(dplyr)
# library(magrittr)
# library(sf)
# library(tidyr)

filter_points_on_land <- function(data, geometry) {
  checkmate::assert_tibble(data)
  checkmate::assert_subset(c("longitude", "latitude"), names(data))
  checkmate::assert_class(geometry, "sfc_MULTIPOLYGON")

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  row_number <- latitude <- longitude <- NULL
  # nolint end

  box <- geometry |> sf::st_bbox() #nolint

  data <-
    data |>
    dplyr::mutate(row_number = dplyr::row_number()) |>
    dplyr::relocate(row_number)

  na_cases <-
    data |>
    dplyr::select(row_number, latitude, longitude) |>
    dplyr::filter(is.na(latitude) | is.na(longitude))

  points <-
    data |>
    dplyr::select(row_number, latitude, longitude) |>
    tidyr::drop_na() |>
    sf::st_as_sf(
      coords = c("longitude", "latitude"),
      crs = sf::st_crs(geometry)
    ) |>
    sf::st_filter(geometry)

  valid_rows <- c(points$row_number, na_cases$row_number)

  data |>
    dplyr::filter(row_number %in% valid_rows) |>
    dplyr::select(-row_number)
}
