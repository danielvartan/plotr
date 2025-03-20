# # Helpers
#
# geocoded_data <- targets::tar_read("geocoded_data")
# geocoded_data |> plot_world_countries()
#
# geocoded_data |>
#   dplyr::mutate(
#     country = dplyr::case_match(
#       country,
#       "Cabo Verde" ~ "Cape Verde",
#       "Czechia" ~ "Czech Republic",
#       "Russian Federation" ~ "Russia",
#       "United Kingdom" ~ "UK",
#       "United States" ~ "USA",
#       .default = country
#     )
#   ) |>
#   tidyr::drop_na() |>
#   dplyr::pull(country) |>
#   unique() |>
#   setdiff(world_data$region |> unique())

plot_world_countries <- function(
    data, #nolint
    col_fill = NULL,
    col_country = "country",
    brandr = file.exists(here::here("_brand.yml")),
    print = TRUE,
    quiet = FALSE,
    ...
  ) {
  checkmate::assert_tibble(data)
  checkmate::assert_subset("country", names(data))
  checkmate::assert_string(col_fill, null.ok = TRUE)
  checkmate::assert_choice(col_fill, names(data), null.ok = TRUE)
  checkmate::assert_string(col_country)
  checkmate::assert_choice(col_country, names(data))
  checkmate::assert_character(data[[col_country]])
  checkmate::assert_flag(brandr)
  checkmate::assert_flag(print)
  checkmate::assert_flag(quiet)

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  geom <- n <- NULL
  # nolint end

  if (isTRUE(brandr)) {
    color_scale <- brandr::scale_brand(aesthetics = "fill", ...)
  } else {
    color_scale <- ggplot2::scale_fill_continuous(...)
  }

  data <-
    data |>
    dplyr::mutate(
      !!as.symbol(col_country) := dplyr::case_match(
        !!as.symbol(col_country),
        "Cabo Verde" ~ "Cape Verde",
        "Czechia" ~ "Czech Republic",
        "Russian Federation" ~ "Russia",
        "United Kingdom" ~ "UK",
        "United States" ~ "USA",
        .default = !!as.symbol(col_country)
      )
    ) |>
    orbis::get_map_fill_data(
      col_fill = col_fill,
      col_code = col_country,
      name_col_ref = "ID",
      quiet = quiet
    ) |>
    dplyr::right_join(
      maps::map("world", plot = FALSE, fill = TRUE) |>
        sf::st_as_sf(),
      by = "ID"
    )

  plot <-
    data |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      ggplot2::aes(geometry = geom, fill = n),
      color = "gray75",
      linewidth = 0.1,
      fill = "white"
    ) +
    ggplot2::geom_sf(
      ggplot2::aes(geometry = geom, fill = n),
      color = NA
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(-150, 150, 50)
      # limits = c(-180, 180)
    )

  plot <-
    plot +
    color_scale() +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = NULL
    )

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
