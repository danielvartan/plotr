# # Helpers
#
# geocoded_data <- targets::tar_read("geocoded_data")
# geocoded_data |> plot_brazil_state()
#
# weighted_data <- targets::tar_read("weighted_data")
#
# # Note that this data went through a filtering process that relied not
# # only on latitude, but also in sex and age (`tidyr::drop_na()`).
# weighted_data |> plot_brazil_state()

plot_brazil_state <- function(
    data, #nolint
    col_fill = NULL,
    col_code = "state_code",
    year = 2020,
    print = TRUE,
    quiet = FALSE,
    brandr = file.exists(here::here("_brand.yml")),
    scale_type = "binned",
    ...
  ) {
  prettycheck::assert_internet()
  checkmate::assert_data_frame(data)
  checkmate::assert_string(col_fill, null.ok = TRUE)
  checkmate::assert_choice(col_fill, names(data), null.ok = TRUE)
  if (!is.null(col_fill)) checkmate::assert_numeric(data[[col_fill]])
  checkmate::assert_string(col_code)
  checkmate::assert_choice(col_code, names(data))
  checkmate::assert_integerish(data[[col_code]])
  checkmate::assert_int(year)
  checkmate::assert_flag(print)
  checkmate::assert_flag(quiet)
  checkmate::assert_flag(brandr)
  checkmate::assert_string(scale_type)

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  geom <- n <- unit <- NULL
  # nolint end

  if (isTRUE(brandr)) {
    color_scale <- brandr::scale_brand(
      aesthetics = "fill",
      scale_type = scale_type,
      ...
    )
  } else {
    color_scale <- ggplot2::scale_fill_binned(...)
  }

  plot <-
    data |>
    orbis::get_map_fill_data(
      col_fill = col_fill,
      col_code = col_code,
      name_col_ref = "code_state",
      quiet = quiet
    ) |>
    dplyr::right_join(
      geobr::read_state(
        year = orbis::get_closest_geobr_year(year),
        showProgress = FALSE
      ) |>
        shush(),
      by = "code_state"
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      ggplot2::aes(geometry = geom),
      color = "gray75",
      linewidth = 0.1,
      fill = "white"
    ) +
    ggplot2::geom_sf(
      ggplot2::aes(geometry = geom, fill = n),
      color = NA
    ) +
    ggspatial::annotation_scale(
      ggplot2::aes(),
      location = "br",
      style = "tick",
      height = ggplot2::unit(0.15, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = "br",
      height = ggplot2::unit(1, "cm"),
      width = ggplot2::unit(1, "cm"),
      pad_x = ggplot2::unit(0.1, "cm"),
      pad_y = ggplot2::unit(0.55, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering
    ) +
    ggspatial::coord_sf(crs = 4674) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    color_scale

  if (isTRUE(print)) print(plot) |> shush()

  invisible(plot)
}
