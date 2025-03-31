plot_brazil_point <- function(
    data, #nolint
    col_latitude = "latitude",
    col_longitude = "longitude",
    col_group = NULL,
    year = 2020,
    alpha = 0.75,
    size_point = 0.5,
    print = TRUE,
    brandr = file.exists(here::here("_brand.yml")),
    ...
  ) {
  prettycheck::assert_internet()
  checkmate::assert_data_frame(data)
  checkmate::assert_string(col_latitude)
  checkmate::assert_subset(col_latitude, names(data))
  prettycheck::assert_numeric(data[[col_latitude]])
  checkmate::assert_string(col_longitude)
  checkmate::assert_subset(col_longitude, names(data))
  prettycheck::assert_numeric(data[[col_longitude]])
  checkmate::assert_string(col_group, null.ok = TRUE)
  checkmate::assert_choice(col_group, names(data), null.ok = TRUE)
  checkmate::assert_int(year)
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_number(size_point, lower = 0)
  checkmate::assert_flag(print)
  checkmate::assert_flag(brandr)

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  . <- geom <- unit <- NULL
  # nolint end

  if (isTRUE(brandr)) {
    color_scale <- brandr::scale_brand(...)
  } else {
    color_scale <- ggplot2::scale_color_continuous(...)
  }

  brazil_state_data <-
    geobr::read_state(
      year = get_closest_geobr_year(year),
      showProgress = FALSE
    ) |>
    shush()

  plot <-
    data |>
    orbis::filter_points_on_land(dplyr::pull(brazil_state_data, geom)) |>
    dplyr::select(
      dplyr::all_of(c(col_latitude, col_longitude, col_group))
    ) |>
    tidyr::drop_na() %>% # Don't change the pipe!
    {
      if (is.null(col_group)) {
        ggplot2::ggplot(
          ggplot2::aes(
            x = !!as.symbol(col_longitude),
            y = !!as.symbol(col_latitude)
          ),
          data = .
        )
      } else {
        ggplot2::ggplot(
          ggplot2::aes(
            x = !!as.symbol(col_longitude),
            y = !!as.symbol(col_latitude),
            color = !!as.symbol(col_group)
          ),
          data = .
        )
      }
    } +
    ggplot2::geom_sf(
      data = brazil_state_data,
      color = "gray75",
      fill = "white",
      linewidth = 0.1,
      inherit.aes = FALSE
    ) + {
      if (is.null(col_group)) {
        ggplot2::geom_point(
          size = size_point,
          color = "#964D01",
          alpha = alpha
        )
      } else {
        ggplot2::geom_point(
          size = size_point,
          alpha = alpha
        )
      }
    } +
    ggspatial::annotation_scale(
      location = "br",
      style = "tick",
      height = ggplot2::unit(0.15, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = "br",
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      pad_x = ggplot2::unit(0.1, "cm"),
      pad_y = ggplot2::unit(0.55, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering
    ) +
    ggspatial::coord_sf(crs = 4674) +
    color_scale +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = 5))
    )

  if (isTRUE(print)) print(plot) |> shush()

  invisible(plot)
}
