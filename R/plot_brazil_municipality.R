# library(brandr) # github.com/danielvartan/brandr
# library(checkmate)
# library(dplyr)
# library(geobr)
# library(ggplot2)
# library(ggspatial)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

plot_brazil_municipality <- function(
    data, #nolint
    col_fill = NULL,
    col_code = "municipality_code",
    transform = "identity", # See `?ggplot2::scale_fill_gradient`
    direction = 1,
    alpha = 1,
    binned = TRUE,
    range = c(0, 10),
    breaks = ggplot2::waiver(),
    n_breaks = NULL,
    labels = ggplot2::waiver(),
    reverse = TRUE,
    limits = NULL,
    zero_na = FALSE,
    point = FALSE,
    print = TRUE,
    quiet = FALSE
  ) {
  prettycheck::assert_internet()
  checkmate::assert_tibble(data)
  checkmate::assert_string(col_fill, null.ok = TRUE)
  checkmate::assert_choice(col_fill, names(data), null.ok = TRUE)
  if (!is.null(col_fill)) prettycheck::assert_numeric(data[[col_fill]])
  checkmate::assert_string(col_code)
  checkmate::assert_choice(col_code, names(data))
  checkmate::assert_integerish(data[[col_code]])
  checkmate::assert_multi_class(transform, c("character", "transform"))
  checkmate::assert_choice(direction, c(-1, 1))
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_flag(binned)
  checkmate::assert_integerish(range, len = 2)
  checkmate::assert_multi_class(breaks, c("function", "numeric", "waiver"))
  checkmate::assert_int(n_breaks, lower = 1, null.ok = TRUE)
  checkmate::assert_multi_class(labels, c("function", "numeric", "waiver"))
  checkmate::assert_flag(reverse)
  checkmate::assert_flag(zero_na)
  checkmate::assert_flag(point)
  checkmate::assert_flag(print)
  checkmate::assert_flag(quiet)

  checkmate::assert_multi_class(
    limits, c("numeric", "function"), null.ok = TRUE
  )

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  geom <- n <- unit <- NULL
  # nolint end

  out <-
    data |>
    get_map_fill_data(
      col_fill = col_fill,
      col_code = col_code,
      name_col_ref = "code_muni",
      quiet = quiet
    ) |>
    dplyr::right_join(
      geobr::read_municipality(
        year = 2017,
        showProgress = FALSE
      ) |>
        rutils::shush(),
      by = "code_muni"
    )

  if (isTRUE(zero_na)) {
    out <- out |> dplyr::mutate(n = ifelse(is.na(n), 0, n))
  }

  if (isTRUE(point)) {
    plot <-
      out |>
      plot_brazil_municipality_point(
        alpha = alpha,
        range = range,
        breaks = breaks
      )
  } else {
    plot <-
      out |>
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
      )
  }

  plot <-
    plot +
    ggspatial::annotation_scale(
      location = "br",
      style = "tick",
      width_hint = 0.25,
      height = unit(0.15, "cm")
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
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = NULL,
      color = NULL,
      size = NULL
    ) +
    brandr::scale_brand(
      aesthetics = ifelse(isTRUE(point), "color", "fill"),
      scale_type = ifelse(isTRUE(binned), "binned", "continuous"),
      direction = direction,
      breaks = breaks,
      n.breaks = n_breaks,
      labels = labels,
      reverse = ifelse(isTRUE(point), FALSE, reverse),
      limits = limits,
      transform = transform
    )

  if (isTRUE(print)) print(plot) |> rutils::shush()

  invisible(plot)
}

# library(checkmate)
# library(dplyr)
# library(geobr)
# library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(sf)
# library(tidyr)

plot_brazil_municipality_point <- function(
    data, #nolint
    alpha = 0.7,
    range = c(0, 10),
    breaks = ggplot2::waiver()
  ) {
  prettycheck::assert_internet()
  checkmate::assert_tibble(data)
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_multi_class(breaks, c("function", "numeric", "waiver"))
  checkmate::assert_integerish(range, len = 2)

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  geom <- latitude <- longitude <- n <- unit <- NULL
  # nolint end

  data_points <-
    data |>
    sf::st_as_sf() |>
    sf::st_centroid() |>
    rutils::shush()

  data_points <- dplyr::tibble(
    longitude = sf::st_coordinates(data_points)[, 1],
    latitude = sf::st_coordinates(data_points)[, 2],
    n = data$n,
    order = rank(n, ties.method = "first")
  ) |>
    tidyr::drop_na() |>
    dplyr::arrange(order)

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = geobr::read_state(
        year = 2017,
        showProgress = FALSE
      ) |>
        rutils::shush(),
      ggplot2::aes(geometry = geom),
      color = "gray75",
      linewidth = 0.1,
      fill = "white",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = data_points,
      mapping = ggplot2::aes(
        x = longitude,
        y = latitude,
        size = n,
        color = n
      ),
      alpha = alpha
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(reverse = TRUE),
      size = ggplot2::guide_legend(reverse = TRUE),
    ) +
    ggplot2::scale_size_continuous(
      range = range,
      breaks = breaks
    ) +
    ggplot2::theme(legend.key = ggplot2::element_blank())
}
