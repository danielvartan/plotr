# @example
# \dontrun{
#   library(dplyr)
#   library(janitor)
#   library(sidrar)
#
#   # 2020 Brazil's GDP by municipality
#   "/t/5938/n6/all/v/37/p/2020/d/v37%200" %>%
#      sidrar::get_sidra(api = "/t/5938/n6/all/v/37/p/2020/d/v37%200") |>
#      dplyr::as_tibble() |>
#      janitor::clean_names() |>
#      dplyr::transmute(
#        n = valor,
#        municipality_code = as.integer(municipio_codigo)
#      ) |>
#      plot_brazil_municipality(
#        col_fill = "n",
#        col_code = "municipality_code",
#        year = 2020,
#        comparable_areas = TRUE,
#        type = "viridis",
#        transform = "log10"
#      )
# }
#
# @noRd

plot_brazil_municipality <- function(
    data, #nolint
    col_fill = NULL,
    col_code = "municipality_code",
    year = 2022,
    comparable_areas = FALSE, # See ?geobr::read_comparable_areas
    alpha = 1,
    range = c(0, 10),
    zero_na = FALSE,
    point = FALSE,
    print = TRUE,
    quiet = FALSE,
    brandr = file.exists(here::here("_brand.yml")),
    scale_type = "binned",
    na.value = "white", #nolint
    ...
  ) {
  prettycheck::assert_internet()
  checkmate::assert_data_frame(data)
  checkmate::assert_string(col_fill, null.ok = TRUE)
  checkmate::assert_choice(col_fill, names(data), null.ok = TRUE)
  if (!is.null(col_fill)) prettycheck::assert_numeric(data[[col_fill]])
  checkmate::assert_string(col_code)
  checkmate::assert_choice(col_code, names(data))
  checkmate::assert_integerish(data[[col_code]])
  checkmate::assert_int(year)
  checkmate::assert_flag(comparable_areas)
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_integerish(range, len = 2)
  checkmate::assert_flag(zero_na)
  checkmate::assert_flag(point)
  checkmate::assert_flag(print)
  checkmate::assert_flag(quiet)
  checkmate::assert_flag(brandr)
  checkmate::assert_string(scale_type)
  prettycheck::assert_color(na.value)

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  . <- code_muni <- geom <- n <- unit <- NULL
  # nolint end

  if (isTRUE(brandr)) {
    color_scale <- brandr::scale_brand(
      aesthetics = ifelse(isTRUE(point), "color", "fill"),
      scale_type = scale_type,
      na.value = na.value,
      ...
    )
  } else {
    if (isTRUE(point)) {
      color_scale <- ggplot2::scale_color_binned(na.value = na.value, ...)
    } else
      color_scale <- ggplot2::scale_fill_binned(na.value = na.value, ...)
  }

  if (isTRUE(comparable_areas)) {
    geom_data <-
      geobr::read_comparable_areas(showProgress = FALSE) |>
      shush()

    out <-
      data |>
      dplyr::mutate(
        amc_code = purrr::map_int(
          as.character(!!as.symbol(col_code)),
          function(x) {
            geom_data$list_code_muni_2010 |>
              stringr::str_detect(x) %>%
              magrittr::extract(geom_data$code_amc, .) |>
              as.integer() %>%
              ifelse(length(.) == 0, NA_integer_, .)
          }
        )
      )

    col_code <- "amc_code"
  } else {
    geom_data <-
      geobr::read_municipality(
        year = get_closest_geobr_year(year),
        showProgress = FALSE
      ) |>
      shush()

    if (nchar(data[[col_code]][1]) == 6) {
      geom_data <-
        geom_data |>
        dplyr::mutate(
          code_muni =
            code_muni |> #nolint
            stringr::str_sub(end = -2) |>
            as.numeric()
        )
    }

    out <- data
  }

  out <-
    out |>
    orbis::get_map_fill_data(
      col_fill = col_fill,
      col_code = col_code,
      name_col_ref = ifelse(isTRUE(comparable_areas), "code_amc", "code_muni"),
      quiet = ifelse(isTRUE(comparable_areas), TRUE, quiet)
    ) |>
    dplyr::right_join(
      y = geom_data,
      by = ifelse(isTRUE(comparable_areas), "code_amc", "code_muni")
    )

  if (isTRUE(zero_na)) {
    out <- out |> dplyr::mutate(n = ifelse(is.na(n), 0, n))
  }

  if (isTRUE(point)) {
    if ("breaks" %in% names(list(...))) {
      breaks <- list(...)[["breaks"]]
    } else {
      breaks <- ggplot2::waiver()
    }

    plot <-
      out |>
      plot_brazil_municipality_point(
        alpha = alpha,
        breaks = breaks,
        range = range,
      )
  } else {
    plot <-
      out |>
      ggplot2::ggplot() +
      ggplot2::geom_sf(
        ggplot2::aes(geometry = geom, fill = n),
        color = "black",
        linewidth = 0.02
      ) +
      ggplot2::geom_sf(
        inherit.aes = FALSE,
        ggplot2::aes(geometry = geom),
        data = geobr::read_country(
          year = get_closest_geobr_year(year, verbose = FALSE),
          showProgress = FALSE
        ) |>
          shush(),
        color = "black",
        fill = NA,
        linewidth = 0.05
      )
  }

  plot <-
    plot +
    ggspatial::annotation_scale(
      location = "br",
      style = "tick",
      width_hint = 0.25,
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
      fill = NULL,
      color = NULL,
      size = NULL
    ) +
    color_scale

  if (isTRUE(print)) print(plot) |> shush()

  invisible(plot)
}

plot_brazil_municipality_point <- function(
    data, #nolint
    year = 2020,
    alpha = 0.7,
    breaks = ggplot2::waiver(),
    range = c(0, 10)
  ) {
  prettycheck::assert_internet()
  checkmate::assert_data_frame(data)
  checkmate::assert_int(year)
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_multi_class(breaks, c("numeric", "waiver"))
  checkmate::assert_integerish(range, len = 2)

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  geom <- latitude <- longitude <- n <- unit <- NULL
  # nolint end

  data_points <-
    data |>
    sf::st_as_sf() |>
    sf::st_centroid() |>
    shush()

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
        year = year,
        showProgress = FALSE
      ) |>
        shush(),
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
