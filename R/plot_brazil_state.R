# library(brandr) # github.com/danielvartan/brandr
# library(checkmate)
# library(dplyr)
# library(geobr)
# library(ggplot2)
# library(lubridate)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils

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
    transform = "identity", # See ?ggplot2::scale_fill_gradient
    direction = 1,
    binned = TRUE,
    breaks = ggplot2::waiver(),
    n_breaks = NULL,
    labels = ggplot2::waiver(),
    reverse = TRUE,
    limits = NULL,
    print = TRUE,
    quiet = FALSE
  ) {
  prettycheck::assert_internet()
  checkmate::assert_tibble(data)
  checkmate::assert_string(col_fill, null.ok = TRUE)
  checkmate::assert_choice(col_fill, names(data), null.ok = TRUE)
  if (!is.null(col_fill)) checkmate::assert_numeric(data[[col_fill]])
  checkmate::assert_string(col_code)
  checkmate::assert_choice(col_code, names(data))
  checkmate::assert_integerish(data[[col_code]])
  checkmate::assert_multi_class(transform, c("character", "transform"))
  checkmate::assert_choice(direction, c(-1, 1))
  checkmate::assert_flag(binned)
  checkmate::assert_multi_class(breaks, c("function", "numeric", "waiver"))
  checkmate::assert_int(n_breaks, lower = 1, null.ok = TRUE)
  checkmate::assert_multi_class(labels, c("function", "numeric", "waiver"))
  checkmate::assert_flag(reverse)
  checkmate::assert_flag(print)
  checkmate::assert_flag(quiet)

  checkmate::assert_multi_class(
    limits, c("numeric", "function"), null.ok = TRUE
  )

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  geom <- n <- unit <- NULL
  # nolint end

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
        year = 2017,
        showProgress = FALSE
      ) |>
        rutils::shush(),
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
      fill = NULL
    ) +
    brandr::scale_brand(
      aesthetics = "fill",
      scale_type = ifelse(isTRUE(binned), "binned", "continuous"),
      direction = direction,
      breaks = breaks,
      n.breaks = n_breaks,
      labels = labels,
      reverse = reverse,
      limits = limits,
      transform = transform
    )

  # if (isFALSE(binned)) {
  #   plot <-
  #     plot + ggplot2::theme(
  #       legend.ticks = ggplot2::element_line(color = "white")
  #     )
  # }

  if (isTRUE(print)) print(plot) |> rutils::shush()

  invisible(plot)
}
