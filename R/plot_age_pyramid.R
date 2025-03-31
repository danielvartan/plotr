plot_age_pyramid <- function(
    data, #nolint
    interval = 10,
    breaks = NULL,
    na_rm = TRUE,
    print = TRUE,
    brandr = file.exists(here::here("_brand.yml")),
    ...
  ) {
  checkmate::assert_data_frame(data)
  checkmate::assert_subset(c("sex", "age"), names(data))
  checkmate::assert_number(interval)
  prettycheck::assert_numeric(breaks, null_ok = TRUE)
  prettycheck::assert_pick(interval, breaks, min_pick = 1)
  checkmate::assert_flag(na_rm)
  checkmate::assert_flag(print)
  checkmate::assert_flag(brandr)

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  sex <- age <- NULL
  # nolint end

  if (isTRUE(brandr)) {
    color_scale <- brandr::scale_fill_brand_d(...)
  } else {
    color_scale <- ggplot2::scale_fill_discrete(...)
  }

  if (is.null(breaks)) breaks <- pretty(data$age, n = interval)

  plot <-
    data |>
    dplyr::select(sex, age) |>
    dplyr::mutate(
      age_group = cut(
        age,
        breaks = breaks,
        right = FALSE,
        include.lowest = TRUE
      )
    ) |>
    tidyr::drop_na() |>
    apyramid::age_pyramid(
      age_group = "age_group",
      split = "sex",
      na.rm = na_rm
    ) +
    color_scale +
    ggplot2::labs(
      x = "Frequency",
      y = "Age group",
      fill = "Sex"
    ) |>
    shush()

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
