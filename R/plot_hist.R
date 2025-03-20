plot_hist <- function(
    data, #nolint
    col,
    name = col,
    bins = 30,
    stat = "density",
    density_line = TRUE,
    density_line_color = ifelse(
      file.exists(here::here("_brand.yml")),
      brandr::get_brand_color("primary"),
      "red"
    ),
    na_rm = TRUE,
    x_label = name,
    y_label = ifelse(stat == "count", "Frequency", "Density"),
    print = TRUE
  ) {
  checkmate::assert_tibble(data)
  checkmate::assert_string(col)
  checkmate::assert_choice(col, names(data))
  prettycheck::assert_numeric(data[[col]])
  checkmate::assert_string(name)
  checkmate::assert_number(bins, lower = 1)
  checkmate::assert_choice(stat, c("count", "density"))
  checkmate::assert_flag(density_line)
  prettycheck::assert_color(density_line_color)
  checkmate::assert_flag(na_rm)
  checkmate::assert_flag(print)
  prettycheck::assert_ggplot_label(x_label)
  prettycheck::assert_ggplot_label(y_label)

  data <- data |> dplyr::select(dplyr::all_of(col))

  if (isTRUE(na_rm)) data <- data |> tidyr::drop_na()

  plot <-
    data |>
    ggplot2::ggplot(
      ggplot2::aes(x = !!as.symbol(col))
    ) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(!!as.symbol(stat))),
      bins = 30,
      color = "white"
    ) +
    ggplot2::labs(
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme(legend.position = "none")

  if (stat == "density" && isTRUE(density_line)) {
    plot <- plot + ggplot2::geom_density(
      color = density_line_color,
      linewidth = 1
    )
  }

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
