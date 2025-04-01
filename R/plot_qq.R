plot_qq <- function(
    data, #nolint
    col,
    line_color = ifelse(
      file.exists(here::here("_brand.yml")),
      brandr::get_brand_color("primary"),
      "red"
    ),
    na_rm = TRUE,
    print = TRUE
  ) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(col)
  checkmate::assert_subset(col, names(data))
  checkmate::assert_numeric(data[[col]])
  prettycheck::assert_color(line_color)
  checkmate::assert_flag(na_rm)
  checkmate::assert_flag(print)

  data <- data |> dplyr::select(dplyr::all_of(col))
  if (isTRUE(na_rm)) data <- data |> tidyr::drop_na()

  plot <-
    data |>
    ggplot2::ggplot(ggplot2::aes(sample = !!as.symbol(col))) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(
      color = line_color,
      linewidth = 1
    ) +
    ggplot2::labs(
      x = "Theoretical quantiles (Std. normal)",
      y = "Sample quantiles"
    ) +
    ggplot2::theme(legend.position = "none")

  if (prettycheck::test_temporal(data[[col]])) {
    plot <- plot + ggplot2::scale_y_continuous(labels = format_as_hm)
  }

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
