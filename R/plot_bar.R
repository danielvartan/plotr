plot_bar <- function(
    data, #nolint
    col,
    na_rm = TRUE,
    x_label = "Frequency",
    y_label = col,
    print = TRUE
  ) {
  col_class_options <- c("character", "factor")

  checkmate::assert_data_frame(data)
  checkmate::assert_string(col)
  checkmate::assert_choice(col, names(data))
  checkmate::assert_multi_class(data[[col]], col_class_options)
  checkmate::assert_flag(na_rm)
  prettycheck::assert_ggplot_label(x_label)
  prettycheck::assert_ggplot_label(y_label)
  checkmate::assert_flag(print)

  data <-
    data |>
    dplyr::select(dplyr::all_of(col)) |>
    dplyr::mutate(!!as.symbol(col) := as.factor(!!as.symbol(col)))

  levels <- data |> dplyr::pull(col) |> levels()

  if (isTRUE(na_rm)) data <- data |> tidyr::drop_na()

  plot <-
    data |>
    ggplot2::ggplot(
      ggplot2::aes(y = !!as.symbol(col))
    ) +
    ggplot2::geom_bar() +
    ggplot2::labs(
      x = x_label,
      y = y_label
    ) +
    ggplot2::scale_y_discrete(limits = rev(levels)) +
    ggplot2::theme(legend.position = "none")

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
