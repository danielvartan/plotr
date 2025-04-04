plot_dist <- function(
    data, #nolint
    col,
    bins = 30,
    stat = "density",
    density_line = TRUE,
    na_rm = TRUE,
    x_label = col,
    print = TRUE
  ) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(col)
  checkmate::assert_choice(col, names(data))
  checkmate::assert_number(bins, lower = 1)
  checkmate::assert_choice(stat, c("count", "density"))
  checkmate::assert_flag(density_line)
  checkmate::assert_flag(na_rm)
  prettycheck::assert_ggplot_label(x_label)
  checkmate::assert_flag(print)

  hist_plot <-
    data |>
    plot_hist(
      col = col,
      bins = bins,
      stat = stat,
      density_line = density_line,
      na_rm = na_rm,
      x_label = x_label,
      print = FALSE
    )

  qq_plot <-
    data |>
    plot_qq(
      col = col,
      na_rm = na_rm,
      print = FALSE
    )

  plot <- patchwork::wrap_plots(hist_plot, qq_plot, ncol = 2)

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
