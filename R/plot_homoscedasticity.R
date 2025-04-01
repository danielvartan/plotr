plot_homoscedasticity <- function(
    model, #nolint
    data,
    col,
    line_color = ifelse(
      file.exists(here::here("_brand.yml")),
      brandr::get_brand_color("primary"),
      "red"
    ),
    x_label = col,
    y_label = latex2exp::TeX("$\\sqrt{|Standardized \\ Residuals|}$"),
    print = TRUE
  ) {
  checkmate::assert_multi_class(model, c("lm", "model_fit", "workflow"))
  checkmate::assert_data_frame(data)
  checkmate::assert_string(col)
  checkmate::assert_choice(col, names(data))
  prettycheck::assert_numeric(data[[col]])
  prettycheck::assert_color(line_color)
  prettycheck::assert_ggplot_label(x_label)
  prettycheck::assert_ggplot_label(y_label)
  checkmate::assert_flag(print)

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  .sd_resid <- value <- NULL
  # nolint end

  model <- model |> extract_fit_engine()
  data <- data |> dplyr::select(dplyr::all_of(col))

  plot <-
    dplyr::tibble(
      .sd_resid =
        model |> #nolint
        stats::rstandard() |>
        abs() |>
        sqrt()
    ) |>
    dplyr::bind_cols(data) |>
    ggplot2::ggplot(ggplot2::aes(!!as.symbol(col), .sd_resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(color = line_color) +
    ggplot2::labs(x = x_label, y = y_label)

  if (isTRUE(print)) print(plot) |> shush()

  invisible(plot)
}
