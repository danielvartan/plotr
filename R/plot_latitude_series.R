plot_latitude_series <- function(
    data, #nolint
    col = "msf_sc",
    col_latitude = "latitude",
    group_width = 1,
    linewidth = 2,
    line_color = ifelse(
      file.exists(here::here("_brand.yml")),
      brandr::get_brand_color("primary"),
      "red"
    ),
    date_breaks = "1 hour",
    minor_breaks = NULL,
    limits = NULL,
    y_label = latex2exp::TeX("$MSF_{sc}$"), # "$MSF_{sc} \\pm SEM$"
    print = TRUE
  ) {
  col_classes <- c("numeric", "integer", "POSIXt", "hms", "Duration")

  checkmate::assert_data_frame(data)
  checkmate::assert_string(col)
  checkmate::assert_subset(col, names(data))
  checkmate::assert_multi_class(data[[col]], col_classes)
  checkmate::assert_string(col_latitude)
  checkmate::assert_subset(col_latitude, names(data))
  prettycheck::assert_numeric(data[[col_latitude]])
  checkmate::assert_number(linewidth, lower = 0)
  prettycheck::assert_color(line_color)
  checkmate::assert_string(date_breaks)

  checkmate::assert_multi_class(
    minor_breaks, c("waiver", "numeric"), null.ok = TRUE
  )

  checkmate::assert_multi_class(
    limits, c("numeric", "function"), null.ok = TRUE
  )

  checkmate::assert_flag(print)

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  latitude <- NULL
  # nolint end

  if (is.null(y_label) && hms::is_hms(data[[col]])) {
    y_label <- paste0("Local time (", col, " +- SEM)")
  }

  if (is.null(y_label) && prettycheck::test_duration(data[[col]])) {
    y_label <- paste0("Duration (", col, " +- SEM)")
  }

  data <-
    data |>
    dplyr::select(dplyr::all_of(c(col_latitude, col))) |>
    dplyr::filter(dplyr::between(latitude, -34, 3)) |>
    dplyr::mutate(
      latitude = ggplot2::cut_width(latitude, group_width),
      latitude = cut_interval_mean(latitude, names = TRUE)
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(prettycheck::test_duration),
        .fns = ~ hms::hms(as.numeric(.x))
      ),
      dplyr::across(
        .cols = dplyr::where(hms::is_hms),
        .fns = function(x) {
          x |>
            lubritime::link_to_timeline(
              threshold = hms::parse_hms("12:00:00")
            )
        }
      )
    ) |>
    tidyr::drop_na() |>
    dplyr::arrange(latitude)

  plot <-
    data |>
    ggplot2::ggplot(
      ggplot2::aes(x = latitude, y = !!as.symbol(col))
    ) +
    ggplot2::geom_boxplot(
      ggplot2::aes(group = latitude),
      outlier.colour = "black",
      outlier.shape = 1,
      width = 0.75
    ) +
    ggplot2::stat_summary(
      fun = mean,
      geom = "point",
      shape = 4
    ) +
    ggplot2::geom_line(
      stat = "smooth",
      method = "lm",
      formula = y ~ x,
      linewidth = linewidth,
      color = line_color,
      alpha = 0.75
    ) +
    ggplot2::scale_x_reverse(limits = limits) +
    ggplot2::labs(
      x = "Latitude",
      y = y_label
    )

  if (inherits(data[[col]], c("POSIXt", "hms", "Duration"))) {
    plot <-
      plot +
      ggplot2::scale_y_datetime(
        date_breaks = date_breaks,
        minor_breaks = minor_breaks,
        date_labels = "%H:%M:%S",
      )
  }

  if (isTRUE(print)) print(plot) |> shush()

  invisible(plot)
}
