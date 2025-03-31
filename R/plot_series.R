# # TO DO
#
# - Add option to cut-offs like Roenneberg et al. (2007).
#    See cut-offs above 60.
# - Create `plot_msf_sc series()`.

# Move to `mctq` package
plot_series <- function(
    data, #nolint
    col_x = "age",
    col_y = "msf_sc",
    col_group = "sex",
    linewidth = 2,
    boundary = 0.5,
    point_size = 1,
    error_bar = TRUE,
    error_bar_width = 0.5,
    error_bar_linewidth = 0.5,
    date_breaks = "15 mins",
    date_minor_breaks = NULL,
    reverse = FALSE,
    change_sign = FALSE,
    x_label = "Age",
    y_label = latex2exp::TeX("$MSF_{sc}$"), # "$MSF_{sc} \\pm SEM$"
    color_label = "Sex",
    print = TRUE,
    brandr = file.exists(here::here("_brand.yml")),
    ...
  ) {
  col_classes <- c("numeric", "integer", "POSIXt", "hms", "Duration")

  checkmate::assert_tibble(data)
  checkmate::assert_string(col_x)
  checkmate::assert_subset(col_x, names(data))
  checkmate::assert_multi_class(data[[col_x]], col_classes)
  checkmate::assert_string(col_y)
  checkmate::assert_subset(col_y, names(data))
  checkmate::assert_multi_class(data[[col_y]], col_classes)
  checkmate::assert_string(col_group)
  checkmate::assert_subset(col_group, names(data))
  checkmate::assert_factor(data[[col_group]])
  checkmate::assert_number(linewidth)
  checkmate::assert_number(boundary)
  checkmate::assert_number(point_size)
  checkmate::assert_flag(error_bar)
  checkmate::assert_number(error_bar_width)
  checkmate::assert_number(error_bar_linewidth)
  checkmate::assert_string(date_breaks)
  checkmate::assert_flag(reverse)
  checkmate::assert_flag(change_sign)
  prettycheck::assert_ggplot_label(x_label)
  prettycheck::assert_ggplot_label(y_label)
  prettycheck::assert_ggplot_label(color_label)
  checkmate::assert_flag(print)
  checkmate::assert_multi_class(
    date_minor_breaks, c("waiver", "numeric"), null.ok = TRUE
  )

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  std_error <- NULL
  # nolint end

  if (isTRUE(brandr)) {
    color_scale <- brandr::scale_color_brand_d(...)
  } else {
    color_scale <- ggplot2::scale_color_discrete(...)
  }

  if (y_label == col_y && hms::is_hms(data[[col_y]])) {
    y_label <- paste0("Local time (", col_y, " +- SEM)")
  } else if (y_label == col_y && prettycheck::test_duration(data[[col_y]])) {
    y_label <- paste0("Duration (", col_y, " +- SEM)")
  }

  data <-
    data |>
    dplyr::select(dplyr::all_of(c(col_x, col_y, col_group))) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(prettycheck::test_duration),
      .fns = ~ hms::hms(as.numeric(.x))
    )) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(hms::is_hms),
      .fns = function(x) {
        x |>
          lubritime::link_to_timeline(
            threshold = hms::parse_hms("12:00:00")
          )
      }
    )) |>
    dplyr::mutate(
      !!as.symbol(col_x) := ggplot2::cut_width(
        !!as.symbol(col_x), width = 1, boundary = boundary, closed = "right"
      )
    )

  if (isTRUE(reverse)) {
    data <-
      data |>
      dplyr::mutate(
        !!as.symbol(col_x) :=
          `levels<-`(
            !!as.symbol(col_x),
            levels(!!as.symbol(col_x)) |>
              rutils::cut_interval_mean(round = TRUE) |>
              change_sign(change_sign)
          ) |>
          forcats::fct_rev()
      )
  } else {
    data <-
      data |>
      dplyr::mutate(
        !!as.symbol(col_x) :=
          `levels<-`(
            !!as.symbol(col_x),
            levels(!!as.symbol(col_x)) |>
              rutils::cut_interval_mean(round = TRUE) |>
              change_sign(change_sign)
          )
      )
  }

  if (isTRUE(reverse)) {
    data <-
      data |>
      dplyr::mutate(
        !!as.symbol(col_x) :=
          `levels<-`(
            !!as.symbol(col_x),
            levels(!!as.symbol(col_x)) |> forcats::fct_rev()
          )
      )
  }

  data_by_col_x <-
    data |>
    dplyr::summarize(
      std_error = rutils::std_error(!!as.symbol(col_y)),
      !!as.symbol(col_y) := mean(!!as.symbol(col_y), na.rm = TRUE),
      .by = !!as.symbol(col_x)
    ) |>
    rutils::shush() |>
    tidyr::drop_na()

  data_by_col_x_and_col_group <-
    data |>
    dplyr::summarize(
      std_error = rutils::std_error(!!as.symbol(col_y)),
      !!as.symbol(col_y) := mean(!!as.symbol(col_y), na.rm = TRUE),
      .by = c(!!as.symbol(col_x), !!as.symbol(col_group))
    ) |>
    rutils::shush() |>
    tidyr::drop_na()

  plot <-
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(x = !!as.symbol(col_x), y = !!as.symbol(col_y), group = 1),
      data = data_by_col_x,
      linewidth = linewidth,
      color = "gray"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = !!as.symbol(col_x),
        y = !!as.symbol(col_y),
        color = !!as.symbol(col_group)
      ),
      data = data_by_col_x_and_col_group,
      size = point_size
    ) +
    ggplot2::scale_x_discrete(breaks = fix_label_decimals) +
    color_scale +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      color = color_label
    )

  if (inherits(data[[col_x]], c("POSIXt", "hms", "Duration"))) {
    plot <-
      plot +
      ggplot2::scale_x_datetime(
        date_breaks = date_breaks,
        minor_breaks = date_minor_breaks,
        date_labels = "%H:%M:%S",
      )
  }

  if (inherits(data[[col_y]], c("POSIXt", "hms", "Duration"))) {
    plot <-
      plot +
      ggplot2::scale_y_datetime(
        date_breaks = date_breaks,
        minor_breaks = date_minor_breaks,
        date_labels = "%H:%M:%S",
      )
  }

  if (isTRUE(error_bar)) {
    plot <-
      plot +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          x = !!as.symbol(col_x),
          y = !!as.symbol(col_y),
          ymin = !!as.symbol(col_y) - std_error,
          ymax = !!as.symbol(col_y) + std_error,
          color = !!as.symbol(col_group)
        ),
        data = data_by_col_x_and_col_group,
        show.legend = FALSE,
        inherit.aes = FALSE,
        width = error_bar_width,
        linewidth = error_bar_linewidth
      )
  }

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
