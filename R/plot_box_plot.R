plot_box_plot <- function(
    data, #nolint
    col,
    label = col,
    jitter = FALSE,
    print = TRUE,
    brandr = file.exists(here::here("_brand.yml")),
    breaks = label,
    ...
  ) {
  checkmate::assert_tibble(data)
  checkmate::assert_character(col)
  checkmate::assert_subset(col, names(data))
  checkmate::assert_flag(jitter)
  checkmate::assert_character(label)
  checkmate::assert_flag(print)
  checkmate::assert_flag(brandr)
  checkmate::assert_character(breaks)

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  name <- value <- NULL
  # nolint end

  for (i in col) {
    if (prettycheck::test_temporal(data[[i]])) {
      if (hms::is_hms(data[[i]])) {
        data[[i]] <-
          data[[i]] |>
          lubritime::link_to_timeline() |>
          as.numeric()
      } else {
        data[[i]] <- data[[i]] |> lubritime::extract_seconds()
      }
    } else {
      prettycheck::assert_numeric(data[[i]])
    }
  }

  names(col) <- label

  data <-
    data |>
    dplyr::select(dplyr::all_of(col)) |>
    tidyr::pivot_longer(dplyr::all_of(unname(col))) |>
    dplyr::mutate(
      name = factor(
        name,
        levels = col |> rev() |> names(),
        labels = col |> rev() |> names()
      )
    ) |>
    tidyr::drop_na(value)

  plot <-
    { #nolint
      if (length(col) == 1) {
        ggplot2::ggplot(
          data = data,
          ggplot2::aes(
            x = name,
            y = value
          )
        )
      } else {
        ggplot2::ggplot(
          data = data,
          ggplot2::aes(
            x = name,
            y = value,
            fill = name
          )
        )
      }
    } + #nolint
    ggplot2::geom_boxplot(
      outlier.colour = ifelse(
        isTRUE(brandr),
        brandr::get_brand_color("primary"),
        "red"
      ),
      outlier.shape = 1,
      width = 0.75
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Variable",
      y = "Value",
      fill = NULL
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )

  if (isTRUE(jitter)) {
    plot <-
      plot +
      ggplot2::geom_jitter(
        width = 0.3,
        alpha = 0.1,
        color = "black",
        size = 0.5
      )
  }

  if (!length(col) == 1) {
    if (isTRUE(brandr)) {
      plot <- plot + brandr::scale_fill_brand_d(breaks = breaks, ...)
    } else {
      ggplot2::scale_fill_discrete(...)
    }
  }

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
