plot_ggally <- function(
    data, # nolint
    cols = names(data),
    mapping = NULL, # ggplot2::aes(colour = sex)
    axis_labels = "none",
    na_rm = TRUE,
    print = TRUE,
    brandr = file.exists(here::here("_brand.yml")),
    ...
  ) {
  checkmate::assert_tibble(data)
  checkmate::assert_character(cols)
  checkmate::assert_subset(cols, names(data))
  checkmate::assert_class(mapping, "uneval", null.ok = TRUE)
  checkmate::assert_choice(axis_labels, c("show", "internal", "none"))
  checkmate::assert_flag(na_rm)
  checkmate::assert_flag(print)

  if (isTRUE(brandr)) {
    color_scale <- brandr::scale_color_brand_d()
    fill_scale <- brandr::scale_fill_brand_d()
  } else {
    color_scale <- ggplot2::scale_color_discrete()
    fill_scale <- ggplot2::scale_fill_discrete()
  }

  out <-
    data |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(hms::is_hms),
        .fns = function(x) {
          x |>
            lubritime::link_to_timeline(
              threshold = hms::parse_hms("12:00:00")
            ) |>
            as.numeric()
        }
      ),
      dplyr::across(
        .cols = dplyr::where(
          ~ !is.character(.x) && !is.factor(.x) &&
            !is.numeric(.x) && !hms::is_hms(.x)
        ),
        .fns = ~ as.numeric(.x)
      )
    )

  if (isTRUE(na_rm)) out <- out |> tidyr::drop_na()

  if (is.null(mapping)) {
    plot <-
      out |>
      GGally::ggpairs(
        lower = list(continuous = "smooth"),
        axisLabels = axis_labels,
        ...
      )
  } else {
    plot <-
      out |>
      GGally::ggpairs(
        mapping = mapping,
        axisLabels = axis_labels,
        ...
      ) +
      color_scale() +
      fill_scale()
  }

  plot <-
    plot +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
