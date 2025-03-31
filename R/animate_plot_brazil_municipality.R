animate_plot_brazil_municipality <- function( #nolint
    data, # nolint
    col_fill = NULL,
    col_group = "year",
    group_label = "Year",
    comparable_areas = TRUE, # See ?geobr::read_comparable_areas
    suffix = NULL,
    width = 1344,
    height = 960,
    dpi = 150,
    text_size = 20,
    ...
) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(col_group, null.ok = TRUE)
  checkmate::assert_choice(col_group, names(data), null.ok = TRUE)
  checkmate::assert_string(group_label)
  checkmate::assert_flag(comparable_areas)
  checkmate::assert_string(suffix, null.ok = TRUE)
  checkmate::assert_number(width, lower = 1)
  checkmate::assert_number(height, lower = 1)
  checkmate::assert_number(dpi, lower = 1)
  checkmate::assert_number(text_size, lower = 1)

  if (!checkmate::test_directory_exists(here::here("images"))) {
    dir.create(here::here("images"))
  }

  group_unique_values <-
    data |>
    dplyr::pull(col_group) |>
    unique() |>
    sort()

  files <- character()

  for (i in group_unique_values) {
    file <- file.path(
      here::here("images"),
      paste0(
        stringr::str_replace_all(col_fill, "_", "-"),
        ifelse(is.null(suffix), "", paste0("-", suffix)),
        "-",
        i,
        ".png"
      )
    )

    i_data <- data |> dplyr::filter(!!as.symbol(col_group) == i)

    plot <-
      do.call(
        "plot_brazil_municipality",
        c(
          list(
            data = i_data,
            col_fill = col_fill,
            comparable_areas = comparable_areas,
            print = FALSE,
            quiet = TRUE
          ),
          list(...)
        )
      ) +
      ggplot2::labs(title = paste0(group_label, ": ", i)) +
      ggplot2::theme(text = ggplot2::element_text(size = 20))

    ggplot2::ggsave(
      filename = file,
      plot = plot,
      device = ragg::agg_png,
      width = width,
      height = height,
      units = "px",
      dpi = dpi,
    )

    files <- files |> append(file)
  }

  animation <-
    files |>
    lapply(magick::image_read) |>
    magick::image_join() |>
    magick::image_animate(fps = 1)

  animation |>
    magick::image_write(
      file.path(
        here::here("images"),
        paste0(
          stringr::str_replace_all(col_fill, "_", "-"),
          ifelse(is.null(suffix), "", paste0("-", suffix)),
          "-",
          "animation",
          ".gif"
        )
      )
    )

  animation
}
