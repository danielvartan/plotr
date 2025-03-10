plot_ggcorrplot <- function(
    data, #nolint
    cols,
    na_rm = TRUE,
    label = TRUE,
    hc_order = TRUE
  ) {
  checkmate::assert_tibble(data)
  checkmate::assert_character(cols)
  checkmate::assert_subset(cols, names(data))
  checkmate::assert_flag(na_rm)
  checkmate::assert_flag(label)
  checkmate::assert_flag(hc_order)

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
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = ~ as.numeric(.x)
      )
    )

  if (isTRUE(na_rm)) out <- out |> tidyr::drop_na(dplyr::all_of(cols))

  corr <- stats::cor(out, use = "complete.obs")
  p_matrix <- ggcorrplot::cor_pmat(out)
  # round(ggcorrplot::cor_pmat(out), 5)

  plot <-
    ggcorrplot::ggcorrplot(
      corr = corr,
      type = "lower",
      ggtheme = ggplot2::theme_get(),
      outline.color = "gray",
      hc.order = hc_order,
      lab = label,
      p.mat = p_matrix
    )

  print(plot)
  invisible(plot)
}
