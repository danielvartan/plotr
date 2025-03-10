# # Helpers
#
# ## Run function until this code block
#
# out |>
#   dplyr::group_by(
#     interval = cut( # Bins by quantiles.
#       !!as.symbol(col_msf_sc),
#       breaks =
#         !!as.symbol(col_msf_sc) |>
#         rutils::drop_na() |>
#         lubritime::link_to_timeline() |>
#         as.numeric() |>
#         stats::quantile(probs),
#       dig.lab = 10,
#       include.lowest = TRUE
#     )
#   ) |>
#   dplyr::pull(interval) |>
#   unique() |>
#   rutils::cut_interval_mean() |>
#   as.POSIXct(tz = "UTC") |>
#   sort()

# Move to `mctq` package
get_msf_sc_cutoffs <- function(
    data, #nolint
    col_msf_sc = "msf_sc",
    probs = NULL,
    pretty = TRUE
  ) {
  checkmate::assert_tibble(data)
  checkmate::assert_string(col_msf_sc)
  checkmate::assert_choice(col_msf_sc, names(data))
  checkmate::assert_numeric(probs, null.ok = TRUE)
  checkmate::assert_flag(pretty)

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  freq <- interval <- NULL
  # nolint end

  out <-
    data |>
    dplyr::select(!!as.symbol(col_msf_sc)) |>
    tidyr::drop_na() |>
    dplyr::mutate(
      !!as.symbol(col_msf_sc) :=
        lubritime::link_to_timeline(!!as.symbol(col_msf_sc)) |>
        as.numeric()
    )

  if (is.null(probs)) {
    ## See `ggplot2::cut_interval()`.
    # Quantiles (e.g., "Extremely early" is 0 to `1 / 3 / 3``) (7 slots).
    probs <- c(
      0 / 3 / 3,
      1 / 3 / 3,
      2 / 3 / 3,
      3 / 3 / 3,
      6 / 3 / 3,
      7 / 3 / 3,
      8 / 3 / 3,
      9 / 3 / 3
    )
  }

  out <-
    out |>
    dplyr::group_by(
      interval = cut(
        !!as.symbol(col_msf_sc),
        breaks =
          rutils::drop_na(!!as.symbol(col_msf_sc)) |>
          stats::quantile(probs),
        dig.lab = 10,
        include.lowest = TRUE
      )
    ) |>
    dplyr::summarise(freq = dplyr::n()) |>
    dplyr::mutate(
      order = seq_along(interval),
      interval = transform_posixt_cut_levels(as.character(interval)),
      freq = (freq / sum(freq)) * 100,
      label = c(
        "Extremely early", "Moderately early", "Slightly early",
        "Intermediate", "Slightly late", "Moderately late", "Extremely late"
      )
    )

  if (isTRUE(pretty)) {
    out |>
      dplyr::mutate(
        interval = paste0(
          interval |>
            lubridate::int_start() |>
            hms::as_hms() |>
            lubritime::round_time(),
          "-",
          interval |>
            lubridate::int_end() |>
            hms::as_hms() |>
            lubritime::round_time()
        )
      )
  } else {
    out
  }
}
