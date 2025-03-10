# # TO DO:
#
# * Index the local time vector in a timeline of 2 or 3 days in length and
#   scan data density in order to choose the densest data window. Extract
#   the statistics after.
#
#   The window must have an iterative length in order to obtain the desired
#   or best density.

# # Helpers
#
# # Possible bins
# out |>
#   dplyr::transmute(
#     interval = cut(
#       !!as.symbol(col_msf_sc),
#       breaks = seq( # Two-day timeline
#         from  = 0 + (15 * 60), # 15 minutes (left buffer)
#         to = ((60 * 60 * 24) * 2) + (15 * 60), # 48 hours + 15 minutes
#         by = 30 * 60 # 30 minutes
#       ),
#       dig.lab = 10
#     )
#   ) |>
#   dplyr::pull(interval) |>
#   levels() |>
#   rutils::cut_interval_mean() |>
#   as.POSIXct(tz = "UTC")

# Move to `mctq` package
plot_chronotype <- function(
    data, #nolint
    col_msf_sc = "msf_sc",
    col_width = 0.8,
    col_border = 0.1,
    color_type = "div",
    direction = 1,
    reverse = FALSE,
    col_msf_sc_threshold = hms::parse_hm("12:00"),
    y_label = latex2exp::TeX("Local time ($MSF_{sc}$)"),
    print = TRUE
  ) {
  # See <https://ggplot2.tidyverse.org/reference/scale_brewer.html>.
  color_type_choices <- c(
    "seq", "sequential",
    "div", "diverging",
    "qual", "qualitative"
  )

  checkmate::assert_tibble(data)
  checkmate::assert_choice(col_msf_sc, names(data))
  checkmate::assert_number(col_width, lower = 0)
  checkmate::assert_number(col_border, lower = 0)
  checkmate::assert_choice(color_type, color_type_choices)
  checkmate::assert_choice(direction, c(-1, 1))
  checkmate::assert_flag(reverse)
  prettycheck::assert_hms(
    col_msf_sc_threshold,
    lower = hms::hms(0),
    upper = hms::parse_hms("23:59:59"),
    null_ok = TRUE
  )
  prettycheck::assert_ggplot_label(y_label)
  checkmate::assert_flag(print)

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  freq <- int <- interval <- label <- NULL
  ee <- me <- se <- int <- sl <- ml <- el <- NULL
  # nolint end

  if (is.null(y_label)) {
    if (hms::is_hms(data[[col_msf_sc]])) {
      y_label <- paste0("Local time (", col_msf_sc, ")")
    } else if (lubridate::is.duration(data[[col_msf_sc]])) {
      y_label <- paste0("Duration (", col_msf_sc, ")")
    } else {
      y_label <- col_msf_sc
    }
  }

  out <-
    data |>
    dplyr::select(!!as.symbol(col_msf_sc)) |>
    tidyr::drop_na() |>
    dplyr::mutate(
      !!as.symbol(col_msf_sc) :=
          !!as.symbol(col_msf_sc) |> #nolint
          lubritime::link_to_timeline(threshold = col_msf_sc_threshold) |>
          as.numeric()
    )

  aes <-
    out |>
    dplyr::group_by(
      interval = cut( # Possibles bins.
        !!as.symbol(col_msf_sc),
        breaks = seq( # Two-day timeline: 1970-01-01--1970-01-02
          from  = 0 + (15 * 60), # 15 minutes (left buffer)
          to = ((60 * 60 * 24) * 2) + (15 * 60), # 48 hours + 15 minutes
          by = 30 * 60 # 30 minutes
        ),
        dig.lab = 10
      )
    ) |>
    dplyr::summarise(freq = dplyr::n()) |>
    dplyr::mutate(
      order = seq_along(interval),
      # Transform bins in `Interval` objects.
      interval = transform_posixt_cut_levels(as.character(interval)),
      freq = (freq / sum(freq)) * 100,
      label =
        lubritime::int_mean(interval) |> #nolint
        hms::as_hms() |>
        lubritime::round_time() |>
        lubritime::fix_hms()
    )

  #            00:30           01:00           01:30
  # -----|---------------|---------------|---------------|------
  #    00:15           00:45           01:15           01:45

  fill <-
    data |>
    get_msf_sc_cutoffs(col_msf_sc = "msf_sc", pretty = FALSE)

  out <-
    aes |>
    dplyr::mutate(
      ee = lubritime::int_overlap(fill$interval[1], interval),
      me = lubritime::int_overlap(fill$interval[2], interval),
      se = lubritime::int_overlap(fill$interval[3], interval),
      int = lubritime::int_overlap(fill$interval[4], interval),
      sl = lubritime::int_overlap(fill$interval[5], interval),
      ml = lubritime::int_overlap(fill$interval[6], interval),
      el = lubritime::int_overlap(fill$interval[7], interval),
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("ee", "me", "se", "int", "sl", "ml", "el")),
        .fns = ~ hms::hms(as.numeric(.x))
      )
    ) |>
    dplyr::mutate(
      fill =
        mapply( #nolint
          FUN = (\(a, b, c, d, e, f, g) which.max(c(a, b, c, d, e, f, g))),
          ee, me, se, int, sl, ml, el
        ) |>
        factor(
          levels = seq(7),
          labels = c(
            "Extremely early", "Moderately early", "Slightly early",
            "Intermediate", "Slightly late", "Moderately late",
            "Extremely late"
          ),
          ordered = TRUE
        )
    ) |>
    dplyr::select(order, interval, freq, fill, label)

  plot <-
    out |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = freq,
        y = stats::reorder(label, -order),
        fill = fill
      )
    ) +
    ggplot2::geom_col(
      width = col_width,
      colour = "#000000",
      linewidth = col_border
    ) +
    ggplot2::scale_x_continuous(minor_breaks = NULL) +
    ggplot2::scale_y_discrete(labels = skip_label(type = "even")) +
    brandr::scale_fill_brand_d(
      color_type = color_type,
      direction = direction,
      reverse = reverse
    ) +
    ggplot2::labs(
      x = "Frequency (%)",
      y = y_label,
      fill = NULL
    )

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
