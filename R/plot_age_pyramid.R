# library(apyramid)
# library(brandr) # github.com/danielvartan/brandr
# library(checkmate)
# library(dplyr)
library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(tidyr)

plot_age_pyramid <- function(
    data, #nolint
    interval = 10,
    breaks = NULL,
    na_rm = TRUE,
    print = TRUE
  ) {
  checkmate::assert_tibble(data)
  checkmate::assert_subset(c("sex", "age"), names(data))
  checkmate::assert_number(interval)
  prettycheck::assert_numeric(breaks, null_ok = TRUE)
  prettycheck::assert_pick(interval, breaks, min_pick = 1)
  checkmate::assert_flag(na_rm)
  checkmate::assert_flag(print)

  # R CMD Check variable bindings fix (See: https://bit.ly/3z24hbU)
  # nolint start
  sex <- age <- NULL
  # nolint end

  if (is.null(breaks)) breaks <- pretty(data$age, n = interval)

  plot <-
    data |>
    dplyr::select(sex, age) |>
    dplyr::mutate(
      age_group = cut(
        age,
        breaks = breaks,
        right = FALSE,
        include.lowest = TRUE
      )
    ) |>
    tidyr::drop_na() |>
    apyramid::age_pyramid(
      age_group = "age_group",
      split = "sex",
      na.rm = na_rm
    ) +
    brandr::scale_fill_brand_d() +
    ggplot2::labs(
      x = "Frequency",
      y = "Age group",
      fill = "Sex"
    ) |>
    rutils::shush()

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
