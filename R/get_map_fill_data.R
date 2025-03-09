# library(checkmate)
# library(cli)
# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rlang)
# library(rutils) # github.com/danielvartan/rutils
# library(tidyr)

get_map_fill_data <- function(
    data, #nolint
    col_fill = NULL,
    col_code,
    name_col_value = "n",
    name_col_ref = col_code,
    quiet = FALSE
  ) {
  checkmate::assert_tibble(data)
  checkmate::assert_string(col_fill, null.ok = TRUE)
  checkmate::assert_choice(col_fill, names(data), null.ok = TRUE)
  checkmate::assert_string(col_code)
  checkmate::assert_choice(col_code, names(data))
  checkmate::assert_string(name_col_value)
  checkmate::assert_string(name_col_ref)
  checkmate::assert_flag(quiet)

  if (is.null(col_fill)) {
    data |>
      dplyr::rename(!!as.symbol(name_col_ref) := !!as.symbol(col_code)) |>
      dplyr::select(!!as.symbol(name_col_ref)) |>
      tidyr::drop_na() |>
      dplyr::count(!!as.symbol(name_col_ref))
  } else {
    prettycheck::assert_numeric(data[[col_fill]], null_ok = TRUE)

    out <-
      data |>
      dplyr::rename(
        !!as.symbol(name_col_ref) := !!as.symbol(col_code),
        !!as.symbol(name_col_value) := !!as.symbol(col_fill)
      ) |>
      dplyr::select(!!as.symbol(name_col_ref), !!as.symbol(name_col_value)) |>
      tidyr::drop_na()

    if (any(duplicated(out[[name_col_ref]]))) {
      cli::cli_alert_warning(
        paste0(
          "There are duplicated values in ",
          "{.strong {cli::col_red(col_code)}}. ",
          "{.strong {cli::col_blue(col_fill)}} will be aggregated ",
          "using the mean."
        )
      ) |>
        rutils::shush(quiet)

      out |>
        dplyr::summarise(
          !!as.symbol(name_col_value) := mean(!!as.symbol(name_col_value)),
          .by = !!as.symbol(name_col_ref)
        )
    } else {
      out
    }
  }
}
