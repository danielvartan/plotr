get_closest_geobr_year <- function(year, type = "country", verbose = TRUE) {
  checkmate::assert_int(year)
  checkmate::assert_choice(type, c("municipality", "state", "country"))
  checkmate::assert_flag(verbose)

  if (type == "country") {
    years <- c(
      1872, 1900, 1911, 1920, 1933, 1940, 1950, 1960, 1970, 1980, 1991, 2000,
      2001, 2010, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020
    )
  } else if (type == "state") {
    years <- c(
      1872, 1900, 1911, 1920, 1933, 1940, 1950, 1960, 1970, 1980, 1991, 2000,
      2001, 2010, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020
    )
  } else if (type == "municipality") {
    years <- c(
      1872, 1900, 1911, 1920, 1933, 1940, 1950, 1960, 1970, 1980, 1991, 2000,
      2001, 2005, 2007, 2010, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,
      2021, 2022
    )
  }

  out <- years[which.min(abs(years - year))]

  if (isTRUE(verbose) && year != out) {
    cli::cli_alert_warning(
      paste0(
        "The closest map year to {.strong {cli::col_red(year)}} is ",
        "{.strong {out}}. Using year {.strong {out}} instead."
      )
    )
  }

  out
}
