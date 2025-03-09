# library(hms)
# library(lubritime) # github.com/danielvartan/lubritime

format_as_hm <- function(x, type = NULL) {
  classes <- c("numeric", "Duration", "difftime", "hms", "POSIXct",
               "POSIXlt", "Interval")

  checkmate::assert_multi_class(x, classes)

  if (hms::is_hms(x)) {
    out <- lubritime:::fix_hms(x)
  } else {
    out <-
      x |>
      lubritime::cycle_time(lubridate::ddays()) |>
      hms::as_hms() |>
      substr(1, 5)
  }

  if (!is.null(type)) out <- out |> skip_label(type = type)

  out
}
