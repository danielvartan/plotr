#' Create intervals of missing labels for `ggplot2` charts
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `skip_label()` is a utility function that creates intervals of missing
#' labels for [`ggplot2`](https://ggplot2.tidyverse.org/) charts. This is
#' useful when you have a large number of labels in a axis and you want to
#' remove some of them.
#'
#' Use this function in the `label` argument of functions like
#' [`scale_x_continuous()`][ggplot2::scale_x_continuous] or
#' [`scale_y_discrete()`][ggplot2::scale_y_discrete].
#'
#' @param x A [`character`][base::character] vector of labels
#' @param type (Optional) A [`character`][base::character] string with the
#'   type of labels to remove. Possible values are `"even"`, `"odd"` and
#'   `"one"` (Default: `"even"`).
#'
#' @return A [`character`][base::character] vector of labels with some
#'   them missing (`""`).
#'
#' @family utility functions
#' @export
#'
#' @examples
#' skip_label(x = 0:5, type = "even")
#' #> [1] "0" ""  "2" ""  "4" "" # Expected
#'
#' skip_label(x = 0:5, type = "odd")
#' #> [1] ""  "1" ""  "3" ""  "5" # Expected
#'
#' skip_label(x = 0:5, type = "one")
#' #> [1] "0" "1" ""  "3" "4" "" # Expected
skip_label <- function(x, type = "even") {
  checkmate::assert_atomic(x)
  checkmate::assert_choice(type, c("even", "odd", "one"))

  x <- x |> as.character()

  if (type == "even") x[seq_along(x) %% 2 == 0] <- ""
  if (type == "odd") x[!seq_along(x) %% 2 == 0] <- ""
  if (type == "one") x[seq_along(x) %% 3 == 0] <- ""

  x
}
