# Borrowed from `rutils`: github.com/danielvartan/rutils
change_sign <- function(x, flag = TRUE) {
  prettycheck::assert_numeric(x)
  checkmate::assert_flag(flag)

  if (isTRUE(flag)) {
    x * (-1)
  } else {
    x
  }
}

# Borrowed from `rutils`: github.com/danielvartan/rutils
cut_interval_mean <- function(x, round = FALSE, names = FALSE) {
  checkmate::assert_multi_class(x, c("character", "factor"))
  checkmate::assert_character(as.character(x), pattern = "^\\[|^\\(")
  checkmate::assert_flag(round)
  checkmate::assert_flag(names)

  if (is.factor(x)) x <- as.character(x)

  out <-
    x |>
    stringr::str_remove_all("\\(|\\[|\\)|\\]") |>
    stringr::str_split(",") |>
    lapply(as.numeric) |>
    lapply(mean) |>
    unlist()

  if (isTRUE(round)) out <- round(out)
  if (isTRUE(names)) names(out) <- x

  out
}

# Borrowed from `rutils`: github.com/danielvartan/rutils
drop_na <- function(x) {
  checkmate::assert_atomic(x)

  x[which(!is.na(x))]
}

# Borrowed from `rutils`: github.com/danielvartan/rutils
grab_fun_par <- function() {
  args_names <- ls(envir = parent.frame(), all.names = TRUE, sorted = FALSE)

  if ("..." %in% args_names) {
    dots <- eval(quote(list(...)), envir = parent.frame())
  } else {
    dots <- list()
  }

  args_names <- setdiff(args_names, "...") |> lapply(as.name)
  names(args_names) <- setdiff(args_names, "...")

  if (!length(args_names) == 0) {
    not_dots <- lapply(args_names, eval, envir = parent.frame())
  } else {
    not_dots <- list()
  }

  out <- c(not_dots, dots)

  out[names(out) != ""]
}

# Borrowed from `rutils`: github.com/danielvartan/rutils
shush <- function(x, quiet = TRUE) {
  if (isTRUE(quiet)) {
    suppressMessages(suppressWarnings(x))
  } else {
    x
  }
}

# Borrowed from `rutils`: github.com/danielvartan/rutils
std_error <- function(x) {
  checkmate::assert_numeric(x)

  stats::sd(x, na.rm = TRUE) / sqrt(length(x))
}
