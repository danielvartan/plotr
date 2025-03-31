# Colors based on a data visualization found in @roenneberg2019b.
scale_mctq_rainbow <- function(
    aesthetics = "color", #nolint
    scale_type = "c",
    color_type = "seq",
    alpha = NULL,
    direction = 1,
    na.value = NA, #nolint
    reverse = FALSE,
    ...
  ) {
  # https://ggplot2-book.org/extensions#sec-new-scales to learn more.
  # See https://ggplot2.tidyverse.org/reference/scale_viridis.html
  scale_type_choices <- c(
    "d", "discrete",
    "c", "continuous",
    "b", "binned"
  )

  # See https://ggplot2.tidyverse.org/reference/scale_brewer.html
  color_type_choices <- c(
    "seq", "sequential",
    "qual", "qualitative"
  )

  checkmate::assert_string(aesthetics)
  checkmate::assert_choice(scale_type, scale_type_choices)
  checkmate::assert_choice(color_type, color_type_choices)
  checkmate::assert_choice(direction, c(-1, 1))
  checkmate::assert_number(alpha, lower = 0, upper = 1, null.ok = TRUE)
  checkmate::assert_string(na.value, na.ok = TRUE)
  if (!is.na(na.value)) prettycheck::assert_color(na.value)
  checkmate::assert_flag(reverse)

  if (color_type %in% c("seq", "sequential")) {
    palette <- \(x) color_mctq_rainbow_sequential( #nolint
      x, alpha = alpha, direction = direction
    )
  } else if (color_type %in% c("qual", "qualitative")) {
    palette <- \(x) color_mctq_rainbow_qualitative( #nolint
      x, alpha = alpha, direction = direction
    )
  }

  if (scale_type %in% c("d", "discrete")) {
    scale_fun <- ggplot2::discrete_scale
    guide <- ggplot2::guide_legend(reverse = reverse)
  } else if (scale_type %in% c("c", "continuous")) {
    scale_fun <- ggplot2::continuous_scale
    guide <- ggplot2::guide_colourbar(reverse = reverse)
  } else if (scale_type %in% c("b", "binned")) {
    scale_fun <- ggplot2::binned_scale
    guide <- ggplot2::guide_colorsteps(reverse = reverse)
  }

  arg_list <- list(
    aesthetics = aesthetics,
    palette = palette,
    na.value = na.value,
    guide = guide
  )

  do.call(
    what = scale_fun,
    args = c(
      list(...)[names(list(...)) %in% names(formals(scale_fun))],
      arg_list
    ) |>
      brandr:::clean_arg_list()
  )
}

scale_color_mctq_rainbow_d <- function(
    aesthetics = "color", #nolint
    scale_type = "d",
    color_type = "qual",
    alpha = NULL,
    direction = 1,
    na.value = NA, #nolint
    reverse = FALSE,
    ...
  ) {
  do.call("scale_mctq_rainbow", grab_fun_par())
}

scale_color_mctq_rainbow_c <- function(
    aesthetics = "color", #nolint
    scale_type = "c",
    color_type = "seq",
    alpha = NULL,
    direction = 1,
    na.value = NA, #nolint
    reverse = FALSE,
    ...
  ) {
  do.call("scale_mctq_rainbow", grab_fun_par())
}

scale_color_mctq_rainbow_b <- function(
    aesthetics = "color", #nolint
    scale_type = "b",
    color_type = "seq",
    alpha = NULL,
    direction = 1,
    na.value = NA, #nolint
    reverse = FALSE,
    ...
  ) {
  do.call("scale_mctq_rainbow", grab_fun_par())
}

scale_colour_mctq_rainbow_d <- scale_color_mctq_rainbow_d
scale_colour_mctq_rainbow_c <- scale_color_mctq_rainbow_c
scale_colour_mctq_rainbow_b <- scale_color_mctq_rainbow_b

scale_fill_mctq_rainbow_d <- function(
    aesthetics = "fill", #nolint
    scale_type = "d",
    color_type = "qual",
    alpha = NULL,
    direction = 1,
    na.value = NA, #nolint
    reverse = FALSE,
    ...
  ) {
  do.call("scale_mctq_rainbow", grab_fun_par())
}

scale_fill_mctq_rainbow_c <- function(
    aesthetics = "fill", #nolint
    scale_type = "c",
    color_type = "seq",
    alpha = NULL,
    direction = 1,
    na.value = NA, #nolint
    reverse = FALSE,
    ...
  ) {
  do.call("scale_mctq_rainbow", grab_fun_par())
}

scale_fill_mctq_rainbow_b <- function(
    aesthetics = "fill", #nolint
    scale_type = "b",
    color_type = "seq",
    alpha = NULL,
    direction = 1,
    na.value = NA, #nolint
    reverse = FALSE,
    ...
  ) {
  do.call("scale_mctq_rainbow", grab_fun_par())
}

# color_mctq_rainbow_sequential(10) |> rutils::vector_to_c()
color_mctq_rainbow_sequential <- function(n, alpha = NULL, direction = 1) {
  brandr::interpolate_colors(
    n = n,
    colors = c(
      "#FF1E00", "#FF7C00", "#FDD400", "#00CB00", "#009DF5",
      "#0040F7", "#981EAF"
    ),
    type = "seq",
    alpha = alpha,
    direction = direction
  )
}

# color_mctq_rainbow_qualitative(10) |> rutils::vector_to_c()
color_mctq_rainbow_qualitative <- function(n, alpha = NULL, direction = 1) {
  brandr::interpolate_colors(
    n = n,
    colors = c(
      "#FF1E00", "#FF7C00", "#FDD400", "#00CB00", "#009DF5",
      "#0040F7", "#981EAF"
    ),
    type = "qual",
    alpha = alpha,
    direction = direction
  )
}
