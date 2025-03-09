# library(ggplot2)

# Colors based on a data visualization found in @roenneberg2019b.

scale_fill_rainbow <- function(direction = 1) {
  colors <- c(
    "#FF1E00", "#FF7C00", "#FDD400", "#00CB00", "#009DF5",
    "#0040F7", "#981EAF"
  )

  if (direction == -1) colors <- rev(colors)

  ggplot2::scale_fill_manual(values = colors)
}
