# Create intervals of missing labels for `ggplot2` charts

**\[maturing\]**

`skip_label()` is a utility function that creates intervals of missing
labels for [`ggplot2`](https://ggplot2.tidyverse.org/) charts. This is
useful when you have a large number of labels in a axis and you want to
remove some of them.

Use this function in the `label` argument of functions like
`scale_x_continuous()` or `scale_y_discrete()`.

## Usage

``` r
skip_label(x, type = "even")
```

## Arguments

- x:

  A [`character`](https://rdrr.io/r/base/character.html) vector of
  labels

- type:

  (Optional) A [`character`](https://rdrr.io/r/base/character.html)
  string with the type of labels to remove. Possible values are
  `"even"`, `"odd"` and `"one"` (Default: `"even"`).

## Value

A [`character`](https://rdrr.io/r/base/character.html) vector of labels
with some them missing (`""`).

## Examples

``` r
skip_label(x = 0:5, type = "even")
#> [1] "0" ""  "2" ""  "4" "" 
#> [1] "0" ""  "2" ""  "4" "" # Expected

skip_label(x = 0:5, type = "odd")
#> [1] ""  "1" ""  "3" ""  "5"
#> [1] ""  "1" ""  "3" ""  "5" # Expected

skip_label(x = 0:5, type = "one")
#> [1] "0" "1" ""  "3" "4" "" 
#> [1] "0" "1" ""  "3" "4" "" # Expected
```
