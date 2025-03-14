# library(beepr)
# library(cffr)
# library(codemetar)
# library(groomr) # github.com/danielvartan/groomr
# library(here)
# library(readr)
# library(rutils) # github.com/danielvartan/rutils
# library(stringr)

# Remove empty lines from `README.md` -----

groomr::remove_blank_line_dups(here::here("README.md"))

# Copy `_brand.yml` to `./inst/extdata/` -----

file.copy(
  from = here::here("_brand.yml"),
  to = here::here("inst", "extdata", "_brand.yml"),
  overwrite = TRUE
)

# Update package versions in `DESCRIPTION` -----

rutils::update_pkg_versions()

file <- here::here("DESCRIPTION")

file |>
  readr::read_lines() |>
  stringr::str_replace_all(
    pattern = "\\.90[0-9]{2}(?=\\))",
    replacement = ""
  ) |>
  readr::write_lines(file)

# Update package year -----

files <- c(
  here::here("LICENSE"),
  here::here("LICENSE.md"),
  here::here("inst", "CITATION")
)

for (i in files) {
  data <-
    i |>
    readr::read_lines() |>
    stringr::str_replace_all(
      pattern = "20\\d{2}",
      replacement = as.character(Sys.Date() |> lubridate::year())
    )

  data |> readr::write_lines(i)
}

# Update `cffr` and `codemeta` -----

cffr::cff_write()
codemetar::write_codemeta()

# Check if the script ran successfully -----

beepr::beep(1)

Sys.sleep(3)
