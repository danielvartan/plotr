test_that("skip_label() | General test", {
  skip_label(x = 0:5, type = "even") |>
    expect_equal(c("0", "", "2", "", "4", ""))

  skip_label(x = 0:5, type = "odd") |>
    expect_equal(c("",  "1", "", "3", "", "5"))

  skip_label(x = 0:5, type = "one") |>
    expect_equal(c("0", "1", "", "3", "4", ""))
})

test_that("skip_label() | Error test", {
  # checkmate::assert_atomic(x)
  skip_label(x = list(), type = "even") |>
    expect_error("Assertion on 'x' failed")

  # checkmate::assert_choice(type, c("even", "odd", "one"))
  skip_label(x = 1, type = "") |>
    expect_error("Assertion on 'type' failed")
})
