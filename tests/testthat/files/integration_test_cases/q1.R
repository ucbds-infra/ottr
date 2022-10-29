test <- list(
  name = "q1",
  cases = list(
    ottr::TestCase$new(
      name = "q1-1",
      code = {
        testthat::expect_true(x >= 1)
        testthat::expect_true(x <= 3)
      }
    ),
    ottr::TestCase$new(
      name = "q1-2",
      hidden = TRUE,
      code = {
        testthat::expect_equal(x, 2)
      }
    )
  )
)
