test <- list(
  name = "q2",
  cases = list(
    ottr::TestCase$new(
      name = "q2-1",
      code = {
        testthat::expect_equal(square(1), 1)
        testthat::expect_equal(square(-1), 1)
      },
      success_message = "square works for 1 and -1",
      failure_message = "square doesn't work for 1 and/or -1"
    ),
    ottr::TestCase$new(
      name = "q2-2",
      hidden = TRUE,
      points = 2,
      code = {
        testthat::expect_equal(square(2.5), 6.25)
      }
    )
  )
)
