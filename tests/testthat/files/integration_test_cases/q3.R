test <- list(
  name = "q3",
  cases = list(
    ottr::TestCase$new(
      name = "q3-1",
      code = {
        testthat::expect_equal(length(y), 3)
      }
    ),
    ottr::TestCase$new(
      name = "q3-2",
      hidden = TRUE,
      points = 2,
      code = {
        testthat::expect_equal(y, c("hi there, a!", "hi there, b!", "hi there, c!"))
      },
      success_message = "y is correct",
      failure_message = "y is incorrect"
    )
  )
)
