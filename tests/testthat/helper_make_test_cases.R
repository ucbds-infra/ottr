#' Generate a list of sample [TestCase] objects. The list returned should match
#' `test$cases` in `files/example_test_file.R` when `name_override` is not provided.
#'
#' @param name_override The name of the question; defaults to `"q1"`
make_test_cases <- function(name_override) {
  if (missing(name_override)) {
    name_override <- "q1"
  }

  return(list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = paste0(name_override, " - 1"),
      points = 1,
      code = {
        testthat::expect_true(is.numeric(x))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = paste0(name_override, " - 2"),
      points = 1,
      code = {
        testthat::expect_true(0 < x)
        testthat::expect_true(x < 100)
      }
    ),
    ottr::TestCase$new(
      hidden = TRUE,
      name = paste0(name_override, " - 3"),
      points = 1,
      code = {
        testthat::expect_equal(x, 2)
      }
    ),
    ottr::TestCase$new(
      hidden = TRUE,
      name = "q1d",
      points = 2,
      success_message = "congrats",
      code = {
        testthat::expect_equal(as.character(x), "2")
      }
    )
  ))
}
