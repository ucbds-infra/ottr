make_test_case_results <- function() {
  test_cases <- make_test_cases()
  return(lapply(test_cases, function(tc) {
    return(TestCaseResult$new(
      !tc$hidden,
      if (tc$hidden) simpleError("failed") else NULL,
      tc
    ))
  }))
}

test_that("$new() initializes fields correctly", {
  filename <- "tests/q1.R"
  tcrs <- make_test_case_results()

  tfr <- TestFileResult$new(filename, tcrs)

  expect_equal(tfr$filename, filename)
  expect_equal(tfr$test_case_results, tcrs)
})

test_that("$get_basename() returns the basename of the test file", {
  filename <- "tests/q1.R"
  tcrs <- make_test_case_results()

  tfr <- TestFileResult$new(filename, tcrs)

  expect_equal(tfr$get_basename(), basename(filename))
})

test_that("$get_score() returns the correct score", {
  filename <- "tests/q1.R"
  tcrs <- make_test_case_results()

  tfr <- TestFileResult$new(filename, tcrs)

  expect_equal(tfr$get_score(), 0.4)
})

test_that("$repr() returns the string representation of the test file results", {
  filename <- "tests/q1.R"
  tcrs <- make_test_case_results()
  tcrs[[1]]$test_case$success_message <- "good"
  tcrs[[3]]$test_case$failure_message <- "bad"

  tfr <- TestFileResult$new(filename, tcrs)

  expect_equal(tfr$repr(), paste(
    "Test q1 - 1 passed",
    "good",
    "",
    "Test q1 - 2 passed",
    "",
    "",
    "Test q1 - 3 failed:",
    "bad",
    "failed",
    "",
    "Test q1d failed:",
    "",
    "failed",
    sep = "\n"
  ))

  # check case when all tests pass
  tfr <- TestFileResult$new(
    "", list(TestCaseResult$new(TRUE, NULL, TestCase$new("", {}, points = 1))))
  expect_equal(tfr$repr(), "\nAll tests passed!")
})

test_that("$to_list() returns the test file results as a list", {
  filename <- "tests/q1.R"
  tcrs <- make_test_case_results()

  tfr <- TestFileResult$new(filename, tcrs)
  tfr_list <- tfr$to_list()

  expect_true(is.list(tfr_list))
  expect_equal(length(tfr_list), 2)
  expect_equal(tfr_list$filename, filename)
  expect_equal(tfr_list$test_case_results, lapply(tcrs, function(tcr) tcr$to_list()))
})
