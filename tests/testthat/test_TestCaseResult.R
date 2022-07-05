test_that("$new() initializes fields correctly", {
  tc <- make_test_cases()[[1]]

  tcr <- TestCaseResult$new(TRUE, NULL, tc)

  expect_equal(tcr$passed, TRUE)
  expect_equal(tcr$error, NULL)
  expect_equal(tcr$test_case, tc)

  tcr <- TestCaseResult$new(FALSE, simpleError("bad"), tc)

  expect_equal(tcr$passed, FALSE)
  expect_equal(tcr$error, simpleError("bad"))
  expect_equal(tcr$test_case, tc)
})

test_that("$get_score() returns the correct score based on whether the test case passed or not", {
  tc <- make_test_cases()[[1]]

  tcr <- TestCaseResult$new(TRUE, NULL, tc)
  expect_equal(tcr$get_score(), tc$points)

  tcr <- TestCaseResult$new(FALSE, NULL, tc)
  expect_equal(tcr$get_score(), 0)
})

test_that("$repr() returns the correct string representation of the results", {
  tc <- make_test_cases()[[1]]

  tcr <- TestCaseResult$new(TRUE, NULL, tc)
  expect_equal(tcr$repr(), "Test q1 - 1 passed\n")

  tc$success_message <- "good"
  expect_equal(tcr$repr(), "Test q1 - 1 passed\ngood")

  tcr$passed <- FALSE
  tcr$error <- simpleError("failed")
  expect_equal(tcr$repr(), "Test q1 - 1 failed:\n\nfailed")

  tc$failure_message <- "bad"
  expect_equal(tcr$repr(), "Test q1 - 1 failed:\nbad\nfailed")
})

test_that("$to_list() converts the result to a list", {
  tc <- make_test_cases()[[1]]

  tcr <- TestCaseResult$new(TRUE, NULL, tc)
  tcr_list <- tcr$to_list()

  expect_true(is.list(tcr_list))
  expect_equal(length(tcr_list), 3)
  expect_equal(tcr_list$passed, TRUE)
  expect_equal(tcr_list$error, "")
  expect_equal(tcr_list$test_case, tc$to_list())

  # check that to_list converts the error to a string
  tcr <- TestCaseResult$new(FALSE, simpleError("bad"), tc)
  tcr_list <- tcr$to_list()
  expect_equal(tcr_list$error, "bad")
})

test_that("$get_message() returns the success/failure message according to $passed", {
  tc <- make_test_cases()[[1]]

  tcr <- TestCaseResult$new(TRUE, NULL, tc)
  expect_true(is.na(tcr$get_message()))

  tc$success_message <- "good"
  expect_equal(tcr$get_message(), tc$success_message)

  tcr <- TestCaseResult$new(FALSE, simpleError("failed"), tc)
  expect_true(is.na(tcr$get_message()))

  tc$failure_message <- "bad"
  expect_equal(tcr$get_message(), tc$failure_message)
})
