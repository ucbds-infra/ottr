library(mockery)

test_that("runs a test against the student's environment and returns a TestFileResult", {
  test_env <- new.env()
  mock_parent.frame <- mock(test_env, cycle = TRUE)
  stub(check, "parent.frame", mock_parent.frame)

  mock_load_test_cases <- mock(list(cases = make_test_cases()), cycle = TRUE)
  stub(check, "load_test_cases", mock_load_test_cases)

  mock_cat <- mock()
  stub(check, "cat", mock_cat)

  test_file_path <- "tests/q1.r"

  results <- check(test_file_path)

  expect_true(inherits(results, "TestFileResult"))

  expect_equal(length(mock_parent.frame), 1)
  expect_args(mock_parent.frame, 1, 1)

  expect_equal(length(mock_load_test_cases), 1)
  expect_args(mock_load_test_cases, 1, test_file_path)

  expect_equal(length(mock_cat), 1)
  expect_args(mock_cat, 1, results$repr())

  # check that it doesn't print if show_results is false
  check(test_file_path, show_results = FALSE)
  expect_equal(length(mock_cat), 1)

  # check that it errors if no test file is provided
  expect_error(check(), regexp = "must have a test file")
})


test_that("correctly includes the test file point value in the TestFileR", {
  test_env <- new.env()
  mock_parent.frame <- mock(test_env, cycle = TRUE)
  stub(check, "parent.frame", mock_parent.frame)

  mock_cat <- mock()
  stub(check, "cat", mock_cat)

  mock_load_test_cases <- mock(list(
    cases = list(ottr::TestCase$new(name = "q1")),
    points = 1
  ), cycle = TRUE)
  stub(check, "load_test_cases", mock_load_test_cases)
  test_file_path <- "tests/q1.r"

  results <- check(test_file_path)

  expect_equal(results$points, 1)
})
