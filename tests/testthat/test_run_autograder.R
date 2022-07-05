library(mockery)

test_that("grades an R script and returns a JSON object of results", {
  set.seed(42)
  test_file_results <- make_test_file_results()
  grading_results <- GradingResults$new(test_file_results)

  mock_grade_script <- mock(grading_results)
  stub(run_autograder, "grade_script", mock_grade_script)

  ret <- run_autograder("hw01.R")

  expect_equal(ret, grading_results$to_json())

  expect_equal(length(mock_grade_script), 1)
  expect_args(mock_grade_script, 1, "hw01.R", "/autograder/source/tests/*.[Rr]", "WLayjJ", TRUE)
})
