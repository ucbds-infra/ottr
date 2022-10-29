library(mockery)

test_that("grades a script and returns a list of TestFileResult objects", {
  set.seed(42)

  test_file_result <- make_test_file_results()[[1]]

  mock_check <- mock(test_file_result, cycle = TRUE)
  stub(grade_script, "check", mock_check)

  env <- new.env()
  mock_execute_script <- mock(env, cycle = TRUE)
  stub(grade_script, "execute_script", mock_execute_script)

  tests_dir <- "my_tests"
  dir.create(tests_dir)
  file.create(file.path(tests_dir, "q1.R"))
  file.create(file.path(tests_dir, "q2.R"))
  tests_glob <- file.path(tests_dir, "*.[Rr]")
  withr::defer(unlink(tests_dir, recursive = TRUE))

  script_path <- "my_script.R"
  script_lines <- c("a = 1", "b = 2", "square = function(x) x ^ 2", "c = square(2)")
  script <- paste(script_lines, collapse = "\n")
  writeLines(script_lines, script_path)
  withr::defer(file.remove(script_path))

  results <- grade_script(script_path, tests_glob)

  expect_equal(results, GradingResults$new(c(test_file_result, test_file_result)))

  expect_equal(length(mock_execute_script), 1)
  expect_args(mock_execute_script, 1, script, TRUE)

  expect_equal(length(mock_check), 2)
  expect_args(mock_check, 1, file.path(tests_dir, "q1.R"), env, FALSE)
  expect_args(mock_check, 2, file.path(tests_dir, "q2.R"), env, FALSE)
})
