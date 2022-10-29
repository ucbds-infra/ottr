library(mockery)

test_that("executes a script and returns an environment", {
  tests_dir <- "my_tests"
  dir.create(tests_dir)
  file.create(file.path(tests_dir, "q1.R"))
  tests_glob <- file.path(tests_dir, "*.[Rr]")
  withr::defer(unlink(tests_dir, recursive = TRUE))

  script <- "
    a = 1
    b = TRUE
    c = 4 * pi
    d = (function(x) 3 * x + 1)(10)
    . = ottr::check(\"my_tests/q1.R\")
  "
  script_tree <- as.list(parse(text = script))

  expected_env <- new.env()
  expected_env$a <- 1
  expected_env$b <- TRUE
  expected_env$c <- 4 * pi
  expected_env$d <- 31

  env <- execute_script(script)

  expect_mapequal(as.list(env), as.list(expected_env))
})

test_that("handles errors thrown by scripts according to ignore_errors", {
  error_message <- "Oh no, our table, it's broken!"
  script <- paste0("
    a = 1
    stop(\"", error_message, "\")
    b = 2
  ")

  env <- execute_script(script)
  expect_equal(env$b, 2)

  env <- execute_script(script, TRUE)
  expect_equal(env$b, 2)

  expect_error(execute_script(script, FALSE), regexp = error_message)
})
