library(mockery)

test_that("executes a script and returns an environment", {
  secret <- "abc123"
  list_name <- parse(text = paste0("check_results_", secret))[[1]]

  script <- "
    a = 1
    b = TRUE
    c = 4 * pi
    d = (function(x) 3 * x + 1)(10)
    . = ottr::check(\"q1\")
  "
  script_tree <- as.list(parse(text = script))

  expected_env <- new.env()
  expected_env$a <- 1
  expected_env$b <- TRUE
  expected_env$c <- 4 * pi
  expected_env$d <- 31
  expected_env[[as.character(list_name)]] <- list()

  # have the mock return a tree without a call to check since there's no test file to run
  fixed_tree <- update_ast_check_calls(script_tree, list_name)
  fixed_tree <- fixed_tree[- length(fixed_tree)]
  mock_update_ast_check_calls <- mock(fixed_tree)
  stub(execute_script, "update_ast_check_calls", mock_update_ast_check_calls)

  env <- execute_script(script, secret)

  expect_mapequal(as.list(env), as.list(expected_env))
})

test_that("handles errors thrown by scripts according to ignore_errors", {
  error_message <- "Oh no, our table, it's broken!"
  script <- paste0("
    a = 1
    stop(\"", error_message, "\")
    b = 2
  ")

  env <- execute_script(script, "abc123")
  expect_equal(env$b, 2)

    env <- execute_script(script, "abc123", TRUE)
  expect_equal(env$b, 2)

  expect_error(execute_script(script, "abc123", FALSE), regexp = error_message)
})
