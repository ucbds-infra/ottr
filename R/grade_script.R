#' Grade an R script against a series of test files
#'
#' @description Execute a script, parse check outputs, and run additional tests specified by the
#' glob pattern `tests_glob` on the test environment.
#'
#' @param script_path The path to the script
#' @param tests_glob The pattern to search for extra tests
#' @param secret The string to be appended to the name `check_results_` as the list name to collect
#' results (optional)
#' @param ignore_errors Whether to ignore errors thrown while executing the script
#'
#' @return The [GradingResults] object after executing tests referenced in the script
#' and those specified by `tests_glob`
grade_script <- function(script_path, tests_glob, ignore_errors) {
  # convert script to a string
  script <- paste(readLines(script_path), collapse = "\n")

  if (missing(ignore_errors)) {
    ignore_errors <- TRUE
  }

  # initialize the collector and store it
  initialize_collector()
  check_collector <- get_collector()

  # run the script and extract results from env, capturing stdout
  testthat::capture_output({
    test_env <- execute_script(script, ignore_errors)
  }, print = TRUE)

  test_file_results <- check_collector$get_results()

  # run the tests in tests_glob on the env, collect in test_file_results
  num_embedded_tests <- length(test_file_results)
  tests_glob <- Sys.glob(tests_glob)
  i <- 1
  for (test_file in tests_glob) {
    already_tested <- sapply(test_file_results, function(tfr) tfr$get_basename())
    if (!(basename(test_file) %in% already_tested)) {
      test_file_results[[i + num_embedded_tests]] <- check(test_file, test_env, FALSE)
      i <- i + 1
    }
  }

  return(GradingResults$new(test_file_results))
}
