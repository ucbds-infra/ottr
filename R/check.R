#' Run the test cases in a test file
#'
#' @description Execute checks in a test suite and return the [TestFileResult] object from executing
#' the test. Optionally prints results of the test to console.
#'
#' @param test_file Path to a test file
#' @param test_env An environment against which to run tests
#' @param show_results Whether to print the results to stdout
#'
#' @return The parsed test results for the suite
#' @export
#'
#' @examples
#' \dontrun{
#' check("tests/q1.R")
#' }
check <- function(test_file, test_env, show_results) {
  # need to specify a test file
  if (missing(test_file)) {
    stop("must have a test file")
  }

  # if show_results is not passed, default to TRUE
  if (missing(show_results)) {
    show_results <- TRUE
  }

  # grab the calling frame
  if (missing(test_env)) {
    test_env <- parent.frame(1)
  }

  eval("options(testthat.use_colours = FALSE)", test_env)

  test_case_results <- c()

  # redirect stdout so that testthat doesn't print
  testthat::capture_output({
    # read the test cases from the test file
    test_cases <- load_test_cases(test_file)$cases

    # run the tests
    for (tc in test_cases) {
      err <- tc$run(test_env)
      test_case_results <- c(test_case_results, TestCaseResult$new(is.null(err), err, tc))
    }
  })

  file_result <- TestFileResult$new(test_file, test_case_results)

  # collect the result if needed
  if (!is.null(get_collector())) {
    get_collector()$add_result(file_result)
  }

  # print out suite_results if show_results is TRUE
  if (show_results) {
    cat(file_result$repr())
  }

  # options(testthat.use_colours = use_colours)

  # return the test suite results
  return(file_result)
}
