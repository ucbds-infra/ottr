#' Grade an R script against test files in a directory
#'
#' @description Run autograder in a Gradescope container and return the results as a
#' properly-formatted JSON string.
#'
#' @param script_path The path to the script
#' @param secret The string to be appended to the name `check_results_` as the list name to collect
#' results (optional)
#' @param ignore_errors Whether to ignore errors thrown while executing the script
#' @param test_dir A directory of tests to glob from
#'
#' @return The JSON string
#' @export
#'
#' @examples
#' \dontrun{
#' run_autograder("hw01.R", "ABC123", TRUE, "tests")
#' }
run_autograder <- function(script_path, secret, ignore_errors, test_dir) {
  if (missing(secret)) {
    secret <- make_secret()
  }

  if (missing(ignore_errors)) {
    ignore_errors <- TRUE
  }

  if (missing(test_dir)) {
    test_dir <- "/autograder/source/tests"
  }

  grading_results <- grade_script(script_path, paste0(test_dir, "/*.[Rr]"), secret, ignore_errors)
  return(grading_results$to_json())
}
