#' Generate an environment from an R script
#'
#' @description Execute a string as an R script and return the environment from that execution.
#'
#' Converts a string to an AST and executes that script in a dummy environment for running test
#' cases against. Transforms all expressions of the form `. = ottr::check(...)` by replacing the `.`
#' with an index into a list in the environment with name `check_results_{SECRET}` to collect the
#' [TestFileResult] objects generated from those checks. (This helps to handle variable
#' name collisions in tests when grading a script.)
#'
#' @param script The string to be executed
#' @param secret The string to be appended to the name `check_results_` as the list name to collect
#' results
#' @param ignore_errors Whether to ignore errors thrown while executing the script
#'
#' @return The global environment after executing the script
execute_script <- function(script, secret, ignore_errors) {

  if (missing(ignore_errors)) {
    ignore_errors <- TRUE
  }

  # convert script to a list of expressions
  tree <- as.list(parse(text = script))

  # create check result collection list name as expression
  list_name <- parse(text = paste0("check_results_", secret))[[1]]

  # wrap calls of form `. = ottr::check(...)` to append to list and convert back to string
  tree <- update_ast_check_calls(tree, list_name)

  # create dummy env for execution and add check_results_XX list
  test_env <- new.env()
  test_env[[as.character(list_name)]] = list()

  # run the script, capturing stdout, and return the environment
  testthat::capture_output({
    for (expr in tree) {
      tryCatch(
        eval(expr, envir = test_env),
        error = function(e) {
          if (!ignore_errors) {
            stop(e)
          }
        }
      )
    }
  })
  return(test_env)
}
