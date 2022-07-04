#' Load test cases from a test file
#'
#' @description Load test case data from a test file. Executes the file and grabs the global `test`
#' variable, which should be a `list`.
#'
#' @param test_file The path to the test file
#'
#' @return The test cases
load_test_cases <- function(test_file) {
  env <- new.env()

  exps <- parse(file = test_file)

  for (i in seq_along(exps)) {
    exp <- exps[i]
    eval(exp, envir = env)
  }

  if (!("test" %in% names(env))) {
    stop(paste0("Test file does not declare a global test variable: ", test_file))
  }

  # add names to any test cases missing them
  test_suite <- env$test
  if (is.na(test_suite$name)) {
    test_suite$name <- basename(test_file)
  }

  for (i in seq_along(test_suite$cases)) {
    tc <- test_suite$cases[[i]]
    if (is.na(tc$name)) {
      tc$name <- paste(test_suite$name, "-", i)
    }
  }

  return(test_suite)
}
