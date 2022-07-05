#' An R6 representing the results of running a test case
#'
#' @description Represents the results of running a test case against a global environment. Contains
#' metadata about the passing/failing of the test case as well as a reference to the test case
#' itself.
#'
#' @field passed Whether the test passed
#' @field error An error raised by executing the test, if any
#' @field test_case The [TestCase] that this result tracks
TestCaseResult <- R6::R6Class(
  "TestCaseResult",
  public = list(
    passed = NA,
    error = NA,
    test_case = NA,

    #' @description Create a test case result.
    #'
    #' @param passed Whether the test passed
    #' @param error An error raised by executing the test, if any
    #' @param test_case The `TestCase` that this result tracks
    initialize = function(passed, error, test_case) {
      self$passed <- passed
      self$error <- error
      self$test_case <- test_case
    },

    #' @description Get the score earned for this test case, accounting for whether the test passed
    #' or failed.
    #'
    #' @return The score
    get_score = function() {
      if (self$passed) {
        return(self$test_case$points)
      } else {
        return(0)
      }
    },

    #' @description Convert this result into a human-readable string for display.
    #'
    #' @return The string representation of this result
    repr = function() {
      message <- self$get_message()
      if (is.na(message)) {
        message <- ""
      }

      if (self$passed) {
        return(paste0("Test ", self$test_case$name, " passed\n", message))
      }

      indented_message <- paste(strsplit(self$error$message, "\n")[[1]], collapse = "\n  ")
      if (length(message) > 0) {
        message <- paste0(message, "\n")
      }

      output <- paste0("Test ", self$test_case$name, " failed:\n", message, indented_message)
      return(output)
    },

    #' @description Convert this result to a JSON-compatible list with all of its fields.
    #'
    #' @return The list representation of this result
    to_list = function() {
      return(list(
        passed = self$passed,
        # don't put NULL because jsonlite turns it into {}
        error = ifelse(is.null(self$error$message), "", self$error$message),
        test_case = self$test_case$to_list()
      ))
    },

    #' @description Get the message to be displayed to the student based on whether the test case
    #' passed or failed, if any.
    #'
    #' @return The message or `NA`
    get_message = function() {
      if (self$passed) {
        return(self$test_case$success_message)
      }
      return(self$test_case$failure_message)
    }
  )
)
