#' An R6 class representing a collection of test case results
#'
#' @description A collection of test case results that correspond to a single test file.
#'
#' @field test_case_results The \code{ink{TestCaseResult}} objects that make up this test file
#' @field filename The name of the test file
TestFileResult <- R6::R6Class(
  "TestFileResult",
  public = list(
    test_case_results = NA,
    filename = NA,

    #' @description Create a test file result.
    #'
    #' @param filename The name of the test file
    #' @param test_case_results The \code{ink{TestCaseResult}} objects that make up this test file
    initialize = function(filename, test_case_results) {
      self$filename <- filename
      self$test_case_results <- test_case_results
    },

    #' @description Get the basename of the file this result corresponds to.
    #'
    #' @return The basename of the test file
    get_basename = function() basename(self$filename),

    #' @description Get the total score earned for this test file as a percentage. Uses
    #' \code{\link{TestCaseResult$get_score}} to determine the points earned for each test case.
    #'
    #' @return The score as a percentage.
    get_score = function() {
      earned <- 0
      possible <- 0
      for (tcr in self$test_case_results) {
        earned <- earned + tcr$get_score()
        possible <- possible + tcr$test_case$points
      }
      return(ifelse(possible == 0, 0, earned / possible))
    },

    #' @description Convert this result into a human-readable string for display.
    #'
    #' @return The string representation of this result
    repr = function() {
      # if all tests passed, just return that
      messages <- c()
      for (tcr in self$test_case_results) {
        if (!is.na(tcr$get_message())) {
          messages <- c(messages, tcr$get_message())
        }
      }

      messages <- paste(messages, collapse = "\n")
      if (length(messages) > 0) {
        messages <- paste0(messages, "\n")
      }

      if (self$get_score() == 1) {
        return(paste0(messages, "All tests passed!"))
      }

      # otherwise, iterate through results and put hints together
      output <- c()
      for (tcr in self$test_case_results) {
        output <- c(output, tcr$repr())
      }

      return(paste0(output, collapse = "\n\n"))
    },

    #' @description Get a vector of all failed hidden test case results.
    #'
    #' @return The failed hidden test case results
    failed_hidden_cases = function() {
      tcrs <- c()
      for (tcr in self$test_case_results) {
        if (tcr$test_case$hidden && !tcr$passed) {
          tcrs <- c(tcrs, tcr)
        }
      }
      return(tcrs)
    },

    #' @description Get a vector of all failed public test case results.
    #'
    #' @return The failed public test case results
    failed_public_cases = function() {
      tcrs <- c()
      for (tcr in self$test_case_results) {
        if (!tcr$test_case$hidden && !tcr$passed) {
          tcrs <- c(tcrs, tcr)
        }
      }
      return(tcrs)
    },

    #' @description Get the total points possible for this test file.
    #'
    #' @return The points possible
    get_points = function() {
      return(
        sum(sapply(sapply(self$test_case_results, getElement, "test_case"), getElement, "points")))
    },

    #' @description Determine whether any public test cases were failed.
    #'
    #' @return Whether an public test cases were failed
    failed_any_public = function() {
      for (tcr in self$test_case_results) {
        if (!tcr$test_case$hidden && !tcr$passed) {
          return(TRUE)
        }
      }
      return(FALSE)
    },

    #' @description Convert this result to a JSON-compatible list with all of its fields.
    #'
    #' @return The list representation of this result
    to_list = function() {
      tcr_lists <- list()
      for (i in seq_along(self$test_case_results)) {
        tcr_lists[[i]] = self$test_case_results[[i]]$to_list()
      }

      return(list(
        filename = self$filename,
        test_case_results = tcr_lists
      ))
    }
  )
)
