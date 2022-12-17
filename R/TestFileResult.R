#' An R6 class representing a collection of test case results
#'
#' @description A collection of test case results that correspond to a single test file.
#'
#' @field test_case_results The [TestCaseResult] objects that make up this test file
#' @field filename The name of the test file
#' @field points The point value of the test file or a list of test case point values
TestFileResult <- R6::R6Class(
  "TestFileResult",
  public = list(
    test_case_results = NA,
    filename = NA,
    points = NA,

    #' @description Create a test file result.
    #'
    #' @param filename The name of the test file
    #' @param test_case_results The [TestCaseResult] objects that make up this test file
    #' @param points The point value of the test file or a list of test case point values
    initialize = function(filename, test_case_results, points = NULL) {
      self$filename <- filename
      self$test_case_results <- test_case_results
      self$points <- points
    },

    #' @description Get the basename of the file this result corresponds to.
    #'
    #' @return The basename of the test file
    get_basename = function() basename(self$filename),

    #' @description Get the total score earned for this test file as a percentage. Uses
    #' [`TestCaseResult$get_score()`][TestCaseResult] to determine the points earned for each test
    #' case.
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
      if (messages != "") {
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

    #' @description Convert this result to a JSON-compatible list with all of its fields.
    #'
    #' @return The list representation of this result
    to_list = function() {
      tcr_lists <- list()
      for (i in seq_along(self$test_case_results)) {
        tcr_lists[[i]] = self$test_case_results[[i]]$to_list()
      }

      ret <- list(
        filename = self$filename,
        points = self$points,
        test_case_results = tcr_lists
      )

      return(ret)
    }
  )
)
