#' An R6 class representing a collection of test case results
#'
#' @description A collection of test case results that correspond to a single test file.
#'
#' @field test_file_results The [TestFileResult] objects that make up this grading
GradingResults <- R6::R6Class(
  "GradingResults",
  public = list(
    test_file_results = NA,

    #' @description Create a grading result.
    #'
    #' @param test_file_results The [TestFileResult] objects that make up this grading
    #' result
    initialize = function(test_file_results) {
      self$test_file_results <- test_file_results
    },

    #' @description Convert these results to a JSON-like `list` that can be convert to a
    #' `GradingResults` object by Otter's Python library.
    #'
    #' The returned list has the JSON format
    #'
    #' ```
    #' {
    #'   "test_file_results": [
    #'     {
    #'       // output of TestFileResult$to_list
    #'     }
    #'   ]
    #' }
    #' ```
    #'
    #' @return The generated list
    to_list = function() {
      out <- list(
        test_file_results = list()
      )
      for (i in seq_along(self$test_file_results)) {
        out$test_file_results[[i]] <- self$test_file_results[[i]]$to_list()
      }
      return(out)
    },

    #' @description Export these results to a JSON string.
    #'
    #' @return The JSON string
    to_json = function() {
      return(jsonlite::toJSON(self$to_list(), auto_unbox = TRUE, pretty = TRUE, null = "null"))
    }
  )
)
