#' An R6 class for collecting [TestFileResult] objects during grading.
#'
#' @description A collection of test file results created while grading an assignment
#'
#' @field test_file_results The [TestFileResult] objects created during grading
CheckCollector <- R6::R6Class(
  "CheckCollector",
  public = list(
    test_file_results = NA,

    initialize = function() {
      self$test_file_results <- list()
    },

    add_result = function(test_file_result) {
      self$test_file_results <- append(self$test_file_results, test_file_result)
    },

    get_results = function() {
      return(self$test_file_results)
    }
  )
)

#' An environment into which a collector will be initialized (so we don't need to update
#' global variables).
collector_env <- new.env()

#' The name of the active collector variable in [collector_env]
collector_varname <- "collector"

#' Create a new global [CheckCollector]
initialize_collector <- function() {
  collector_env[[collector_varname]] <- CheckCollector$new()
}

#' Retrieve the global [CheckCollector]
get_collector <- function() {
  return(collector_env[[collector_varname]])
}
