#' An R6 class representing a test case
#'
#' @description A test case for Ottr. Contains configurations and code to be executed for the test.
#'
#' @field name The name of the test case
#' @field code The code to be executed as part of the test case
#' @field points The point value of the test case
#' @field hidden Whether the test case is hidden
#' @field success_message A message to show to students if the test passes
#' @field failure_message A message to show to students if the test fails
#'
#' @export
#'
#' @examples
#' tc = TestCase$new("q1", {
#'   testthat::assert_true(q1.ans)
#' })
#' env = new.env()
#' env$q1.ans = TRUE
#' tc$run(env)
TestCase <- R6::R6Class(
  "TestCase",
  public = list(
    name = NA,
    code = NA,
    points = NA,
    hidden = NA,
    success_message = NA,
    failure_message = NA,

    #' @description Create a test case.
    #'
    #' @param name The name of the test case
    #' @param code The code to be executed as part of the test case
    #' @param points The point value of the test case
    #' @param hidden Whether the test case is hidden
    #' @param success_message A message to show to students if the test passes
    #' @param failure_message A message to show to students if the test fails
    initialize = function(
      name,
      code,
      points = 1,
      hidden = FALSE,
      success_message = NA,
      failure_message = NA
    ) {
      self$name <- name
      self$code <- substitute(code)
      self$points <- points
      self$hidden <- hidden
      self$success_message <- success_message
      self$failure_message <- failure_message
    },

    #' @description Run the test case against the provided environment.
    #'
    #' @param env The environment to run the test case in
    run = function(env) {
      error <- NULL
      tryCatch(
        eval(self$code, envir = env, enclos = baseenv()),
        error = function(e) error <<- e
      )
      return(error)
    },

    #' @description Convert this test case to a JSON-compatible list with all of its fields.
    #'
    #' @return The list representation of this test case
    to_list = function() {
      return(list(
        name = self$name,
        code = paste(deparse(self$code), collapse = "\n"),
        points = self$points,
        hidden = self$hidden,
        success_message = self$success_message,
        failure_message = self$failure_message
      ))
    }
  )
)
