#' Check whether a string is valid R code
#'
#' @description Determine whether a code snippet has any syntax errors.
#'
#' @param script The code snippet
#'
#' @return Whether the code snippet is valid (can be parsed with `parse`)
#' @export
#'
#' @examples
#' s = "
#' a = TRUE
#' b = c(1, 2, 3)
#' d = function(x) x ^ 2
#' f = d(b)
#' "
#' valid_syntax(s)
#' #> [1] TRUE
#'
#' s = "
#' if (TRUE) {
#'   a = c(1, 2)
#' "
#' valid_syntax(s)
#' #> [1] FALSE
valid_syntax <- function(script) {
  error <- FALSE
  tryCatch(
    parse(text = script),
    error = function(e) error <<- TRUE
  )
  return(!error)
}

#' Determine whether this R session is running on Jupyter.
#'
#' @description Determine whether this R session is running on Jupyter by checking for a CommManager
#' in IRkernel.
#'
#' @return A boolean indicating whether IRkernel is running.
#'
#' @examples
#' \dontrun{
#' running_on_jupyter()
#' }
running_on_jupyter <- function() {
  return(!is.null(IRKernel::comm_manager()))
}
