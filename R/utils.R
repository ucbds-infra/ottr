#' A string containing characters that can be made into a valid variable name. Does not include any
#' digits because randomly sampling with them included could result in an invalid variable name.
valid_expr_chars <- "abcdefghijklmnopqrstuvwxyzABCDEFGHIJLKMNOPQRSTUVWXYZ._"

#' Generate a random string of characters
#'
#' @description Randomly generate a string of `n_chars` sampled at random from `valid_chars`.
#'
#' @param n_chars The number of characters in the string; defaults to 6
#' @param valid_chars A string of characters to choose from; defaults to all alphanumerals, `.`, and
#' `_`
#'
#' @return The generated string
make_secret <- function(n_chars, valid_chars) {
  if (missing(n_chars)) {
    n_chars <- 6
  }

  if (missing(valid_chars)) {
    valid_chars <- strsplit(valid_expr_chars, "")[[1]]
  }

  chars <- sample(valid_chars, n_chars, replace = TRUE)
  return(paste(chars, collapse = ""))
}


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
