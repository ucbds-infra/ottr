# TODO: convert update_ast_check_calls to also work for calls that aren't in assignment statements
# (i.e.. `ottr::check(...)`, not `. = ottr::check(...)`)

#' Collect results of calls to `ottr::check` in an AST
#'
#' @description Traverse an AST (a list of expressions) and change calls of the form
#' `. = ottr::check(...)` so that they are appended to a list with name `list_name`.
#'
#' If `list_name` is `check_results_XX`, then `. = ottr::check(...)` becomes
#' `check_results_XX[[<int>]] = ottr::check(...)`, where `<int>` is an integer
#'
#' @param tree The tree to traverse
#' @param list_name The quoted name of the list
#'
#' @return The tree with substitutions made
update_ast_check_calls <- function(tree, list_name) {
  list_idx <- 1
  for (i in seq_along(tree)) {
    expr <- tree[[i]]
    if (methods::is(expr, "=")) {
      right_expr <- expr[[3]]
      call <- right_expr[[1]]
      if (length(call) >= 3) {
        pkg <- call[[2]]
        fn <- call[[3]]
        if (pkg == "ottr" && fn == "check") {
          env <- new.env()
          env$list_name <- list_name
          env$list_idx <- list_idx
          new_left_expr <- substitute(list_name[[list_idx]], env)
          expr[[2]] <- new_left_expr
          list_idx <- list_idx + 1
        }
      }
    }
    tree[[i]] <- expr
  }
  return(tree)
}
