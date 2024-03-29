% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_ast_check_calls.R
\name{update_ast_check_calls}
\alias{update_ast_check_calls}
\title{Collect results of calls to \code{ottr::check} in an AST}
\usage{
update_ast_check_calls(tree, list_name)
}
\arguments{
\item{tree}{The tree to traverse}

\item{list_name}{The quoted name of the list}
}
\value{
The tree with substitutions made
}
\description{
Traverse an AST (a list of expressions) and change calls of the form
\code{. = ottr::check(...)} so that they are appended to a list with name \code{list_name}.

If \code{list_name} is \code{check_results_XX}, then \code{. = ottr::check(...)} becomes
\verb{check_results_XX[[<int>]] = ottr::check(...)}, where \verb{<int>} is an integer
}
