#' Create the expect JSON-serializable representation of the results returned by
#' [make_test_file_results()]
make_test_file_results_list <- function() {
  results <- make_test_file_results()
  return(list(test_file_results = lapply(results, function(r) r$to_list())))
}
