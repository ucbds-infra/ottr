test_that("$new() initializes fields correctly", {
  tfrs <- make_test_file_results()

  gr <- GradingResults$new(tfrs)

  expect_equal(gr$test_file_results, tfrs)
})

test_that("$to_list() returns the grading results as a list", {
  tfrs <- make_test_file_results()

  gr <- GradingResults$new(tfrs)

  expect_equal(gr$to_list(), make_test_file_results_list())
})

test_that("$to_json() returns the grading results as JSON", {
  tfrs <- make_test_file_results()

  gr <- GradingResults$new(tfrs)

  expect_equal(
    gr$to_json(),
    jsonlite::toJSON(make_test_file_results_list(), auto_unbox = TRUE, pretty = TRUE)
  )
})
