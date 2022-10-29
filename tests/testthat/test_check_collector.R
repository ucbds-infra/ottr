test_that("CheckCollector works", {
  collector <- CheckCollector$new()
  expect_equal(collector$get_results(), list())

  res1 <- TestFileResult$new(list(), "foo.R")
  collector$add_result(res1)
  expect_equal(collector$get_results(), list(res1))

  res2 <- TestFileResult$new(list(), "bar.R")
  collector$add_result(res2)
  expect_equal(collector$get_results(), list(res1, res2))
})

test_that("initialize_collector and get_collector work", {
  expect_true(is.null(get_collector()))

  initialize_collector()
  collector <- get_collector()
  expect_false(is.null(collector))
  expect_true(inherits(collector, "CheckCollector"))

  initialize_collector()
  expect_true(inherits(get_collector(), "CheckCollector"))
  expect_false(identical(collector, get_collector()))
})
