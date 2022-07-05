get_expected_fields <- function() {
  return(list(
    name = "q1-1",
    code = substitute({
      x <- 1
      testthat::expect_equal(x, 1)
    }),
    points = 2,
    hidden = TRUE,
    success_message = "hi there",
    failure_message = "bye there"
  ))
}

test_that("$new() initializes fields correctly", {
  expected_fields <- get_expected_fields()

  check_fields <- function(field) {
    expect_equal(tc[[field]], expected_fields[[field]])
  }

  tc <- do.call(TestCase$new, expected_fields)
  lapply(names(expected_fields), check_fields)

  # check default value of points
  expected_fields$points <- 1
  tc <- do.call(TestCase$new, expected_fields[names(expected_fields) != "points"])
  lapply(names(expected_fields), check_fields)

  # check default value of hidden
  expected_fields$hidden <- FALSE
  tc <- do.call(TestCase$new, expected_fields[names(expected_fields) != "hidden"])
  lapply(names(expected_fields), check_fields)
})

test_that("$run() runs its code in the provided environment and returns any errors thrown", {
  tc <- TestCase$new("q1-1", {
    x <- 1
    y <- 2 * x
    stop("done")
    z <- y * 3
  })

  env <- new.env()

  err <- tc$run(env)

  expect_equal(env$x, 1)
  expect_equal(env$y, 2)
  expect_false("z" %in% names(env))

  expect_true(inherits(err, "error"))
  expect_equal(err$message, "done")

  # check case when no error is thrown
  tc$code <- substitute({
    a <- 1
    b <- 2 * a
  })

  err <- tc$run(env)

  expect_equal(env$a, 1)
  expect_equal(env$b, 2)

  expect_true(is.null(err))
})

test_that("$to_list() converts itself to a list correctly", {
  expected_fields <- get_expected_fields()

  check_fields <- function(field) {
    expect_equal(tc$to_list()[[field]], expected_fields[[field]])
  }

  tc <- do.call(TestCase$new, expected_fields)

  expect_true(is.list(tc$to_list()))
  expected_fields$code <- "{\n    x <- 1\n    testthat::expect_equal(x, 1)\n}"
  lapply(names(expected_fields), check_fields)
})
