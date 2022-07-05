test_that("parses a test file into a series of test cases", {
  test_file_path <- test_path("files", "example_test_file.R")

  suite <- load_test_cases(test_file_path)

  expect_equal(length(suite), 2)
  expect_equal(suite$name, "q1")
  expect_equal(suite$cases, make_test_cases())
})

test_that("supplies a name to the suite if none is provided", {
  test_file_path <- test_path("files", "another_test_file.R")
  lines <- readLines(test_path("files", "example_test_file.R"))
  lines <- lines[!stringr::str_detect(lines, "^  name = \"q1\"")]
  writeLines(lines, test_file_path)
  withr::defer(file.remove(test_file_path))

  suite <- load_test_cases(test_file_path)

  expect_equal(suite$cases, make_test_cases("another_test_file"))
})

test_that("fails for a test file with no `test` variable", {
  test_file_path <- test_path("files", "a_bad_test_file.R")
  writeLines(c("s = 1", "foo <- 'bar'"), test_file_path)
  withr::defer(file.remove(test_file_path))

  expect_error(
    load_test_cases(test_file_path),
    regexp = paste0("Test file does not declare a global test variable: ", test_file_path)
  )
})
