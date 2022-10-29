test_that("components integrate correctly", {
  script_path <- test_path("script.R")
  withr::defer(file.remove(script_path))

  check_code <- paste(
    "ottr::check(\"",
    Sys.glob(test_path("files", "integration_test_cases", "*.[Rr]")),
    "\")",
    sep = "",
    collapse = "\n"
  )

  run_test <- function(code, expected_json_path) {
    code_with_checks <- paste0(code, "\n", check_code)
    writeLines(code_with_checks, script_path, sep = "\n")
    results_json <- run_autograder(script_path)
    expect_equal(
      as.character(stringr::str_replace_all(results_json, r"(\\u001b\[\d\dm)", "")),
      paste(readLines(expected_json_path, warn = FALSE), collapse = "\n"))
  }

  passes_all <- "
    x <- 2
    square <- function(x) x ^ 2
    y <- paste0(\"hi there, \", c(\"a\", \"b\", \"c\"), \"!\")
  "
  run_test(passes_all, test_path("files", "integration_expected_results", "passes_all.json"))

  fails_2_hidden <- paste0(passes_all, "\nsquare <- function(x) 1")
  run_test(fails_2_hidden, test_path("files", "integration_expected_results", "fails_2_hidden.json"))

  fails_3 <- paste0(passes_all, "\ny <- c(1, 2)")
  run_test(fails_3, test_path("files", "integration_expected_results", "fails_3.json"))
})
