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

  run_test <- function(code, ...) {
    code_with_checks <- paste0(code, "\n", check_code)
    writeLines(code_with_checks, script_path, sep = "\n")
    results_json <- run_autograder(script_path)
    got <- as.character(stringr::str_replace_all(results_json, r"(\\u001b\[\d\dm)", ""))
    want <- NULL
    # go through all valid JSON file paths and check if any matches; if it does, use that one, otherwise
    # use the first one
    for (p in c(...)) {
      pc <- paste(readLines(p, warn = FALSE), collapse = "\n")
      if (is.null(want)) {
        want <- pc
      }
      if (got == pc) {
        want <- pc
        break
      }
    }
    testthat::expect_equal(want, got)
  }

  passes_all <- "
    x <- 2
    square <- function(x) x ^ 2
    y <- paste0(\"hi there, \", c(\"a\", \"b\", \"c\"), \"!\")
  "
  run_test(
    passes_all,
    test_path("files", "integration_expected_results", "passes_all.json"),
    test_path("files", "integration_expected_results_rounding", "passes_all.json"))

  fails_2_hidden <- paste0(passes_all, "\nsquare <- function(x) 1")
  run_test(
    fails_2_hidden,
    test_path("files", "integration_expected_results", "fails_2_hidden.json"),
    test_path("files", "integration_expected_results_rounding", "fails_2_hidden.json"))

  fails_3 <- paste0(passes_all, "\ny <- c(1, 2)")
  run_test(
    fails_3,
    test_path("files", "integration_expected_results", "fails_3.json"),
    test_path("files", "integration_expected_results_rounding", "fails_3.json"))
})
