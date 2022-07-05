make_test_file_results <- function() {
  return(list(
    TestFileResult$new("tests/q1.R", list(
      TestCaseResult$new(
        TRUE,
        NULL,
        TestCase$new("q1-1", {
          testthat::expect_true(TRUE)
        })
      ),
      TestCaseResult$new(
        TRUE,
        NULL,
        TestCase$new("q1-2", {
          expect_false(FALSE)
        })
      )
    )),
    TestFileResult$new("tests/q2.R", list(
      TestCaseResult$new(
        TRUE,
        NULL,
        TestCase$new("q2-1", {
          testthat::expect_true(TRUE)
        })
      ),
      TestCaseResult$new(
        FALSE,
        NULL,
        TestCase$new("q2-2", {
          expect_false(TRUE)
        }, hidden = TRUE)
      )
    ))
  ))
}
