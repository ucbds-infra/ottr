test_that("recognizes valid syntax", {
  s = "
    a = TRUE
    b = c(1, 2, 3)
    d = function(x) x ^ 2
    f = d(b)
  "
  expect_true(valid_syntax(s))
})

test_that("recognizes invalid syntax", {
  s = "
    if (TRUE) {
      a = c(1, 2)
  "
  expect_false(valid_syntax(s))
})
