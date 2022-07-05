test_that("generates a random string of characters", {
  set.seed(42)
  expect_equal(make_secret(), "WLayjJ")
  expect_equal(make_secret(10), "rWUxgJyLTt")
  expect_equal(make_secret(20, c("a", "b", "c", "1", "2", "3")), "bbcaac12221b1cbab3c3")
})
