test_that("collects calls to ottr::check in a list", {
  list_name <- parse(text = "my_list_abc123")[[1]]
  tree <- as.list(parse(text = "
    ans.1 = 2
    . = ottr::check('q1')

    foo = function(x) x ^ 2
    ans.2 = foo(4)

    . = ottr::check('q2')

    df = foo %>% sample(10)
    ottr::check('q3')
  "))

  fixed_tree <- update_ast_check_calls(tree, list_name)

  expect_equal(paste(fixed_tree, collapse = "\n"), paste(c(
    "ans.1 = 2",
    "my_list_abc123[[1]] = ottr::check(\"q1\")",
    "foo = function(x) x^2",
    "ans.2 = foo(4)",
    "my_list_abc123[[2]] = ottr::check(\"q2\")",
    "df = foo %>% sample(10)",
    "ottr::check(\"q3\")"
  ), collapse = "\n"))
})
