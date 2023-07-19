library(mockery)

test_that("saves the notebook by displaying Javascript", {
  stub(save_notebook, "running_on_jupyter", TRUE)
  stub(save_notebook, "file.mtime", mock(Sys.time(), cycle = TRUE))
  stub(save_notebook, "file.size", mock(1, cycle = TRUE))

  mock_display_javascript <- mock()
  stub(save_notebook, "IRdisplay::display_javascript", mock_display_javascript)

  expect_true(save_notebook("foo.ipynb"))

  expect_called(mock_display_javascript, 1)
})

test_that("handles save failure", {
  stub(save_notebook, "running_on_jupyter", TRUE)
  now <- Sys.time()
  stub(save_notebook, "file.mtime", mock(now, cycle = TRUE))
  stub(save_notebook, "file.size", mock(1, cycle = TRUE))

  mock_display_javascript <- mock()
  stub(save_notebook, "IRdisplay::display_javascript", mock_display_javascript)

  timeout <- 2
  start <- Sys.time()
  expect_false(save_notebook("foo.ipynb", timeout = timeout))
  stop <- Sys.time()

  expect_true(stop - start > as.difftime(timeout, units = "secs"))

  expect_called(mock_display_javascript, 1)
})

test_that("does nothing if not running on Jupyter", {
  stub(save_notebook, "running_on_jupyter", FALSE)

  mock_display_javascript <- mock()
  stub(save_notebook, "IRdisplay::display_javascript", mock_display_javascript)

  expect_true(save_notebook("foo.ipynb"))

  expect_called(mock_display_javascript, 0)
})
