library(mockery)

test_that("writes a zip file with the correct contents", {
  mock_display_html <- mock()
  stub(export, "IRdisplay::display_html", mock_display_html)

  notebook_path <- "my_notebook.ipynb"
  writeLines(c("This is a notebook!"), notebook_path, sep = "")
  withr::defer(file.remove(notebook_path))

  export(notebook_path)

  expect_equal(length(Sys.glob("*.zip")), 1)
  zip_path <- Sys.glob("*.zip")[1]
  withr::defer(file.remove(zip_path))

  # check that the zip file name has the correct format
  expect_true(stringr::str_detect(zip_path, paste0(
    "^",
    tools::file_path_sans_ext(basename(notebook_path)),
    "_\\d{4}_\\d{2}_\\d{2}T\\d{2}_\\d{2}_\\d{2}\\.zip$")))

  # check the zip file contents
  expect_equal(zip::zip_list(zip_path)$filename, c("__zip_filename__", notebook_path))

  # check that __zip_filename__ was deleted
  expect_false(file.exists("__zip_filename__"))

  # check that IRdisplay::display_html was called
  expect_equal(length(mock_display_html), 1)
})
