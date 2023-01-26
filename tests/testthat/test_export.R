library(mockery)

get_pdf_path <- function(p) paste0(tools::file_path_sans_ext(basename(p)), ".pdf")

test_that("writes a zip file with the correct contents", {
  mock_display_html <- mock()
  stub(export, "IRdisplay::display_html", mock_display_html)

  subm_path <- "my_notebook.ipynb"
  writeLines(c("This is a notebook!"), subm_path, sep = "")
  withr::defer(file.remove(subm_path))

  export(subm_path)

  expect_equal(length(Sys.glob("*.zip")), 1)
  zip_path <- Sys.glob("*.zip")[1]
  withr::defer(file.remove(zip_path))

  # check that the zip file name has the correct format
  expect_true(stringr::str_detect(zip_path, paste0(
    "^",
    tools::file_path_sans_ext(basename(subm_path)),
    "_\\d{4}_\\d{2}_\\d{2}T\\d{2}_\\d{2}_\\d{2}\\.zip$")))

  # check the zip file contents
  expect_equal(zip::zip_list(zip_path)$filename, c("__zip_filename__", subm_path))

  # check that __zip_filename__ was deleted
  expect_false(file.exists("__zip_filename__"))

  # check that IRdisplay::display_html was called
  expect_called(mock_display_html, 1)
})

test_that("supports PDF exports for ipynb files", {
  stub(export, "IRdisplay::display_html", mock())

  mock_system2 <- mock()
  stub(export, "system2", mock_system2)

  stub(export, "file.rename", mock())

  subm_path <- "my_notebook.ipynb"
  writeLines(c("This is a notebook!"), subm_path, sep = "")
  withr::defer(file.remove(subm_path))

  pdf_path <- get_pdf_path(subm_path)
  writeLines(c("This is a pdf!"), pdf_path, sep = "")
  withr::defer(file.remove(pdf_path))

  export(subm_path, pdf = TRUE)

  expect_equal(length(Sys.glob("*.zip")), 1)
  zip_path <- Sys.glob("*.zip")[1]
  withr::defer(file.remove(zip_path))

  # check the zip file contents
  expect_equal(
    zip::zip_list(zip_path)$filename,
    c("__zip_filename__", subm_path, pdf_path))

  # check that __zip_filename__ was deleted
  expect_false(file.exists("__zip_filename__"))

  # check that IRdisplay::display_html was called
  expect_called(mock_system2, 1)
  expect_args(mock_system2, 1, "jupyter", c("nbconvert", "--to=pdf", paste0("--output=", pdf_path), subm_path))
})

test_that("supports PDF exports for Rmd files", {
  stub(export, "IRdisplay::display_html", mock())

  mock_render <- mock()
  stub(export, "rmarkdown::render", mock_render)

  tempfile_path <- "foo.Rmd"
  stub(export, "tempfile", tempfile_path)

  subm_path <- "my_submission.Rmd"
  writeLines(c("This is an Rmd file!\n"), subm_path, sep = "")
  withr::defer(file.remove(subm_path))

  pdf_path <- get_pdf_path(subm_path)
  writeLines(c("This is a pdf!"), pdf_path, sep = "")
  withr::defer(file.remove(pdf_path))

  export(subm_path, pdf = TRUE)

  expect_equal(length(Sys.glob("*.zip")), 1)
  zip_path <- Sys.glob("*.zip")[1]
  withr::defer(file.remove(zip_path))

  # check the zip file contents
  expect_equal(
    zip::zip_list(zip_path)$filename,
    c("__zip_filename__", subm_path, pdf_path))

  # check that __zip_filename__ was deleted
  expect_false(file.exists("__zip_filename__"))

  # check that IRdisplay::display_html was called
  expect_called(mock_render, 1)
  expect_args(mock_render, 1, tempfile_path, "pdf_document", pdf_path, dirname(subm_path))
})
