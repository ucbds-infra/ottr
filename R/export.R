#' Export a submission to a zip file
#'
#' @description Export a submission to a zip file for submitting. If indicated, a PDF of the
#' submission is generated and included in the zip file. (PDF generation is only supported for Rmd
#' and ipynb files.)
#'
#' @param submission_path The path to the submission
#' @param export_path The path at which to write the zip file (optional)
#' @param display_link Whether to display a download link with `IRdisplay`
#' @param pdf Whether to include a PDF of the submission (only works for Rmd and ipynb files)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' export("hw01.ipynb")
#'
#' # with pdf
#' export("hw01.ipynb", pdf = TRUE)
#' }
export <- function(submission_path, export_path = NULL, display_link = TRUE, pdf = FALSE) {
  timestamp <- format(Sys.time(), "%Y_%m_%dT%H_%M_%S")

  if (is.null(export_path)) {
    subm_name <- tools::file_path_sans_ext(basename(submission_path))
    export_path <- paste0(subm_name, "_", timestamp, ".zip")
  }

  pdf_path <- NULL
  if (pdf) {
    ext <- tools::file_ext(submission_path)
    pdf_path <- paste0(tools::file_path_sans_ext(basename(submission_path)), ".pdf")
    if (ext == "ipynb") {
      system2("jupyter", c("nbconvert", "--to=pdf", paste0("--output=", pdf_path), submission_path))

      # move the PDF to the working directory because nbconvert outputs it in the directory
      # containing the notebook
      move_from <- paste0(file.path(dirname(submission_path), pdf_path))
      file.rename(move_from, pdf_path)
    } else if (ext == "Rmd") {
      # add metadata to allow errors in the RMarkdown
      new_subm_path <- tempfile(fileext = ".Rmd")
      contents <- c(
        "```{r cache = F, include = F}\nknitr::opts_chunk$set(error = TRUE)\n```",
        readLines(submission_path))
      writeLines(contents, new_subm_path)

      rmarkdown::render(new_subm_path, "pdf_document", pdf_path)
      file.remove(new_subm_path)
    } else {
      stop("Only Rmd and ipynb files can be converted to PDFs")
    }
  }

  zip_filename_file_name <- "__zip_filename__"
  writeLines(c(export_path), zip_filename_file_name, sep = "")

  zip_files <- c(zip_filename_file_name, submission_path)
  if (!is.null(pdf_path)) {
    zip_files <- c(zip_files, pdf_path)
  }
  zip::zip(export_path, zip_files, mode = "cherry-pick")

  file.remove(zip_filename_file_name)

  if (display_link) {
    IRdisplay::display_html(sprintf("
    <p>Your submission has been exported. Click <a href='%s' download='%s'
    target='_blank'>here</a> to download the zip file.</p>
    ", export_path, export_path))
  }
}
