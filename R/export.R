#' Export a JupyterNotebook to a zip file
#'
#' @description Export a Jupyter Notebook to a zip file for submission.
#'
#' @param notebook_path The path to the notebook
#' @param export_path The path at which to write the zip file (optional)
#' @param display_link Whether to display a download link with `IRdisplay`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' export("hw01.ipynb")
#' }
export <- function(notebook_path, export_path = NULL, display_link = TRUE) {
  timestamp <- format(Sys.time(), "%Y_%m_%dT%H_%M_%S")

  if (is.null(export_path)) {
    notebook_name <- tools::file_path_sans_ext(basename(notebook_path))
    export_path <- paste0(notebook_name, "_", timestamp, ".zip")
  }

  zip_filename_file_name <- "__zip_filename__"
  writeLines(c(export_path), zip_filename_file_name, sep = "")

  zip_files <- c(zip_filename_file_name, notebook_path)
  zip::zip(export_path, zip_files)
  file.remove(zip_filename_file_name)

  if (display_link) {
    IRdisplay::display_html(sprintf("
    <p>Your submission has been exported. Click <a href='%s' download='%s'
    target='_blank'>here</a> to download the zip file.</p>
    ", export_path, export_path))
  }
}
