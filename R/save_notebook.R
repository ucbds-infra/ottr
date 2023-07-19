#' Attempt to save the current notebook.
#'
#' @description Attempt to save the notebook by displaying Javascript if running on Jupyter. This
#' function waits until the modification time of the file has changed or until the specified timeout
#' expires.
#'
#' @param nb_path The path to the notebook
#' @param timeout Number of seconds to wait for save
#'
#' @return A boolean indicating whether the file was saved successfully. If
#' Jupyter is not running, this function returns TRUE.
#'
#' @examples
#' \dontrun{
#' save_notebook("foo.ipynb")
#' }
save_notebook <- function(nb_path, timeout = 10) {
  if (!running_on_jupyter()) {
    return(TRUE)
  }

  orig_mod_time = file.mtime(nb_path)
  start = Sys.time()
  IRdisplay::display_javascript("
    if (typeof Jupyter !== 'undefined') {
      Jupyter.notebook.save_checkpoint();
    }
    else {
      document.querySelector('[data-command=\"docmanager:save\"]').click();
    }
  ")

  timeout <- as.difftime(timeout, units = "secs")
  while (Sys.time() - start < timeout) {
    curr_mod_time = file.mtime(nb_path)
    if (orig_mod_time < curr_mod_time && file.size(nb_path) > 0) {
      return(TRUE)
    }

    Sys.sleep(0.2)
  }

  return(FALSE)
}
