#' @keywords internal

.onLoad <- function(libname, pkgname) {
  # Load shinyBS when the package is loaded
  requireNamespace("shinyBS", quietly = TRUE)
}
