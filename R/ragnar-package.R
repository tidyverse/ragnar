#' @keywords internal
"_PACKAGE"


.globals <- new.env(parent = emptyenv())

#' @exportS3Method reticulate::py_to_r
py_to_r.markitdown._markitdown.DocumentConverterResult <- function(x) {
  c(title = x$title, text = x$text_content)
}


.onLoad <- function(libname, pkgname) {
  reticulate::py_require("markitdown")
  Sys.setenv(RETICULATE_PYTHON = "managed")
}

