#' @keywords internal
"_PACKAGE"


.globals <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  reticulate::py_require("markitdown")
}

