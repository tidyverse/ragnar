#' @keywords internal
"_PACKAGE"


.globals <- new.env(parent = emptyenv())


#' @importFrom dotty .
dotty::.

ragnartools.markitdown <- NULL

.onLoad <- function(libname, pkgname) {
  Sys.setenv(RETICULATE_PYTHON = "managed")
  S7::methods_register()
  reticulate::py_require(c(
    # Pin onnxruntime until this is resolved:
    #  https://github.com/microsoft/markitdown/issues/1266
    # New VC++ version requirement begins:
    # https://github.com/Microsoft/onnxruntime/releases/tag/v1.21.0
    if (is_windows()) "onnxruntime<=1.20.1",
    "markitdown[all]"
  ))

  ragnartools.markitdown <<- reticulate::import_from_path(
    "_ragnartools.markitdown",
    system.file("python", package = "ragnar"),
    delay_load = TRUE
  )
}
