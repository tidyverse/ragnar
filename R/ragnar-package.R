#' @keywords internal
"_PACKAGE"


.globals <- new.env(parent = emptyenv())

# Not exported via NAMESPACE because the S3 class of DocumentConverterResult is not stable
py_to_r.markitdown.DocumentConverterResult <- function(x) {
  text <- x$text_content
  if (!is.null(x$title)) text <- stri_c("# ", x$title, "\n\n", text)
  text
}

#' @importFrom dotty .
dotty::.

.onLoad <- function(libname, pkgname) {
  Sys.setenv(RETICULATE_PYTHON = "managed")
  S7::methods_register()
  reticulate::py_require(c(
    # Pin onnxruntime until this is resolved: https://github.com/microsoft/markitdown/issues/1266
    # New VC++ version requirement begins:
    # https://github.com/Microsoft/onnxruntime/releases/tag/v1.21.0
    if (is_windows()) "onnxruntime<=1.20.1",
    "markitdown[all]"
  ))

  reticulate::py_register_load_hook("markitdown", function() {
    ## markitdown maintainers forgot to include `DocumentConverterResult`
    ## in one of the releases, so the `nameOfClass()` approach can't work
    #  nameOfClass(reticulate::import("markitdown")$DocumentConverterResult)
    ## So we actually convert something to markdown to get back a reified
    ## DocumentConverterResult object
    file <- tempfile(fileext = ".txt")
    on.exit(unlink(file))
    (writeLines)("hi", file)
    convert <- init_markitdown()$convert
    DocumentConverterResult <- convert(file)
    if (inherits(DocumentConverterResult, "python.builtin.object")) {
      s3_class <- class(DocumentConverterResult)[1]
      registerS3method(
        "py_to_r",
        s3_class,
        py_to_r.markitdown.DocumentConverterResult,
        environment(reticulate::py_to_r)
      )
    }
  })
}
