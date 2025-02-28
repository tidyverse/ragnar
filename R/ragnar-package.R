#' @keywords internal
"_PACKAGE"


.globals <- new.env(parent = emptyenv())

# Not exported via NAMESPACE because the S3 class of DocumentConverterResult is not stable
py_to_r.markitdown.DocumentConverterResult <- function(x) {
  text <- x$text_content
  if(!is.null(x$title))
    text <- stri_c("# ", x$title, "\n\n", text)
  text
}


.onLoad <- function(libname, pkgname) {
  ## Commented out until these are fixed:
  ## https://github.com/microsoft/markitdown/issues/1063
  ## https://github.com/microsoft/markitdown/issues/1036
  ## See additional comments in read_as_markitdown().
  Sys.setenv(RETICULATE_PYTHON = "managed")
  reticulate::py_require(c(
    # "markitdown@git+https://github.com/microsoft/markitdown.git@main#subdirectory=packages/markitdown",
    # Pin to known working latest commit on "2025-02-28 12:50:17 EST"
    # test with ragnar::read_as_markdown("https://r4ds.hadley.nz/base-R.html")
    "markitdown@git+https://github.com/microsoft/markitdown.git@9a19fdd134b4de99ffbba6d50fd7848df6a6c50a#subdirectory=packages/markitdown",
    "markdownify<1"
  ))


  reticulate::py_register_load_hook("markitdown", function() {

    ## markitdown maintainers forgot to include `DocumentConverterResult`
    ## in one of the releases, so the `nameOfClass()` approach can't work
    #  nameOfClass(reticulate::import("markitdown")$DocumentConverterResult)
    ## So we actually convert something to get back a reified
    ## DocumentConverterResult object
    fi <- tempfile(fileext = ".txt")
    on.exit(unlink(fi))
    writeLines("hi", fi)
    convert <- init_markitdown()$convert
    DocumentConverterResult <- convert(fi)
    if (inherits(DocumentConverterResult, "python.builtin.object")) {
      s3_class <- class(DocumentConverterResult)[1]
      registerS3method(
        "py_to_r",
        s3_class,
        py_to_r.markitdown.DocumentConverterResult,
        environment(reticulate::py_to_r)
      )
    }

    # still waiting on https://github.com/microsoft/markitdown/pull/322
    # to be included in a release.
#     reticulate::py_run_string(local = TRUE, r"---(
# import markitdown._markitdown as md
#
# _CustomMarkdownify = md._CustomMarkdownify
# og_convert_a = _CustomMarkdownify.convert_a
#
# def patched_convert_a(self, el, text, convert_as_inline):
#     # if el.get('href', '') and el.get('href', '').startswith('https://rdrr.io/r'):
#     #    return text
#     if el.find_parent('pre'):
#         return text
#     return og_convert_a(self, el, text, convert_as_inline)
#
# _CustomMarkdownify.convert_a = patched_convert_a
#
# )---")


  })
}
