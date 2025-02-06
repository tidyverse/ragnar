
set_markitdown_options <- function(...) {
  init_markitdown(...)
}

init_markitdown <- function(...) {
  .globals$markitdown <- reticulate::import("markitdown")$MarkItDown(...)
}

markitdown <- function(x, ...) {
  # ... kwargs that can be passed down. These can be supplied to
  # MarkItDown.__init__() or passed on to individual convert() kwargs:
  # llm_client
  # llm_model
  # style_map  (used for docx conversion)
  # exiftool_path  (used for wav mp3 image conversion)
  # file_extension
  # url
  convert <- .globals$markitdown$convert %||% init_markitdown()$convert
  convert(x, ...)
}

