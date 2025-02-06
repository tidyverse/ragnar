
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
  res <- convert(x, ...)
  c(title = res$title, content = res$text_content)
}


split_markdown <- function(md, split_by = c("h1", "h2", "h3", "pre", "p")) {
  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html))
  pandoc::pandoc_convert(text = md, to = "html", output = tmp_html)
  html_text3(doc = read_html(tmp_html, encoding = "UTF-8"),
             split_tags = split_by)
}

frame_markdown <- function(md, frame_by = c("h1", "h2", "h3"), split_by = c("p", "pre")) {
  md <- split_markdown(md, split_by = unique(c(frame_by, split_by)))
  frame <- vec_frame_flattened_tree(md, frame_by, names = "tag", leaves = "text")
  if (base::setequal(split_by, frame_by))
    frame[["tag"]] <- NULL
  as_tibble(frame)
}


if(FALSE) {
  markitdown("https://r4ds.hadley.nz/base-r") |>
    # unname() |>
    frame_markdown() |>
    print(n = Inf)
}
