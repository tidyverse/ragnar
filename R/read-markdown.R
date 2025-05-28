set_markitdown_options <- function(...) {
  init_markitdown(...)
}

init_markitdown <- function(...) {
  .globals$markitdown <- reticulate::import("markitdown")$MarkItDown(...)
}


#' Convert files to markdown
#'
#' @param x A filepath or url. Accepts a wide variety of file types, including
#'   PDF, PowerPoint, Word, Excel, Images (EXIF metadata and OCR), Audio (EXIF
#'   metadata and speech transcription), HTML, Text-based formats (CSV, JSON, XML),
#'   ZIP files (iterates over contents), Youtube URLs, and EPubs.#'
#' @param ... Passed on to `MarkItDown.convert()`
#' @param canonical logical, whether to postprocess the output from MarkItDown
#'   with `commonmark::markdown_commonmark()`.
#'
#' @returns A single string of markdown
#' @export
#'
#' @examplesIf reticulate::py_available()
#' # convert html
#' read_as_markdown("https://r4ds.hadley.nz/base-R.html") |>
#'   substr(1, 1000) |>
#'   cat()
#'
#' read_as_markdown("https://r4ds.hadley.nz/base-R.html", canonical = TRUE) |>
#'   substr(1, 1000) |>
#'   cat()
#'
#' # convert pdf
#' pdf <- file.path(R.home("doc"), "NEWS.pdf")
#' read_as_markdown(pdf) |> substr(1, 1000) |> cat()
#' ## alternative:
#' # pdftools::pdf_text(pdf) |> substr(1, 2000) |> cat()
#'
#' # convert images to markdown descriptions using OpenAI
#' jpg <- file.path(R.home("doc"), "html", "logo.jpg")
#' if (Sys.getenv("OPENAI_API_KEY") != "") {
#'   # if (xfun::is_macos()) system("brew install ffmpeg")
#'   reticulate::py_require("openai")
#'   llm_client <- reticulate::import("openai")$OpenAI()
#'   read_as_markdown(jpg, llm_client = llm_client, llm_model = "gpt-4.1-mini")
#'   # # Description:
#'   # The image displays the logo of the R programming language. It features a
#'   # large, stylized capital letter "R" in blue, positioned prominently in the
#'   # center. Surrounding the "R" is a gray oval shape that is open on the right
#'   # side, creating a dynamic and modern appearance. The R logo is commonly
#'   # associated with statistical computing, data analysis, and graphical
#'   # representation in various scientific and professional fields.
#' }
#'
#' # Alternative approach to image conversion:
#' if (
#'   Sys.getenv("OPENAI_API_KEY") != "" &&
#'     rlang::is_installed("ellmer") &&
#'     rlang::is_installed("magick")
#' ) {
#'   chat <- ellmer::chat_openai(echo = TRUE)
#'   chat$chat("Describe this image", ellmer::content_image_file(jpg))
#' }
read_as_markdown <- function(x, ..., canonical = FALSE) {
  check_string(x)

  if (getOption("ragnar.markitdown.use_reticulate", TRUE)) {
    # use the Python API, faster, more powerful, the default
    # but we leave an escape hatch just in case there are other python
    # dependencies that conflict
    convert <- .globals$markitdown$convert %||% init_markitdown()$convert
    md <- convert(x, ...)
  } else {
    # use the markitdown cli API, (much) slower, but can be isolated from
    # reticulated python.

    check_dots_empty()
    outfile <- withr::local_tempfile(fileext = ".md")
    exit_code <- cli_markitdown(c(shQuote(x), "-o", shQuote(outfile)))
    if (
      !identical(exit_code, 0L) ||
        (no_outfile_produced <- !file.exists(outfile))
    ) {
      # more useful output to stderr() should have been printed
      # already by cli_markitdown() if we are here.
      errmsg <- stri_flatten(
        c(
          paste("markitdown exit code: ", exit_code),
          if (no_outfile_produced) "No output file produced."
        ),
        collapse = "\n"
      )
      stop(errmsg)
    }

    md <- stri_read_lines(outfile)
  }

  md <- stri_replace_all_fixed(md, "\f", "\n\n---\n\n")
  md <- unlist(stri_split_lines(md)) # normalize newlines
  if (canonical)
    md <- commonmark::markdown_commonmark(
      md,
      normalize = TRUE,
      footnotes = TRUE,
      width = 72L,
      extensions = TRUE
    )
  md <- stri_flatten(md, "\n")
  glue::as_glue(md)
}


markdown_locate_boundaries_bytes_index <- function(text, tags = NULL) {
  lines <- text |> stri_split_lines() |> unlist()
  text <- lines |> stri_flatten("\n")

  if (text == "")
    return(data_frame(tag = character(), start = integer(), end = integer()))

  doc <- text |>
    commonmark::markdown_html(
      sourcepos = TRUE,
      extensions = TRUE,
      normalize = TRUE
    ) |>
    enc2utf8() |>
    charToRaw() |>
    read_html(encoding = "UTF-8")

  elements <- doc |> xml_find_all(xpath = "//*[@data-sourcepos]")

  df <- tibble::tibble(
    tag = elements |> xml_name(),
    source_position = elements |> xml_attr("data-sourcepos")
  )

  if (length(tags)) {
    df <- df[df$tag %in% unique(c(tags)), ]
  }

  # commonmark returns positions as line:byte-line:byte
  # e.g., 52:1-52:20
  position <- df$source_position |>
    stri_split_charclass("[-:]", n = 4L, simplify = TRUE)
  storage.mode(position) <- "integer"
  colnames(position) <- c("start_line", "start_byte", "end_line", "end_byte")

  line_numbytes <- stri_numbytes(lines) + 1L # +1 for \n
  line_startbyte <- c(1L, 1L + drop_last(cumsum(line_numbytes)))

  start <-
    line_startbyte[position[, "start_line"]] + position[, "start_byte"] - 1L
  end <-
    line_startbyte[position[, "end_line"]] + position[, "end_byte"] - 1L

  ## To convert byte to char index:
  # char_byte_indexes <-
  #   stri_split_boundaries(text, type = "character")[[1L]] |>
  #   stri_numbytes() |> cumsum()
  # start <- match(start, char_byte_indexes)
  # end <- match(end, char_byte_indexes)
  tibble::tibble(tag = df$tag, start = start, end = end)
}


#' Segment markdown text
#'
#' @param text Markdown string
#' @param tags,segment_by A character vector of html tag names, e.g.,
#'   `c("h1", "h2", "h3", "pre")`
#' @param trim logical, trim whitespace on segments
#' @param omit_empty logical, whether to remove empty segments
#'
#' @returns A named character vector. Names will correspond to `tags`, or `""`
#'   for content in between tags.
#' @export
#'
#' @examples
#' md <- r"---(
#'
#' # Sample Markdown File
#'
#' ## Introduction
#'
#' This is a sample **Markdown** file for testing.
#'
#' ### Features
#'
#' - Simple **bold** text
#' - _Italicized_ text
#' - `Inline code`
#' - A [link](https://example.com)
#' - ‘Curly quotes are 3 bytes chars.’ Non-ascii text is fine.
#'
#' This is a paragraph with <p> tag.
#'
#' This next segment with code has a <pre> tag
#'
#' ```r
#' hello_world <- function() {
#'   cat("Hello, World!\n")
#' }
#' ```
#'
#' A table <table>:
#'
#'   | Name  | Age | City      |
#'   |-------|----:|-----------|
#'   | Alice |  25 | New York  |
#'   | Bob   |  30 | London    |
#'
#'
#' ## Conclusion
#'
#' Common tags:
#'
#' - h1, h2, h3, h4, h5, h6: section headings
#' - p: paragraph (prose)
#' - pre: pre-formatted text, meant to be displayed with monospace font.
#'   Typically code or code output
#' - blockquote: A blockquote
#' - table: A table
#' - ul: Unordered list
#' - ol: Ordered list
#' - li: Individual list item in a <ul> or <ol>
#'
#'
#' )---"
#' markdown_segment(md) |> tibble::enframe()
#' markdown_segment(md |> trimws()) |> tibble::enframe()
#' markdown_segment(md, c("li"), trim = TRUE, omit_empty = TRUE) |> tibble::enframe()
#' markdown_segment(md, c("table"), trim = TRUE, omit_empty = TRUE) |> tibble::enframe()
#' markdown_segment(md, c("ul"), trim = TRUE, omit_empty = TRUE) |> tibble::enframe()
markdown_segment <- function(
  text,
  tags = c("h1", "h2", "h3", "h4"),
  trim = FALSE,
  omit_empty = FALSE
) {
  # normalize newlines
  text <- text |> stri_split_lines() |> unlist() |> stri_flatten("\n")
  bytes <- charToRaw(text)

  # get cut positions
  sourcepos <- markdown_locate_boundaries_bytes_index(text, tags = tags)
  tag_boundaries <- sort(unique(c(sourcepos$start, sourcepos$end + 1L)))
  boundaries <- c(1L, tag_boundaries, length(bytes) + 1L)

  # split
  # np <- reticulate::import("numpy")
  # splits <- np$split(as.array(bytes), as.array(cuts - 1L)) |> vapply(rawToChar, "")
  sizes <- drop_first(boundaries) - drop_last(boundaries)
  splits <- vec_chop(bytes, sizes = sizes) |> vapply(rawToChar, "")

  if (trim) splits <- stri_trim_both(splits) # drops names

  # make names
  split_tags <- c("", sourcepos$tag[match(tag_boundaries, sourcepos$start)])
  split_tags[is.na(split_tags)] <- ""
  stopifnot(length(splits) == length(split_tags))
  names(splits) <- split_tags

  if (omit_empty) {
    splits <- splits[nzchar(splits) | nzchar(names(splits))]
  } else {
    empty_bookend <- c(
      if (sizes[1L] == 0L) 1L,
      if (last(sizes) == 0L) length(splits)
    )
    if (length(empty_bookend)) {
      splits <- splits[-empty_bookend]
    }
  }

  splits
}

#' @param frame_by Character vector of tags that will become columns in the returned dataframe.
#' @export
#' @rdname markdown_segment
markdown_frame <- function(
  text,
  frame_by = c("h1", "h2", "h3"),
  segment_by = NULL
) {
  text <- markdown_segment(text, unique(c(frame_by, segment_by)))
  frame <- vec_frame_flattened_tree(
    text,
    frame_by,
    names = "tag",
    leaves = "text"
  )
  if (!length(segment_by) || base::setequal(segment_by, frame_by))
    frame[["tag"]] <- NULL
  as_tibble(frame)
}


markdown_segment_text <- function(
  text,
  split_by = c("h1", "h2", "h3", "pre", "p")
) {
  ## Uses pandoc to convert md to html, then html_text3() to read and split.
  ## Returns a character vector. Note, the returned text does not have
  ## markdown formatting like ``` fences. Currently unused.
  ## TOOD: probably better to use commonmark instead of pandoc here.
  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html))
  check_installed("pandoc")
  pandoc::pandoc_convert(text = text, to = "html", output = tmp_html)
  html_text3(
    doc = read_html(tmp_html, encoding = "UTF-8"),
    split_tags = split_by
  )
}


#' Read a document as Markdown
#'
#' `ragnar_read()` uses [markitdown](https://github.com/microsoft/markitdown) to
#' convert a document to markdown. If `frame_by_tags` or `split_by_tags` is
#' provided, the converted markdown content is then split and converted to a
#' data frame, otherwise, the markdown is returned as a string.
#'
#' @param x file path or url.
#' @param ... passed on `markitdown.convert`.
#' @param split_by_tags character vector of html tag names used to split the
#'   returned text
#' @param frame_by_tags character vector of html tag names used to create a
#'   dataframe of the returned content
#'
#' @returns
#' Always returns a data frame with the columns:
#'   - `origin`: the file path or url
#'   - `hash`: a hash of the text content
#'   - `text`: the markdown content
#'
#' If `split_by_tags` is not `NULL`, then a `tag` column is also included containing
#' the corresponding tag for each text chunk. `""` is used for text chunks that
#' are not associated with a tag.
#'
#' If `frame_by_tags` is not `NULL`, then additional columns are included for each
#' tag in `frame_by_tags`. The text chunks are associated with the tags in the
#' order they appear in the markdown content.
#'
#' @export
#'
#' @examplesIf reticulate::py_available()
#' file <- tempfile(fileext = ".html")
#' download.file("https://r4ds.hadley.nz/base-R.html", file, quiet = TRUE)
#'
#' # with no arguments, returns a single row data frame.
#' # the markdown content is in the `text` column.
#' file |> ragnar_read() |> str()
#'
#' # use `split_by_tags` to get a data frame where the text is split by the
#' # specified tags (e.g., "h1", "h2", "h3")
#' file |>
#'   ragnar_read(split_by_tags = c("h1", "h2", "h3"))
#'
#' # use `frame_by_tags` to get a dataframe where the
#' # headings associated with each text chunk are easily accessible
#' file |>
#'   ragnar_read(frame_by_tags = c("h1", "h2", "h3"))
#'
#' # use `split_by_tags` and `frame_by_tags` together to further break up `text`.
#' file |>
#'   ragnar_read(
#'     split_by_tags = c("p"),
#'     frame_by_tags = c("h1", "h2", "h3")
#'   )
#'
#' # Example workflow adding context to each chunk
#' file |>
#'   ragnar_read(frame_by_tags = c("h1", "h2", "h3")) |>
#'   glue::glue_data(r"--(
#'     ## Excerpt from the book "R for Data Science (2e)"
#'     chapter: {h1}
#'     section: {h2}
#'     content: {text}
#'
#'     )--") |>
#'   # inspect
#'   _[6:7] |> cat(sep = "\n~~~~~~~~~~~\n")
#'
#' # Advanced example of postprocessing the output of ragnar_read()
#' # to add language to code blocks, markdown style
#' library(dplyr, warn.conflicts = FALSE)
#' library(stringr)
#' library(rvest)
#' library(xml2)
#' file |>
#'   ragnar_read(frame_by_tags = c("h1", "h2", "h3"),
#'               split_by_tags = c("p", "pre")) |>
#'   mutate(
#'     is_code = tag == "pre",
#'     text = ifelse(is_code, str_replace(text, "```", "```r"), text)
#'   ) |>
#'   group_by(h1, h2, h3) |>
#'   summarise(text = str_flatten(text, "\n\n"), .groups = "drop") |>
#'   glue::glue_data(r"--(
#'     # Excerpt from the book "R for Data Science (2e)"
#'     chapter: {h1}
#'     section: {h2}
#'     content: {text}
#'
#'     )--") |>
#'   # inspect
#'   _[9:10] |> cat(sep = "\n~~~~~~~~~~~\n")
ragnar_read <- function(x, ..., split_by_tags = NULL, frame_by_tags = NULL) {
  text <- read_as_markdown(x, ...)
  hash <- rlang::hash(text)

  if (is.null(frame_by_tags) && is.null(split_by_tags)) {
    out <- tibble::tibble(
      origin = x,
      hash = hash,
      text = text
    )
    return(out)
  }

  segmented <- markdown_segment(
    text,
    tags = unique(c(split_by_tags, frame_by_tags)),
    trim = TRUE,
    omit_empty = TRUE
  )

  frame <- vec_frame_flattened_tree(
    segmented,
    frame_by_tags %||% character(),
    names = "tag",
    leaves = "text"
  )

  # The tags column only needs to be there if we segment additionally to framing.
  if (is.null(split_by_tags) || base::setequal(split_by_tags, frame_by_tags)) {
    frame[["tag"]] <- NULL
  }

  frame[["origin"]] <- x
  frame[["hash"]] <- hash

  # reorder columns
  frame <- frame[unique(c("origin", "hash", names(frame), "text"))]

  as_tibble(frame)
}

# ------ utils

cli_markitdown <- function(args, ...) {
  if (is.na(Sys.getenv("PYTHONIOENCODING", NA)))
    withr::local_envvar("PYTHONIOENCODING" = "utf-8") # needed on windows

  reticulate::uv_run_tool(
    "markitdown",
    args,

    # pin cli escape hatch to a known working version
    # incase a future markitdown release breaks the default path.
    from = "markitdown[all]==0.1.1",

    # exclude_newer = "2025-02-22",

    python_version = "3.11",
    ...
  )
}


should_init_python <- function() {
  reticulate::py_available() ||
    interactive() ||
    identical(Sys.getenv("IN_PKGDOWN"), "true")
}
