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
#' @keywords internal
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
# NOTE: this is used in promptdown
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

  sizes <- drop_first(boundaries) - drop_last(boundaries)
  splits <- vec_chop(bytes, sizes = sizes) |> vapply(rawToChar, "")

  if (trim) {
    splits <- stri_trim_both(splits)
  }

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


markdown_locate_boundaries_bytes_index <- function(text, tags = NULL) {
  lines <- text |> stri_split_lines() |> unlist()
  text <- lines |> stri_flatten("\n")

  if (text == "") {
    return(data_frame(tag = character(), start = integer(), end = integer()))
  }

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
  if (!length(segment_by) || base::setequal(segment_by, frame_by)) {
    frame[["tag"]] <- NULL
  }
  as_tibble(frame)
}
