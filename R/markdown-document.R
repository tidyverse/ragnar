#' Markdown documents
#'
#' @description
#'
#' `MarkdownDocument` represents a complete Markdown document stored as a single
#' character string. The constructor normalizes `text` by collapsing lines and
#' ensuring UTF-8 encoding, so downstream code can rely on a consistent format.
#'
#' [`read_as_markdown()`] is the recommended way to create a `MarkdownDocument`.
#' The constructor itself is exported only so advanced users can construct one by
#' other means when needed.
#'
#' @param text \[string] Markdown text.
#' @param origin \[string] Optional source path or URL. Defaults to the
#'   `"origin"` attribute of `text`, if present, otherwise `NULL`.
#'
#' @return An S7 object that inherits from `MarkdownDocument`, which is a length
#'   1 string of markdown text with an `@origin` property.
#' @export
#'
#' @name MarkdownDocument
#' @examples
#' md <- MarkdownDocument(
#'   "# Title\n\nSome text.",
#'   origin = "example.md"
#' )
#' md
MarkdownDocument := new_class(
  parent = class_character,
  properties = list(origin = prop_string(allow_null = TRUE)),
  constructor = function(text, origin = attr(text, "origin", TRUE)) {
    text <- markdown_normalize(text)
    new_object(text, origin = origin)
  },
  validator = function(self) {
    if (!is_scalar(self)) {
      "must be a string (length 1 character)"
    }
  }
)

markdown_normalize <- function(md, canonical = FALSE) {
  md <- md |>
    stri_split_lines() |>
    unlist() |>
    enc2utf8() |>
    stri_trim_right() |>
    stri_flatten("\n")

  if (canonical) {
    md <- md |>
      commonmark::markdown_commonmark(
        normalize = TRUE,
        footnotes = TRUE,
        extensions = TRUE
      )
  }
  md
}

local({
  method(convert, list(class_character, MarkdownDocument)) <-
    function(from, to, ...) {
      MarkdownDocument(from, ...)
    }
})


#' Markdown documents chunks
#'
#' @description
#'
#' `MarkdownDocumentChunks` stores information about candidate chunks in a
#' Markdown document. It is a tibble with three required columns:
#'
#' * `start`, `end` — integers. These are character positions (1-based, inclusive) in the source
#' `MarkdownDocument`, so that `substring(md, start, end)` yields the chunk
#' text. Ranges can overlap.
#'
#' * `context` — character.
#' A general-purpose field for adding context to a chunk. This column is
#' combined with `text` to augment chunk content when generating embeddings with
#' `ragnar_store_insert()`, and is also returned by `ragnar_retrieve()`. Keep in
#' mind that when chunks are deoverlapped (in `ragnar_retrieve()` or
#' `chunks_deoverlap()`), only the context value from the first chunk is kept.
#' `markdown_chunk()` by default populates this column with all the markdown
#' headings that are in-scope at the chunk start position.
#'
#' Additional columns can be included.
#'
#' The original document is available via the `@document` property.
#'
#' For normal use, chunk a Markdown document with [`markdown_chunk()`]; the
#' class constructor itself is exported only so advanced users can generate or
#' tweak chunks by other means.
#'
#' @param chunks A data frame containing `start`, `end`, and `context` columns,
#'   and optionally other columns.
#' @param document A `MarkdownDocument`.
#'
#' @return An S7 object that inherits from `MarkdownDocumentChunks`, which is
#'   also a `tibble`.
#' @export
#' @seealso [MarkdownDocument()]
#' @name MarkdownDocumentChunks
#' @examples
#' doc_text <- "# A\n\nB\n\n## C\n\nD"
#' doc <- MarkdownDocument(doc_text, origin = "some/where")
#' chunk_positions <- tibble::tibble(
#'   start = c(1L, 9L),
#'   end = c(8L, 15L),
#'   context = c("", "# A"),
#'   text = substring(doc, start, end)
#' )
#' chunks <- MarkdownDocumentChunks(chunk_positions, doc)
#' identical(chunks@document, doc)
MarkdownDocumentChunks := new_class(
  parent = new_S3_class(c("tbl_df", "tbl", "data.frame")),
  validator = function(self) {
    # TODO: make context optional
    if (!all(c("start", "end", "context") %in% names(self))) {
      "must have names 'start', 'end' and 'context'"
    }
  },
  constructor = function(chunks, document = attr(chunks, "document", TRUE)) {
    new_object(as_tibble(chunks), document = document)
  },
  properties = list(
    document = MarkdownDocument
  )
)


#' @exportS3Method pillar::tbl_sum
"tbl_sum.ragnar::MarkdownDocumentChunks" <- function(x, ...) {
  default_header <- NextMethod()
  c(
    "@document@origin" = x@document@origin,
    default_header
  )
}

local({
  method(vec_proxy, MarkdownDocumentChunks) <- function(x, ...) {
    # drop S7 class, properties, convert to simple data.frame
    out <- x
    attributes(out) <- NULL
    names(out) <- names(x)
    new_data_frame(out)
  }
})
