pick_cut_positions <- function(candidates, chunk_size) {
  .Call(pick_cut_positions_, as.integer(candidates), as.integer(chunk_size))
}

str_chunk1 <- function(
  string,
  candidate_cutpoints,
  # assuming:
  #   1 token ~ 4 characters
  #   one page ~ 400 tokens
  #   target chunk size ~ 1 page
  max_size = 1600L,
  trim = TRUE
) {
  if (isTRUE(is.na(string))) return(NA_character_)
  check_string(string, allow_na = TRUE)
  string_len <- stri_length(string)
  if (string_len <= max_size) return(string)

  candidate_cutpoints <- c(
    1L,
    as.integer(candidate_cutpoints),
    string_len
  )

  cut_points <- pick_cut_positions(candidate_cutpoints, max_size)
  chunks <- stri_sub(
    string,
    drop_last(cut_points),
    drop_first(cut_points),
    use_matrix = FALSE
  )

  if (trim) chunks <- stri_trim_both(chunks)

  chunks <- chunks[nzchar(chunks)]

  chunks
}

str_locate_boundaries1 <- function(string, boundary) {
  check_string(string)
  if (inherits(boundary, "stringr_pattern")) {
    locations <- stringr::str_locate_all(string, boundary)[[1L]][, "end"]
  } else {
    check_string(boundary)
    locations <- switch(
      boundary,
      ## TODO: we might need to a specialized markdown <p> tag detector here,
      ## since this will false positive on code chunks and non-compact lists
      ## (<pre> <ul> or <ol>) in markdown.
      ## We can probably factor markdown_boundaries() out of markdown_segment() and
      ## use it here. I.e., use commonmark::markdown_html() to extract sourcepos,
      ## then split on raw vector.
      ## ... or use stringi to convert byte to char indexes, e.g.,
      ## stri_split_boundaries(x, type = "char")[[1]] |>  stri_numbytes()
      paragraph = stri_locate_all_fixed(string, "\n\n", omit_no_match = TRUE)[[
        1L
      ]][, "end"],

      # Note, stri_opts_brkiter 'type = line_break' is really about finding
      # candidates line break for the purpose of line wrapping a string, not
      # about finding actual new line boundaries. `type = line_break` might be
      # more suitable than `type = word` for our purpose here.
      # stri_split_lines() does more comprehensive identification of line
      # breaks, but isn't exported as a boundary detector. Most text passing
      # through here is expected to have been normalized as markdown already...
      line_break = stri_locate_all_fixed(string, "\n", omit_no_match = TRUE)[[
        1L
      ]][, "end"],

      sentence = ,
      word = ,
      character = stri_locate_all_boundaries(
        string,
        type = boundary,
        locale = "@ss=standard"
      )[[1L]][, "end"],
      stop(
        'boundaries values must be one of: "paragraph", "sentence", "line_break", "word", "character" or a stringr pattern'
      )
    )
  }
  locations
}

str_chunk <- function(
  x,
  max_size,
  boundaries = c("paragraph", "sentence", "line_break", "word", "character"),
  trim = TRUE,
  simplify = TRUE
) {
  chunk1 <- function(string, boundary) {
    str_chunk1(
      string,
      candidate_cutpoints = str_locate_boundaries1(string, boundary),
      max_size = max_size,
      trim = trim
    )
  }

  out <- lapply(x, function(string) {
    chunks <- chunk1(string, boundaries[[1L]])

    # iterate on boundaries for chunks that are still too large
    repeat {
      lens <- stri_length(chunks)
      is_over_size <- lens > max_size
      if (!any(is_over_size, na.rm = TRUE)) break
      boundaries <- boundaries[-1L]
      if (!length(boundaries)) break
      chunks <- as.list(chunks)
      chunks[is_over_size] <- lapply(
        chunks[is_over_size],
        chunk1,
        boundaries[[1L]]
      )
      chunks <- unlist(chunks)
      # TODO: recurse and returned nested list of strings if simplify=FALSE
    }
    chunks
  })

  if (simplify) out <- unlist(out)

  out
}


#' Chunk text
#'
#' Functions for chunking text into smaller pieces while preserving meaningful
#' semantics. These functions provide flexible ways to split text based on
#' various boundaries (sentences, words, etc.) while controlling chunk sizes and
#' overlap.
#'
#' @param x A character vector, list of character vectors, or data frame containing a `text` column.
#' @param max_size Integer. The maximum number of characters in each chunk.
#'   Defaults to `1600`, which typically is approximately 400 tokens, or 1 page of text.
#' @param boundaries A sequence of boundary types to use in order until
#'   `max_size` is satisfied. Valid values are `"sentence"`, `"word"`,
#'   `"line_break"`, `"character"`, `"paragraph"`, or a `stringr_pattern` object
#'   like `stringr::fixed()`.
#' @param simplify Logical. If `TRUE`, the output is simplified. If `FALSE`,
#'   returns a vector that has the same length as `x`. If `TRUE`, character
#'   strings are `unlist()`ed, and dataframes are `tidyr::unchop()`ed.
#' @param trim logical, whether to trim leading and trailing whitespace from
#'   strings. Default `TRUE`.
#' @param ... Additional arguments passed to internal functions.
#  TODO:
#  @param overlap Numeric between `0` and `1`. The fraction of overlap between
#    consecutive chunks. Default: 0.
#  @param str_length Function used to calculate string lengths. Pass along a
#'   tokenizer to use `tokens` instead of characters as the count (not fully
#'   implemented yet)
#'
#' @details
#'
#' Chunking is the combination of two fundamental operations:
#' - identifying boundaries: finding character positions where it makes sense to split a string.
#' - extracting slices: extracting substrings using the candidate boundaries to produce chunks that
#'   match the requested `chunk_size` and `chunk_overlap`
#'
#' `ragnar_chunk()` is a higher-level function that does both, identifies boundaries and extracts slices.
#'
#' If you need lower-level control, you can alternatively use the lower-level functions
#' `ragnar_segment()` in combination with `ragnar_chunk_segments()`.
#'
#' `ragnar_segment()`: Splits text at semantic boundaries.
#'
#' `ragnar_chunk_segments()`: Combines text segments into chunks.
#'
#' For most usecases, these two are equivalent:
#' ```r
#' x |> ragnar_chunk()
#' x |> ragnar_segment() |> ragnar_chunk_segments()
#' ```
#'
#' When working with data frames, these functions preserve all columns and use
#' `tidyr::unchop()` to handle the resulting list-columns when `simplify = TRUE`.
#'
#' @return
#' - For character input with `simplify = FALSE`: A list of character vectors
#' - For character input with `simplify = TRUE`: A character vector of chunks
#' - For data frame input with `simplify = FALSE`: A data frame with the same number of rows as the input, where the
#' `text` column transformed into a list of chararacter vectors.
#' - For data frame input with `simplify = TRUE`: Same as a data frame input with `simplify=FALSE`, with the
#'  `text` column expanded by `tidyr::unchop()`
#'
#' @examples
#' # Basic chunking with max size
#' text <- "This is a long piece of text. It has multiple sentences.
#'          We want to split it into chunks. Here's another sentence."
#' ragnar_chunk(text, max_size = 40) # splits at sentences
#'
#' # smaller chunk size: first splits at sentence boundaries, then word boundaries
#' ragnar_chunk(text, max_size = 20)
#'
#' # only split at sentence boundaries. Note, some chunks are oversized
#' ragnar_chunk(text, max_size = 20, boundaries = c("sentence"))
#'
#' # only consider word boundaries when splitting:
#' ragnar_chunk(text, max_size = 20, boundaries = c("word"))
#'
#' # first split at sentence boundaries, then word boundaries,
#' # as needed to satisfy `max_chunk`
#' ragnar_chunk(text, max_size = 20, boundaries = c("sentence", "word"))
#'
#' # Use a stringr pattern to find semantic boundaries
#' ragnar_chunk(text, max_size = 10, boundaries = stringr::fixed(". "))
#' ragnar_chunk(text, max_size = 10, boundaries = list(stringr::fixed(". "), "word"))
#'
#'
#' # Working with data frames
#' df <- data.frame(
#'   id = 1:2,
#'   text = c("First sentence. Second sentence.", "Another sentence here.")
#' )
#' ragnar_chunk(df, max_size = 20, boundaries = "sentence")
#' ragnar_chunk(df$text, max_size = 20, boundaries = "sentence")
#'
#' # Chunking pre-segmented text
#' segments <- c("First segment. ", "Second segment. ", "Third segment. ", "Fourth segment. ")
#' ragnar_chunk_segments(segments, max_size = 20)
#' ragnar_chunk_segments(segments, max_size = 40)
#' ragnar_chunk_segments(segments, max_size = 60)
#'
#' @name ragnar_chunk
#' @rdname ragnar_chunk
#' @export
ragnar_chunk <- function(
  x,
  max_size = 1600L,
  boundaries = c("paragraph", "sentence", "line_break", "word", "character"),
  ...,
  trim = TRUE,
  simplify = TRUE
) {
  if (is.data.frame(x)) {
    check_character(x[["text"]])
    x[["text"]] <- str_chunk(
      x[["text"]],
      max_size = max_size,
      boundaries = boundaries,
      trim = trim,
      ...,
      simplify = FALSE
    )
    if (simplify) x <- tidyr::unchop(x, "text")
  } else {
    boundaries <- as_boundaries_list(boundaries)
    x <- str_chunk(
      x,
      max_size = max_size,
      boundaries = boundaries,
      trim = trim,
      simplify = simplify,
      ...
    )
  }
  x
}

#' @export
#' @rdname ragnar_chunk
ragnar_segment <- function(
  x,
  boundaries = "sentence",
  ...,
  trim = FALSE,
  simplify = TRUE
) {
  if (is.data.frame(x)) {
    check_character(x[["text"]])
    x[["text"]] <- ragnar_segment(
      x[["text"]],
      boundaries = boundaries,
      trim = trim,
      ...,
      simplify = FALSE
    )
    if (simplify) x <- tidyr::unchop(x, "text")
    return(x)
  }

  boundaries <- as_boundaries_list(boundaries)
  check_character(x)
  out <- lapply(x, function(string) {
    cutpoints <- lapply(boundaries, str_locate_boundaries1, string = string) |>
      unlist() |>
      c(1L, stri_length(string)) |>
      sort() |>
      unique()
    segments <- stri_sub(string, drop_last(cutpoints), drop_first(cutpoints))
    if (trim) segments <- stri_trim_both(segments)
    segments
  })

  if (simplify) out <- unlist(out)

  out
}

#' @export
#' @rdname ragnar_chunk
ragnar_chunk_segments <- function(
  x,
  max_size = 1600L,
  ...,
  simplify = TRUE,
  trim = TRUE
) {
  sep <- ""
  if (is.data.frame(x)) {
    stopifnot(is.list(x[["text"]]), all(map_chr(x[["text"]]), is.character))
    x[["text"]] <- ragnar_chunk_segments(
      x[["text"]],
      ...,
      max_size = max_size,
      trim = trim,
      sep = sep,
      simplify = FALSE
    )
    if (simplify) x <- tidyr::unchop(x, "text")
    return(x)
  }
  check_string(sep)
  if (is.list(x)) {
    out <- lapply(x, function(string) {
      str_chunk1(
        stri_flatten(x, collapse = sep),
        candidate_cutpoints = cumsum(stri_length(x) + stri_length(sep)),
        max_size = max_size,
        trim = trim,
        ...
      )
    })
    if (simplify) out <- unlist(out)

    return(out)
  }

  check_character(x)
  str_chunk1(
    stri_flatten(x, sep),
    candidate_cutpoints = cumsum(stri_length(x) + stri_length(sep)),
    max_size = max_size,
    trim = trim,
    ...
  )
}

# ' @export
ragnar_view <- function(x) {
  # concept, pop up widget or page that makes it easy to inspect chunks,
  # for tweaking a chunking approach to a particular corpus. The viewer shows:
  # - distinct boundaries between chunks
  # - print representation of text shown (i.e., cat() not print())
  # - makes it easy to quickly scan across lots of content:
  #     all chunks shown on one page, easy scroll, easy jump between chunks via sidebar?, interactive search?
  # - metadata (other columns like 'h1' , 'h2') shown adjacent to chunks
  .NotYetImplemented()
}


as_boundaries_list <- function(x) {
  if (inherits(x, "stringr_pattern")) {
    list(x)
  } else {
    # TODO: move checks out of str_locate_boundaries1() into here.
    as.list(x)
  }
}
