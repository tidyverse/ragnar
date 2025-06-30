#' @export
ragnar_chunk <- function(x, ...) {
  stopifnot(is.data.frame(x), c("origin", "text") %in% names(x))
  x |>
    rename(doc_text = text) |>
    mutate(
      chunk = lapply(doc_text, \(md) {
        markdown_chunk(md, ..., headings = TRUE, text = TRUE)
      })
    ) |>
    unnest(chunk, names_sep = "_")
}


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
  if (isTRUE(is.na(string))) {
    return(NA_character_)
  }
  check_string(string, allow_na = TRUE)
  string_len <- stri_length(string)
  if (string_len <= max_size) {
    return(string)
  }

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

  if (trim) {
    chunks <- stri_trim_both(chunks)
  }

  chunks <- chunks[nzchar(chunks)]

  chunks
}

str_locate_boundaries1 <- function(string, boundary) {
  check_string(string)
  if (inherits(boundary, "stringr_pattern")) {
    locations <- stringr::str_locate_all(string, boundary)[[1L]][, "end"]
    return(locations)
  }
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
    paragraph = stri_locate_all_fixed(
      string,
      "\n\n",
      omit_no_match = TRUE
    )[[1L]][, "end"],

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
      if (!any(is_over_size, na.rm = TRUE)) {
        break
      }
      boundaries <- boundaries[-1L]
      if (!length(boundaries)) {
        break
      }
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

  if (simplify) {
    out <- unlist(out)
  }

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
#' @export
ragnar_chunk_legacy <- function(
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
#' @rdname ragnar_chunk_legacy
ragnar_segment_legacy <- function(
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
    if (simplify) {
      x <- tidyr::unchop(x, "text")
    }
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
    if (trim) {
      segments <- stri_trim_both(segments)
    }
    segments
  })

  if (simplify) {
    out <- unlist(out)
  }

  out
}

#' @export
#' @rdname ragnar_chunk_legacy
ragnar_chunk_segments_legacy <- function(
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
    if (simplify) {
      x <- tidyr::unchop(x, "text")
    }
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
    if (simplify) {
      out <- unlist(out)
    }

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

  if (trim) {
    splits <- stri_trim_both(splits)
  } # drops names

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
  if (!length(segment_by) || base::setequal(segment_by, frame_by)) {
    frame[["tag"]] <- NULL
  }
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
