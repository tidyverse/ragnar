#' Chunk a Markdown document
#'
#' `markdown_chunk()` splits a single Markdown string into shorter optionally
#' overlapping chunks while nudging cut points to the nearest sensible boundary
#' (heading, paragraph, sentence, line, word, or character). It returns a tibble
#' recording the character ranges, headings context, and text for each chunk.
#'
#' @param md A `MarkdownDocument`, or a length-one character vector containing
#'   Markdown.
#' @param target_size Integer. Target chunk size in characters. Default: 1600
#'   (\eqn{\approx} 400 tokens, or 1 page of text). Actual chunk size may differ
#'   from the target by up to `2 * max_snap_dist`. When set to `NULL`, `NA` or
#'   `Inf` and used with `segment_by_heading_levels`, chunk size is unbounded
#'   and each chunk corresponds to a segment.
#' @param target_overlap Numeric in `[0, 1)`. Fraction of desired overlap
#'   between successive chunks. Default: `0.5`. Even when `0`, some overlap can
#'   occur because the last chunk is anchored to the document end.
#' @param max_snap_dist Integer. Furthest distance (in characters) a cut point
#'   may move to reach a semantic boundary. Defaults to one third of the stride
#'   size between target chunk starts. Chunks that end up on identical
#'   boundaries are merged.
#' @inheritParams rlang::args_dots_empty
#' @param context Logical. Add a `context` column containing the Markdown
#'   headings in scope at each chunk start. Default: `TRUE`.
#' @param segment_by_heading_levels Integer vector with possible values `1:6`.
#'   Headings at these levels are treated as segment boundaries; chunking is
#'   performed independently for each segment. No chunk will overlap a segment
#'   boundary, and any future deoverlapping will not combine segments. Each
#'   segment will have a chunk that starts at the segment start and a chunk that
#'   ends at the segment end (these may be the same chunk or overlap
#'   substantially if the segment is short). Default: disabled.
#' @param text Logical. If `TRUE`, include a `text` column with the chunk
#'   contents. Default: `TRUE`.
#'
#' @return
#'
#' A [`MarkdownDocumentChunks`] object, which is a tibble (data.frame) with with
#' columns `start` `end`, and optionally `context` and `text`. It also has a
#' `@document` property, which is the input `md` document (potentially
#' normalized and converted to a [`MarkdownDocument`]).
#'
#' @seealso [ragnar_chunks_view()] to interactively inspect the output of
#'   `markdown_chunk()`. See also [MarkdownDocumentChunks()] and
#'   [MarkdownDocument()], where the input and return value of
#'   `markdown_chunk()` are described more fully.
#'
#' @export
#' @examples
#' md <- "
#' # Title
#'
#' ## Section 1
#'
#' Some text that is long enough to be chunked.
#'
#' A second paragraph to make the text even longer.
#'
#' ## Section 2
#'
#' More text here.
#'
#' ### Section 2.1
#'
#' Some text under a level three heading.
#'
#' #### Section 2.1.1
#'
#' Some text under a level four heading.
#'
#' ## Section 3
#'
#' Even more text here.
#' "
#'
#' markdown_chunk(md, target_size = 40)
#' markdown_chunk(md, target_size = 40, target_overlap = 0)
#' markdown_chunk(md, target_size = NA, segment_by_heading_levels = c(1, 2))
#' markdown_chunk(md, target_size = 40, max_snap_dist = 100)
markdown_chunk <- function(
  md,
  target_size = 1600L,
  target_overlap = .5,
  ...,
  max_snap_dist = target_size * (1 - target_overlap) / 3,
  segment_by_heading_levels = integer(),
  context = TRUE,
  text = TRUE
) {
  # arg checks
  check_number_whole(
    target_size,
    min = 1,
    allow_infinite = TRUE,
    allow_na = TRUE,
    allow_null = TRUE
  )
  if (is.null(target_size) || is.na(target_size)) {
    target_size <- Inf
  }
  check_number_decimal(target_overlap, min = 0, max = 1)
  segment_by_heading_levels <- sort(unique(as.integer(c(
    segment_by_heading_levels,
    list(...)$pre_segment_heading_levels
  ))))
  if ("pre_segment_heading_levels" %in% ...names()) {
    warning(
      "`pre_segment_heading_levels` has been renamed to `segment_by_heading_levels`"
    )
  } else {
    check_dots_empty()
  }
  if (!S7_inherits(md, MarkdownDocument)) {
    md <- convert(md, MarkdownDocument)
  }
  # end arg checks

  md_len <- stri_length(md)
  md_positions <-
    markdown_node_positions(md, type = c("heading", "paragraph", "code_block"))
  md_headings <- markdown_headings(md, md_positions)

  segment_breaks <-
    filter(md_headings, level %in% as.integer(segment_by_heading_levels))$start
  chunk_targets <- make_chunk_targets(
    md_len = md_len,
    segment_breaks = segment_breaks,
    target_overlap = target_overlap,
    target_chunk_size = target_size
  )

  snap_table <- data_frame(
    from = sort.int(unique(c(chunk_targets$start, chunk_targets$end + 1L))),
    to = NA_integer_
  )

  # snap endcaps to doc limits
  stopifnot(snap_table$from[c(1, nrow(snap_table))] == c(1, md_len + 1L))
  snap_table$to[c(1, nrow(snap_table))] <- c(1L, md_len + 1L)

  boundary_types <-
    c("heading", "paragraph", "sentence", "line", "word", "char")
  for (boundary_type in boundary_types) {
    unsnapped <- is.na(snap_table$to)
    if (!any(unsnapped)) {
      break
    }
    candidates <- switch(
      boundary_type,
      heading = md_headings$start,
      paragraph = md_positions |>
        filter(type %in% c("paragraph", "code_block")) |>
        _$start,
      sentence = str_locate_boundary_starts1(md, "sentence"),
      line = stri_locate_all_fixed(md, "\n")[[1L]][, "start"],
      word = str_locate_boundary_starts1(md, "line_break"),
      char = {
        snap_table <- snap_table |>
          mutate(to = coalesce(to, from))
        break
      }
    )

    snap_table$to[unsnapped] <- snap_nearest(
      snap_table$from[unsnapped],
      candidates,
      max_dist = max_snap_dist
    )
  }

  chunks <- chunk_targets |>
    mutate(
      start = snap_table$to[match(start, snap_table$from)],
      end = snap_table$to[match(end + 1L, snap_table$from)] - 1L,
      ## when max_snap_dist > chunk_size, then the start and end can snap to the
      ## same boundary. If that happens, scan ahead to the next boundary in the
      ## snap table and adjust the end to use that.
      end = ifelse(
        start <= end,
        end,
        map_int(end, \(e) {
          snap_table$to[which.max(snap_table$to > (e + 1L))] - 1L
        })
      )
    ) |>
    # remove chunks that may have snapped to the same position, preferring
    # the larger chunk
    slice_max(end, by = start, with_ties = FALSE, n = 1L) |>
    slice_min(start, by = end, with_ties = FALSE, n = 1L) |>
    arrange(start)

  check_uncovered_gaps(chunks, md_len)

  if (context) {
    chunks$context <-
      markdown_position_headings(md, chunks$start, md_headings) |>
      map_chr(\(h) {
        stri_flatten(h$text, "\n", na_empty = TRUE, omit_empty = TRUE)
      })
  }

  if (text) {
    chunks <- chunks |> mutate(text = stri_sub(md, start, end))
  }

  MarkdownDocumentChunks(chunks, document = md)
}


markdown_node_positions <- function(md, type = NULL, text = FALSE) {
  check_string(md)
  if (md == "") {
    return(data_frame(type = character(), start = integer(), end = integer()))
  }

  doc <- md |>
    commonmark::markdown_xml(
      sourcepos = TRUE,
      extensions = TRUE,
      footnotes = TRUE,
      normalize = TRUE
    ) |>
    charToRaw() |>
    xml2::read_xml(encoding = "UTF-8")

  elements <- doc |> xml_find_all(xpath = "//*[@sourcepos]")

  df <- data_frame(
    type = elements |> xml_name(),
    sourcepos = elements |> xml_attr("sourcepos")
  )

  if (length(type)) {
    df <- df[df$type %in% unique(c(type)), ]
  }

  # commonmark returns positions as line:byte-line:byte
  # e.g., 52:1-52:20
  pos <- df$sourcepos |>
    stri_split_charclass("[-:]", n = 4L, simplify = TRUE)
  storage.mode(pos) <- "integer"
  colnames(pos) <- c(
    "start_line",
    "start_line_byte",
    "end_line",
    "end_line_byte"
  )
  df$sourcepos <- NULL

  # prepare to convert byte index to a char index
  chars <- stri_split_boundaries(md, type = "character")[[1L]]
  char_lens <- stri_numbytes(chars)
  char_startbytes <- cumsum(c(1L, char_lens))
  # TODO: this assume "\n" is the line sep. crlf should have been normalized out
  # by read_as_markdown(), but we should be robust to crlf here in case document was
  # read another way.
  line_startbyte <- c(1L, char_startbytes[chars == "\n"] + 1L)

  # # -1 because both start_line and start_line_byte are inclusive.
  # E.g, map "1:1" to 1
  start <- line_startbyte[pos[, "start_line"]] + pos[, "start_line_byte"] - 1L
  end <- line_startbyte[pos[, "end_line"]] + pos[, "end_line_byte"] - 1L

  df$start <- match(start, char_startbytes)
  df$end <- match(end + 1L, char_startbytes) - 1L
  if (text) {
    df$text <- stri_sub(md, df$start, df$end)
  }

  df |> arrange(start, end)
}

str_locate_boundary_starts1 <- function(string, type) {
  check_string(string)
  stri_locate_all_boundaries(
    string,
    type = type,
    locale = "@ss=standard"
  )[[1L]][, "start"]
}


snap_nearest <- function(x, candidates, max_dist = NULL) {
  n_candidates <- length(candidates)
  if (n_candidates == 0L) {
    return(rep(NA, length(x)))
  }
  if (n_candidates == 1L) {
    out <- rep(candidates, length(x))
    if (!is.null(max_dist)) {
      out[abs(x - out) > max_dist] <- NA
    }
    return(out)
  }
  left_idx <- findInterval(x, candidates, all.inside = TRUE)
  right_idx <- left_idx + 1L

  left <- candidates[left_idx]
  right <- candidates[right_idx]

  dist_left <- abs(x - left)
  dist_right <- abs(x - right)
  left_closer <- dist_left < dist_right
  pick <- ifelse(left_closer, left, right)

  if (!is.null(max_dist)) {
    dist <- ifelse(left_closer, dist_left, dist_right)
    pick[dist > max_dist] <- NA
  }
  pick
}

make_segment_chunk_targets <- function(
  start = 1L,
  end,
  chunk_size = 1600L,
  overlap = 0.5
) {
  last_start <- end - chunk_size + 1L
  if (last_start < start) {
    # segment shorter than target chunk size
    return(data_frame(start = as.integer(start), end = as.integer(end)))
  }

  starts <- round(seq.int(
    from = start,
    to = last_start,
    by = chunk_size * (1 - overlap)
  ))
  if (last(starts) != last_start) {
    # ensure right-aligned last chunk
    starts[length(starts) + 1L] <- last_start
  }

  ends <- starts + (chunk_size - 1L)
  data_frame(start = as.integer(starts), end = as.integer(ends))
}

make_chunk_targets <- function(
  md_len,
  segment_breaks = integer(),
  target_chunk_size = 1600L,
  target_overlap = 0.5
) {
  segment_starts <- unique(c(1L, as.integer(segment_breaks)))
  segment_ends <- c(segment_starts[-1] - 1L, as.integer(md_len))

  vec_rbind(
    !!!map2(
      segment_starts,
      segment_ends,
      make_segment_chunk_targets,
      chunk_size = target_chunk_size,
      overlap = target_overlap
    ),
    ptype = data_frame(start = integer(), end = integer())
  )
}


markdown_headings <- function(
  md,
  md_positions = markdown_node_positions(md, type = "heading")
) {
  md_positions |>
    filter(type == "heading") |>
    mutate(
      text = stri_sub(md, start, end),
      level = stri_locate_first_regex(text, "[^#]")[, "start"] - 1L
    )
}


check_uncovered_gaps <- function(chunks, md_len) {
  # still need to handle NAs, backfills for missing segments
  if (anyNA(chunks)) {
    stop("NA encountered")
  }

  delta <- integer(md_len + 1L)
  add(delta[chunks$start]) <- 1L
  add(delta[chunks$end + 1]) <- -1L
  coverage <- drop_last(cumsum(delta))
  if (!isFALSE(any(coverage == 0))) {
    # print(rle(coverage))
    stop(
      "markdown_chunk() failed to generate chunks. Please report
         this error https://github.com/tidyverse/ragnar/issues"
    )
  }
}


markdown_position_headings <- function(
  md,
  position,
  md_headings = markdown_headings(md)
) {
  # TODO: still need to clarify what happens if the chunk starts in the middle
  # of a heading, e.g., for pathological inputs with an extremely long headings
  # or max_snap_dist==0
  df <- left_join(
    tidyr::crossing(position, heading_level = 1:6),
    md_headings |>
      select(level, start, end, text) |>
      rename_with(\(nms) paste0("heading_", nms)),
    by = join_by(
      heading_level,
      closest(position > heading_start)
    )
  ) |>
    arrange(position, heading_level) |>
    tidyr::nest(heading = starts_with("heading_"), .names_sep = "_")

  df |>
    mutate(
      nearest_preceeding_heading_level = md_headings$level[
        findInterval(
          position,
          md_headings$start,
          left.open = TRUE
        ) |>
          na_if(0)
      ],
      start_of_heading_level = md_headings$level[
        match(position, md_headings$start)
      ],
      max_applicable_level = coalesce(
        start_of_heading_level - 1L,
        nearest_preceeding_heading_level,
        99L
      ),
      heading = map2(heading, max_applicable_level, \(h, l) {
        h[!is.na(h$start) & h$level <= l, ]
      })
    ) |>
    _$heading
}

utils::globalVariables(c(
  # markdown_chunk() and helpers
  "level",
  "type",
  "to",
  "from",
  "start",
  "end",
  "text",
  "heading_level",
  "heading_start",
  "closest",
  "start_of_heading_level",
  "nearest_preceeding_heading_level",
  "heading",
  "max_applicable_level"
))
