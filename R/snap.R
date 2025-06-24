# Snap every element of x to the nearest candidate boundary
# whenever the gap is ≤ max_dist.
#
# ── Arguments ────────────────────────────────────────────────
# x            sorted numeric vector of positions to adjust
# candidates   sorted numeric vector of snap candidates
# max_dist     non-negative scalar threshold
#
# ── Returns ─────────────────────────────────────────────────
# numeric vector — same length as x.
snap_nearest <- function(x, candidates, max_dist) {
  left_idx <- findInterval(
    x,
    candidates,
    all.inside = TRUE,
    checkSorted = FALSE,
    checkNA = FALSE
  )
  right_idx <- left_idx + 1L

  left <- candidates[left_idx]
  right <- candidates[right_idx]

  dist_left <- abs(x - left)
  dist_right <- abs(x - right)
  left_closest <- dist_left < dist_right

  pick <- ifelse(left_closest, left, right)
  dist <- ifelse(left_closest, dist_left, dist_right)
  pick[dist > max_dist] <- NA
  pick
}

# Return every chunk’s **inclusive start** and **exclusive end** index
# for a region [range_start, range_end].
#
# range_start, range_end   inclusive bounds of the text to chop
# chunk_size               width of each chunk in characters
# overlap_fraction         proportion of *chunk_size* to overlap (0 ≤ f < 1)
#
# The result fully covers the region, the last chunk is right-aligned,
# and ends are “Python style” (exclusive).
chunk_one_segment <- function(
  start = 1L,
  end,
  target_chunk_size = 1600L,
  target_overlap = 0.5
) {
  # ensure right-aligned last chunk
  last_start <- end - target_chunk_size + 1L
  if (last_start < start) {
    return(data_frame(start = as.integer(start), end = as.integer(end)))
  }

  starts <- round(seq.int(
    from = start,
    to = last_start,
    by = target_chunk_size * (1 - target_overlap)
  ))
  if (last(starts) != last_start) {
    starts[length(starts) + 1L] <- last_start
  }

  ends <- starts + (target_chunk_size - 1L)
  data_frame(start = as.integer(starts), end = as.integer(ends))
}

make_chunk_targets <- function(
  md_len,
  segment_breaks = integer(),
  target_chunk_size = 1600L,
  target_overlap = 0.5
) {
  boundries <- c()

  segment_starts <- unique(c(1L, as.integer(segment_breaks)))
  segment_ends <- c(segment_starts[-1] - 1L, as.integer(md_len))

  list_rbind(
    map2(
      segment_starts,
      segment_ends,
      chunk_one_segment,
      target_chunk_size = target_size,
      target_overlap = target_overlap
    ),
    ptype = data_frame(start = integer(), end = integer())
  )
}

prepend <- function(body, head) {
  c(head, body)
}


get_markdown_headings <- function(
  md,
  md_positions = get_markdown_source_positions(md, type = "heading")
) {
  md_positions |>
    filter(type == "heading") |>
    mutate(
      text = stri_sub(md, start, end),
      level = stri_locate_first_regex(text, "[^#]")[, "start"] - 1L
    )
}


ragnar_chunk_v3 <- function(
  md,
  target_size = 1600L,
  target_overlap = .5,
  max_snap_dist = target_size * target_overlap / 3,
  pre_segment_heading_levels = 2L
) {
  if (max_snap_dist > (target_size * target_overlap / 2)) {
    warning("might result in gaps")
  }

  md_len <- stri_length(md)

  md_positions <-
    get_markdown_source_positions(md, type = c("heading", "paragraph"))

  md_headings <- get_markdown_headings(md, md_positions)

  segment_breaks <-
    filter(md_headings, level <= pre_segment_heading_levels)$start
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

  # pre-snap endcaps to doc limits
  stopifnot(snap_table$from[c(1, nrow(snap_table))] == c(1, md_len + 1L))
  snap_table$to[c(1, nrow(snap_table))] <- c(1L, md_len + 1L)

  for (type in c("heading", "paragraph", "sentence", "word", "char")) {
    unsnapped <- is.na(snap_table$to)
    if (!any(unsnapped)) {
      break
    }
    candidates <- switch(
      type,
      heading = md_headings$start,
      paragraph = filter(md_positions, type == "paragraph")$start,
      sentence = str_locate_boundary_starts1(md, "sentence"),
      line = stri_locate_all_fixed(md, "\n")[[1L]][, "start"],
      word = str_locate_boundary_starts1(md, "line_break"),
      char = {
        snap_table <- snap_table |>
          mutate(snapped = coalesce(snapped, target))
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
    ) |>
    # remove chunks that may have snapped to the same position, preferring
    # the larger chunk
    slice_max(end, by = start) |>
    slice_min(start, by = end) |>
    arrange(start)

  check_uncovered_gaps(chunks, md_len)

  # TODO: for pathological inputs with an extremely long heading or
  # max_snap_dist==0, figure out what to do if the chunk starts in the
  # middle of a heading
  out <- chunks |>
    rename_with(~ paste0("chunk_", .x)) |>
    tidyr::crossing(heading_level = 1:6) |>
    left_join(
      md_headings |> rename_with(~ paste0("heading_", .x)),
      join_by(
        heading_level,
        closest(chunk_start > heading_start)
      )
    ) |>
    arrange(chunk_start, heading_level) |>
    tidyr::nest(heading = starts_with("heading_"), .names_sep = "_") |>
    # mutate(heading = lapply(heading, tidyr::drop_na)) |>
    mutate(
      headings = map_chr(heading, \(h) {
        stri_flatten(h$text, "\n", na_empty = TRUE, omit_empty = TRUE)
      }),
      heading = NULL
    ) |>
    rename_with(\(nm) stri_replace_first_regex(nm, "^chunk_", "")) |>
    mutate(text = stri_sub(md, start, end))

  out
}


check_uncovered_gaps <- function(chunks, me_len) {
  # still need to handle NAs, backfills for missing segments
  if (anyNA(chunks)) {
    stop("NA encountered")
  }

  delta <- integer(md_len + 1L)
  add(delta[chunks$start]) <- 1L
  add(delta[chunks$end + 1]) <- -1L
  coverage <- drop_last(cumsum(delta))
  if (!isFALSE(any(coverage == 0))) {
    print(rle(coverage))
    browser()
    stop("uncovered chunks")
  }
}


ragnar_segment <- function(md, tags = c("h1", "h2")) {}

augment_chunk_headings <- function(chunks, ...) {}

## ?? set a 'min_chunk_size` that will merge adjacent chunks (e.g. from segmenting)??
## ?? should ragnar_chunk should take a pre-segmented dataframe too maybe??
## standardize on colnames:
##  - origin
##  - doc_text
##  - start
##  - end
##  - ... other cols (headings automatically bundled as extra cols in ... ? extra cols automatically prepended in embed? )
