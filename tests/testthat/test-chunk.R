test_that("pick_cut_positions_ works", {
  # Basic cases
  candidates <- c(1L, 4L, 7L, 8L, 10L)
  expect_equal(pick_cut_positions(candidates, 3L), c(1L, 4L, 7L, 10L))
  expect_equal(pick_cut_positions(candidates, 5L), c(1L, 4L, 8L, 10L))
  expect_equal(pick_cut_positions(candidates, 7L), c(1L, 8L, 10L))

  # Edge cases
  expect_equal(pick_cut_positions(1L, 5L), 1L)
  expect_equal(pick_cut_positions(1L:2L, 5L), 1L:2L)
  expect_equal(pick_cut_positions(1L:3L, 5L), c(1L, 3L))
  expect_equal(pick_cut_positions(c(1L, 1L:3L), 5L), c(1L, 3L))
  expect_equal(pick_cut_positions(c(1L, 1L), 5L), c(1L))
  expect_equal(pick_cut_positions(c(1L, 2L), 5L), c(1L, 2L))
  expect_equal(length(pick_cut_positions(integer(0), 5L)), 0L)

  # Large gaps
  candidates <- c(1L, 100L, 200L, 300L)
  expect_equal(pick_cut_positions(candidates, 50L), candidates)

  # First/last position inclusion
  candidates <- c(1L, 5L, 10L, 15L, 20L)
  result <- pick_cut_positions(candidates, 8L)
  expect_equal(result[1], 1L)
  expect_equal(result[length(result)], 20L)

  # Consecutive positions
  expect_equal(pick_cut_positions(1:5, 2L), c(1L, 3L, 5L))
})

test_that("markdown_chunk() handles NFD combining characters correctly", {
  # base string uses precomposed characters (NFC); nfd_text is the same
  # string decomposed into base char + combining mark (NFD). Both must
  # produce identical chunk boundaries and text.
  base_text <- "Long [aā], [ã], [eē], [ẽ] are frequent."
  nfd_text <- stringi::stri_trans_nfd(base_text)

  md <- paste(
    "## TEST 1",
    "Some unrelated introductory paragraph.",
    "## Look at me",
    nfd_text,
    "## TEST 2",
    "Another unrelated trailing paragraph.",
    sep = "\n\n"
  )

  chunks <- markdown_chunk(
    md,
    target_size = NA,
    segment_by_heading_levels = 1:6
  )

  # The chunk under "## Look at me" should contain the full combining-character
  # text, not be truncated/misaligned partway through it.
  look_at_me_chunk <- chunks$text[grepl("Look at me", chunks$text)]
  expect_length(look_at_me_chunk, 1)
  expect_true(grepl(nfd_text, look_at_me_chunk, fixed = TRUE))

  # There should still be exactly 3 segments/chunks, matching the 3 headings.
  expect_equal(nrow(chunks), 3)

  # The final chunk (## TEST 2) should not have been swallowed by the
  # preceding one, which would indicate the end offset ran past the segment.
  expect_true(any(grepl("TEST 2", chunks$text, fixed = TRUE)))
})
