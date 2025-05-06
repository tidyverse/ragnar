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
