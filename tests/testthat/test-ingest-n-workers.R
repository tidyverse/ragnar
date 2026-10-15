## Tests for #206: n_workers default formula correctness

# The formula is:
#   min(detectCores(), nrow(paths) %/% 4L) |> min(8L) |> max(2L)
#
# We test the internal helper directly by calling it with mocked
# detectCores values. The production change that would make these tests
# fail is: restoring max(detectCores(), 4L) in step 1.

compute_n_workers <- function(n_paths, n_cores) {
  ragnar:::compute_default_n_workers(n_paths = n_paths, n_cores = n_cores)
}

test_that("n_workers uses min(cores, pages/4) not max(cores, 4)", {
  # On a 2-core machine with 40 pages:
  # correct:  min(2, 10) = 2  ->  min(2, 8) = 2  ->  max(2, 2) = 2
  # buggy:    max(2, 4)  = 4  ->  min(4, 10, 8)  ->  max(4, 2) = 4
  expect_equal(compute_n_workers(n_paths = 40, n_cores = 2), 2L)
})

test_that("n_workers caps at 8 regardless of core count", {
  # 32-core machine, 100 pages -> min(32, 25) = 25 -> min(25, 8) = 8
  expect_equal(compute_n_workers(n_paths = 100, n_cores = 32), 8L)
})

test_that("n_workers floors at 2 with very few pages", {
  # 16-core machine, 3 pages -> min(16, 0) = 0 -> min(0, 8) = 0 -> max(0, 2) = 2
  expect_equal(compute_n_workers(n_paths = 3, n_cores = 16), 2L)
})

test_that("n_workers is bounded by pages/4 on medium machines", {
  # 8-core machine, 12 pages -> min(8, 3) = 3 -> min(3, 8) = 3 -> max(3, 2) = 3
  expect_equal(compute_n_workers(n_paths = 12, n_cores = 8), 3L)
})

test_that("n_workers on single-core machine still returns at least 2", {
  # 1-core machine, 40 pages -> min(1, 10) = 1 -> min(1, 8) = 1 -> max(1, 2) = 2
  expect_equal(compute_n_workers(n_paths = 40, n_cores = 1), 2L)
})

test_that("n_workers on 4-core machine with 20 pages returns 4", {
  # correct:  min(4, 5) = 4  ->  min(4, 8) = 4  ->  max(4, 2) = 4
  # buggy:    max(4, 4) = 4  ->  min(4, 5, 8) = 4  (coincidentally same)
  # This case passes even with the bug, kept for documentation.
  expect_equal(compute_n_workers(n_paths = 20, n_cores = 4), 4L)
})
