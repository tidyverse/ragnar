# Tests for the ingest_summary attribute returned by ragnar_store_ingest()
#
# Issue #202: the function currently returns store invisibly with no per-path
# outcome data. These tests specify the desired contract:
#
#   attr(result, "ingest_summary") is a tibble with columns:
#     - origin  <chr>: the path/URL that was processed
#     - status  <chr>: one of "ok", "skipped_exists", "error"

local_md_paths <- function(n = 2) {
  paths <- replicate(n, {
    p <- tempfile(fileext = ".md")
    writeLines(c("# Test", "", "Some content."), p)
    p
  })
  paths
}

local_store <- function() {
  ragnar_store_create(
    embed = \(x) matrix(nrow = length(x), ncol = 4L, stats::runif(4L * length(x)))
  )
}

# --- ingest_summary attribute is present and well-formed ----------------------

test_that("ragnar_store_ingest() returns an ingest_summary attribute", {
  skip_on_cran()
  paths <- local_md_paths(2)
  store <- local_store()

  result <- ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  summary <- attr(result, "ingest_summary")
  expect_s3_class(summary, "data.frame")
  expect_named(summary, c("origin", "status"))
})

# --- successful ingestion reports "ok" ----------------------------------------

test_that("successfully ingested paths have status 'ok'", {
  skip_on_cran()
  paths <- local_md_paths(2)
  store <- local_store()

  result <- ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  summary <- attr(result, "ingest_summary")
  expect_setequal(summary$origin, paths)
  expect_true(all(summary$status == "ok"))
})

# --- re-ingesting an existing origin reports "skipped_exists" -----------------

test_that("paths already in the store have status 'skipped_exists'", {
  skip_on_cran()
  paths <- local_md_paths(2)
  store <- local_store()

  # first ingest
  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  # second ingest: all paths already exist
  result <- ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  summary <- attr(result, "ingest_summary")
  expect_setequal(summary$origin, paths)
  expect_true(all(summary$status == "skipped_exists"))
})

# --- mix of new and existing paths --------------------------------------------

test_that("ingest_summary distinguishes new paths from existing ones", {
  skip_on_cran()
  old_paths <- local_md_paths(1)
  new_paths <- local_md_paths(1)
  store <- local_store()

  ragnar_store_ingest(store, old_paths, progress = FALSE, build_index = FALSE)

  result <- ragnar_store_ingest(
    store, c(old_paths, new_paths),
    progress = FALSE, build_index = FALSE
  )

  summary <- attr(result, "ingest_summary")
  expect_equal(nrow(summary), 2L)
  expect_equal(
    summary$status[summary$origin == old_paths],
    "skipped_exists"
  )
  expect_equal(
    summary$status[summary$origin == new_paths],
    "ok"
  )
})

# --- failed prepare() reports "error" -----------------------------------------

test_that("paths where prepare() errors have status 'error'", {
  skip_on_cran()
  paths <- local_md_paths(1)
  store <- local_store()

  bad_prepare <- function(path) stop("deliberate prepare error")

  # ragnar_store_ingest() issues a warning per failing worker, not an error
  result <- suppressWarnings(
    ragnar_store_ingest(
      store, paths,
      prepare = bad_prepare,
      progress = FALSE,
      build_index = FALSE
    )
  )

  summary <- attr(result, "ingest_summary")
  expect_equal(nrow(summary), 1L)
  expect_equal(summary$status, "error")
})

# --- empty paths returns empty summary ----------------------------------------

test_that("empty paths vector returns an empty ingest_summary", {
  skip_on_cran()
  store <- local_store()

  result <- ragnar_store_ingest(store, character(0), progress = FALSE, build_index = FALSE)

  summary <- attr(result, "ingest_summary")
  expect_s3_class(summary, "data.frame")
  expect_named(summary, c("origin", "status"))
  expect_equal(nrow(summary), 0L)
})
