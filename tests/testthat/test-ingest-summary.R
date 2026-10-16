# Tests for the ingest_summary attribute returned by ragnar_store_ingest()
#
# Issue #202: the function currently returns store invisibly with no per-path
# outcome data. These tests specify the desired contract:
#
#   attr(result, "ingest_summary") is a tibble with columns:
#     - path      <chr>: the input path/URL
#     - status    <chr>: one of "ok", "skipped_exists", "error"
#     - n_chunks  <int>: the number of stored chunks, or zero on error
#     - message   <chr>: the worker error message, or NA

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

  visible <- withVisible(
    ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)
  )
  expect_false(visible$visible)
  result <- visible$value
  expect_identical(result@con, store@con)

  summary <- attr(result, "ingest_summary")
  expect_s3_class(summary, "tbl_df")
  expect_identical(
    vapply(summary, typeof, character(1L)),
    c(
      path = "character", status = "character",
      n_chunks = "integer", message = "character"
    )
  )
})

# --- successful ingestion reports "ok" ----------------------------------------

test_that("successfully ingested paths have status 'ok'", {
  skip_on_cran()
  paths <- local_md_paths(2)
  store <- local_store()

  result <- ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  summary <- attr(result, "ingest_summary")
  expect_identical(
    summary,
    tibble(path = paths, status = "ok", n_chunks = 1L, message = NA_character_)
  )
})

# --- re-ingesting an existing origin reports "skipped_exists" -----------------

test_that("paths already in the store have status 'skipped_exists'", {
  skip_on_cran()
  paths <- local_md_paths(2)
  store <- local_store()
  writeLines(rep("Longer content for several chunks.", 100), paths[[1L]])

  # first ingest
  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)
  counts <- DBI::dbGetQuery(
    store@con,
    "SELECT origin, COUNT(*)::INTEGER AS n_chunks FROM chunks GROUP BY origin"
  )

  # second ingest: all paths already exist
  result <- ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  summary <- attr(result, "ingest_summary")
  expect_identical(
    summary,
    tibble(
      path = paths,
      status = "skipped_exists",
      n_chunks = counts$n_chunks[match(paths, counts$origin)],
      message = NA_character_
    )
  )
  expect_true(any(summary$n_chunks > 1L))
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
  expect_identical(
    summary,
    tibble(
      path = c(old_paths, new_paths),
      status = c("skipped_exists", "ok"),
      n_chunks = 1L,
      message = NA_character_
    )
  )
})

# --- failed prepare() reports "error" -----------------------------------------

test_that("paths where prepare() errors have status 'error'", {
  skip_on_cran()
  paths <- local_md_paths(1)
  store <- local_store()

  bad_prepare <- function(path) stop("deliberate prepare error")

  captured <- NULL
  withCallingHandlers(
    result <- ragnar_store_ingest(
      store, paths,
      prepare = bad_prepare,
      progress = FALSE,
      build_index = FALSE
    ),
    ragnar_ingest_worker_error = function(w) {
      captured <<- w
      invokeRestart("muffleWarning")
    }
  )

  summary <- attr(result, "ingest_summary")
  expect_identical(
    summary,
    tibble(
      path = paths,
      status = "error",
      n_chunks = 0L,
      message = conditionMessage(captured)
    )
  )
  expect_match(summary$message, "deliberate prepare error", fixed = TRUE)
})

# --- empty paths returns empty summary ----------------------------------------

test_that("empty paths vector returns an empty ingest_summary", {
  skip_on_cran()
  store <- local_store()

  result <- ragnar_store_ingest(store, character(0), progress = FALSE, build_index = FALSE)

  summary <- attr(result, "ingest_summary")
  expect_identical(
    summary,
    tibble(
      path = character(), status = character(),
      n_chunks = integer(), message = character()
    )
  )
})

test_that("ingest_summary preserves input paths, ordering, and duplicates", {
  skip_on_cran()

  paths <- c("second-input", "first-input", "second-input", "bad-input")
  store <- ragnar_store_create(embed = NULL)
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
  prepare <- function(path) {
    if (path == "bad-input") {
      stop("deliberate prepare error")
    }
    text <- paste(rep("Some content for several chunks.", 20), collapse = "\n\n")
    ragnar::MarkdownDocument(text, origin = paste0("resolved-", path)) |>
      ragnar::markdown_chunk(target_size = 50, target_overlap = 0)
  }

  captured <- NULL
  withCallingHandlers(
    result <- ragnar_store_ingest(
      store, paths, prepare = prepare,
      n_workers = 2L, progress = FALSE, build_index = FALSE
    ),
    ragnar_ingest_worker_error = function(w) {
      captured <<- w
      invokeRestart("muffleWarning")
    }
  )

  counts <- DBI::dbGetQuery(
    store@con,
    "SELECT origin, COUNT(*)::INTEGER AS n_chunks FROM chunks GROUP BY origin"
  )
  expected_counts <- counts$n_chunks[
    match(paste0("resolved-", paths[1:3]), counts$origin)
  ]
  expect_identical(
    attr(result, "ingest_summary"),
    tibble(
      path = paths,
      status = c("ok", "ok", "ok", "error"),
      n_chunks = c(expected_counts, 0L),
      message = c(rep(NA_character_, 3L), conditionMessage(captured))
    )
  )
  expect_true(all(expected_counts > 1L))
  expect_equal(nrow(counts), 2L)
})
