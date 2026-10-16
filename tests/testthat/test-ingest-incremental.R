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

test_that("unchanged local files get status 'skipped_unchanged' on re-ingest", {
  skip_on_cran()
  paths <- local_md_paths(1)
  store <- local_store()

  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  # second ingest: file unchanged
  result <- ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)
  summary <- attr(result, "ingest_summary")
  expect_equal(summary$status, "skipped_unchanged")
})

test_that("local file freshness preserves subsecond modification times", {
  skip_on_cran()
  paths <- local_md_paths(1)
  store <- local_store()
  mtime <- as.POSIXct("2026-10-16 12:00:00", tz = "UTC")
  Sys.setFileTime(paths[[1]], mtime + 0.1234565)

  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  result <- ragnar_store_ingest(
    store, paths,
    prepare = function(path) stop("Unchanged timestamps should skip preparation."),
    progress = FALSE, build_index = FALSE
  )
  expect_equal(attr(result, "ingest_summary")$status, "skipped_unchanged")

  writeLines(c("# Modified", "", "New content."), paths[[1]])
  Sys.setFileTime(paths[[1]], mtime + 0.8)

  result <- ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)
  summary <- attr(result, "ingest_summary")
  expect_equal(summary$status, "ok")
  expect_equal(
    DBI::dbGetQuery(store@con, "SELECT text FROM documents")$text,
    "# Modified\n\nNew content."
  )
})

test_that("page_metadata is written after successful ingest", {
  skip_on_cran()
  paths <- local_md_paths(1)
  store <- local_store()

  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  meta <- DBI::dbGetQuery(store@con, "SELECT * FROM page_metadata")
  expect_equal(nrow(meta), 1L)
  expect_equal(meta$origin, paths[[1]])
})

test_that("ingest_summary includes 'skipped_unchanged' as a possible status", {
  skip_on_cran()
  paths <- local_md_paths(2)
  store <- local_store()

  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)
  result <- ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)
  summary <- attr(result, "ingest_summary")
  expect_true(all(summary$status == "skipped_unchanged"))
})

test_that("shortened sources are stored before being marked unchanged", {
  skip_on_cran()

  path <- withr::local_tempfile(fileext = ".md")
  writeLines("ABCDE", path)
  mtime <- as.POSIXct("2026-10-16 12:00:00", tz = "UTC")
  Sys.setFileTime(path, mtime)
  store <- ragnar_store_create(embed = NULL)
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
  prepare <- function(path) {
    ragnar::MarkdownDocument(paste(readLines(path), collapse = "\n"), origin = path) |>
      ragnar::markdown_chunk(target_size = 1L, target_overlap = 0, max_snap_dist = 0)
  }
  ragnar_store_ingest(
    store, path, prepare = prepare,
    n_workers = 1L, progress = FALSE, build_index = FALSE
  )

  writeLines("AB", path)
  Sys.setFileTime(path, mtime + 1)
  result <- ragnar_store_ingest(
    store, path, prepare = prepare,
    n_workers = 1L, progress = FALSE, build_index = FALSE
  )
  expect_identical(
    attr(result, "ingest_summary"),
    tibble(path = path, status = "ok", n_chunks = 2L, message = NA_character_)
  )
  expect_equal(
    DBI::dbGetQuery(store@con, "SELECT text FROM documents")$text,
    "AB"
  )
  expect_equal(
    DBI::dbGetQuery(store@con, "SELECT text FROM chunks ORDER BY start")$text,
    c("A", "B")
  )

  result <- ragnar_store_ingest(
    store, path,
    prepare = function(path) stop("Unchanged sources should skip preparation."),
    n_workers = 1L, progress = FALSE, build_index = FALSE
  )
  expect_identical(
    attr(result, "ingest_summary"),
    tibble(
      path = path, status = "skipped_unchanged", n_chunks = 2L,
      message = NA_character_
    )
  )
})
