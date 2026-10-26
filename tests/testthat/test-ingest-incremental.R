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

test_that("unchanged ingestion uses a fixed number of database queries", {
  skip_on_cran()
  paths <- local_md_paths(32)
  withr::defer(unlink(paths))
  store <- ragnar_store_create(embed = NULL)
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
  ragnar_store_ingest(
    store, paths,
    prepare = function(path) {
      ragnar::MarkdownDocument(paste(readLines(path), collapse = "\n"), origin = path) |>
        ragnar::markdown_chunk()
    },
    n_workers = 1L, progress = FALSE, build_index = FALSE
  )

  queries <- 0L
  db_get_query <- DBI::dbGetQuery
  local_mocked_bindings(
    dbGetQuery = function(...) {
      queries <<- queries + 1L
      db_get_query(...)
    },
    .package = "DBI"
  )
  for (n in c(1L, length(paths))) {
    queries <- 0L
    result <- ragnar_store_ingest(
      store, paths[seq_len(n)],
      prepare = function(path) stop("Unchanged files should skip preparation."),
      progress = FALSE, build_index = FALSE
    )
    expect_equal(attr(result, "ingest_summary")$status, rep("skipped_unchanged", n))
    if (n == 1L) single_queries <- queries
  }
  expect_equal(queries, single_queries)
})

test_that("local file freshness preserves subsecond modification times", {
  skip_on_cran()
  paths <- local_md_paths(2)
  store <- local_store()
  mtime <- as.POSIXct("2026-10-16 12:00:00", tz = "UTC")
  Sys.setFileTime(paths, mtime + c(0.1234565, 0.1234575))

  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  result <- ragnar_store_ingest(
    store, paths,
    prepare = function(path) stop("Unchanged timestamps should skip preparation."),
    progress = FALSE, build_index = FALSE
  )
  expect_equal(attr(result, "ingest_summary")$status, rep("skipped_unchanged", 2))

  writeLines(c("# Modified", "", "New content."), paths[[1]])
  Sys.setFileTime(paths[[1]], mtime + 0.8)

  result <- ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)
  summary <- attr(result, "ingest_summary")
  expect_equal(summary$status, c("ok", "skipped_unchanged"))
  expect_equal(
    DBI::dbGetQuery(
      store@con, "SELECT text FROM documents WHERE origin = ?",
      params = list(paths[[1]])
    )$text,
    "# Modified\n\nNew content."
  )
})

test_that("document freshness fields are written after successful ingest", {
  skip_on_cran()
  paths <- local_md_paths(1)
  store <- local_store()
  mtime <- as.POSIXct("2026-10-16 12:00:00", tz = "UTC")
  Sys.setFileTime(paths[[1]], mtime)

  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  meta <- DBI::dbGetQuery(store@con, "SELECT * FROM documents")
  expect_equal(nrow(meta), 1L)
  expect_equal(meta$origin, paths[[1]])
  expect_equal(meta$last_modified, mtime)
  expect_equal(meta$content_hash, rlang::hash(meta$text))
  expect_false(is.na(meta$ingested_at))
})

test_that("direct content updates clear stale document freshness", {
  skip_on_cran()
  path <- withr::local_tempfile(fileext = ".md")
  writeLines("Original source.", path)
  store <- ragnar_store_create(embed = NULL)
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
  ragnar_store_ingest(
    store, path, n_workers = 1L, progress = FALSE, build_index = FALSE
  )

  ragnar_store_update(
    store, MarkdownDocument("Manual update.", origin = path) |> markdown_chunk()
  )
  meta <- DBI::dbGetQuery(
    store@con, "SELECT last_modified, content_hash, ingested_at FROM documents"
  )
  expect_true(all(is.na(meta)))

  result <- ragnar_store_ingest(
    store, path, n_workers = 1L, progress = FALSE, build_index = FALSE
  )
  expect_equal(attr(result, "ingest_summary")$status, "ok")
  expect_equal(
    DBI::dbGetQuery(store@con, "SELECT text FROM documents")$text,
    "Original source."
  )
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
