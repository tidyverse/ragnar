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

test_that("modified local files are re-ingested after content changes", {
  skip_on_cran()
  paths <- local_md_paths(1)
  store <- local_store()

  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  # Modify the file; use Sys.sleep to ensure mtime changes
  Sys.sleep(1.1)
  writeLines(c("# Modified", "", "New content."), paths[[1]])

  result <- ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)
  summary <- attr(result, "ingest_summary")
  expect_equal(summary$status, "ok")
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
