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

test_that("ragnar_store_sync() removes documents not in keep", {
  skip_on_cran()
  paths <- local_md_paths(3)
  store <- local_store()
  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  keep <- paths[1:2]
  result <- ragnar_store_sync(store, keep = keep, rebuild_index = FALSE)

  remaining <- DBI::dbGetQuery(store@con, "SELECT origin FROM documents")$origin
  expect_setequal(remaining, keep)
  expect_false(paths[[3]] %in% remaining)
})

test_that("ragnar_store_sync() returns store invisibly with sync_summary attribute", {
  skip_on_cran()
  paths <- local_md_paths(2)
  store <- local_store()
  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  result <- ragnar_store_sync(store, keep = paths[1], rebuild_index = FALSE)

  # result wraps the same underlying DuckDB connection as store
  expect_identical(result@con, store@con)
  summary <- attr(result, "sync_summary")
  expect_s3_class(summary, "data.frame")
  expect_named(summary, c("origin", "action"))
  expect_true("kept" %in% summary$action)
  expect_true("deleted" %in% summary$action)
})

test_that("ragnar_store_sync() dry_run returns summary tibble without modifying store", {
  skip_on_cran()
  paths <- local_md_paths(2)
  store <- local_store()
  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  summary <- ragnar_store_sync(store, keep = paths[1], dry_run = TRUE, rebuild_index = FALSE)

  expect_s3_class(summary, "data.frame")
  expect_named(summary, c("origin", "action"))
  # Store unchanged
  remaining <- DBI::dbGetQuery(store@con, "SELECT origin FROM documents")$origin
  expect_setequal(remaining, paths)
})

test_that("ragnar_store_sync() preserves freshness only for kept documents", {
  skip_on_cran()
  paths <- local_md_paths(2)
  store <- local_store()
  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)
  metadata <- DBI::dbGetQuery(
    store@con,
    "SELECT origin, last_modified, content_hash, ingested_at
     FROM documents WHERE origin = ?",
    params = list(paths[[1]])
  )

  ragnar_store_sync(store, keep = paths[1], rebuild_index = FALSE)

  remaining <- DBI::dbGetQuery(
    store@con,
    "SELECT origin, last_modified, content_hash, ingested_at FROM documents"
  )
  expect_equal(remaining, metadata)
})

test_that("ragnar_store_sync() errors on v1 store", {
  skip_on_cran()
  store_v1 <- ragnar_store_create(
    embed = \(x) matrix(stats::runif(10), nrow = length(x), ncol = 10),
    version = 1
  )
  expect_error(
    ragnar_store_sync(store_v1, keep = character(0)),
    regexp = "version 2"
  )
})

test_that("ragnar_store_sync() keeping all origins deletes nothing", {
  skip_on_cran()
  paths <- local_md_paths(2)
  store <- local_store()
  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  result <- ragnar_store_sync(store, keep = paths, rebuild_index = FALSE)
  summary <- attr(result, "sync_summary")
  expect_true(all(summary$action == "kept"))
})
