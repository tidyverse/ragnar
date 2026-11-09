test_that("v2 stores keep freshness fields on documents", {
  skip_on_cran()
  store <- ragnar_store_create(embed = NULL)
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
  tables <- DBI::dbListTables(store@con)
  expect_false("page_metadata" %in% tables)
  expect_setequal(
    DBI::dbListFields(store@con, "documents"),
    c("doc_id", "origin", "text", "last_modified", "content_hash", "ingested_at")
  )
})

test_that("direct inserts and updates leave freshness fields empty", {
  skip_on_cran()
  store <- ragnar_store_create(embed = NULL)
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
  ragnar_store_insert(
    store, MarkdownDocument("Inserted.", origin = "inserted") |> markdown_chunk()
  )
  ragnar_store_update(
    store, MarkdownDocument("Updated.", origin = "updated") |> markdown_chunk()
  )
  ragnar_store_update(
    store, MarkdownDocument("Replaced.", origin = "updated") |> markdown_chunk()
  )
  documents <- DBI::dbGetQuery(
    store@con,
    "SELECT origin, text, last_modified, content_hash, ingested_at
     FROM documents ORDER BY origin"
  )
  expect_equal(documents$origin, c("inserted", "updated"))
  expect_equal(documents$text, c("Inserted.", "Replaced."))
  expect_true(all(is.na(documents[c("last_modified", "content_hash", "ingested_at")])))
})

test_that("ragnar_store_connect() preserves document freshness fields", {
  skip_on_cran()
  tmp <- withr::local_tempfile(fileext = ".duckdb")
  path <- withr::local_tempfile(fileext = ".md")
  writeLines("Stored document.", path)
  store <- ragnar_store_create(tmp, embed = NULL)
  ragnar_store_ingest(
    store, path, n_workers = 1L, progress = FALSE, build_index = FALSE
  )
  documents <- DBI::dbGetQuery(store@con, "SELECT * FROM documents")
  DBI::dbDisconnect(store@con, shutdown = TRUE)

  store2 <- ragnar_store_connect(tmp, read_only = FALSE)
  withr::defer(DBI::dbDisconnect(store2@con, shutdown = TRUE))
  expect_equal(DBI::dbGetQuery(store2@con, "SELECT * FROM documents"), documents)
  result <- ragnar_store_ingest(
    store2, path,
    prepare = function(path) stop("Unchanged timestamps should skip preparation."),
    n_workers = 1L, progress = FALSE, build_index = FALSE
  )
  expect_equal(attr(result, "ingest_summary")$status, "skipped_unchanged")
})

local_older_v2_store <- function(path, env = parent.frame()) {
  location <- withr::local_tempfile(fileext = ".duckdb", .local_envir = env)
  store <- ragnar_store_create(location, embed = NULL)
  ragnar_store_insert(store, read_as_markdown(path) |> markdown_chunk())
  ragnar_store_build_index(store)
  DBI::dbExecute(
    store@con,
    "ALTER TABLE documents DROP COLUMN last_modified;
     ALTER TABLE documents DROP COLUMN content_hash;
     ALTER TABLE documents DROP COLUMN ingested_at;"
  )
  DBI::dbDisconnect(store@con, shutdown = TRUE)
  location
}

test_that("older v2 stores remain readable without migration", {
  skip_on_cran()
  path <- withr::local_tempfile(fileext = ".md")
  writeLines("Stored document.", path)
  location <- local_older_v2_store(path)

  store <- ragnar_store_connect(location)
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
  expect_equal(
    DBI::dbListFields(store@con, "documents"),
    c("doc_id", "origin", "text")
  )
  expect_equal(ragnar_retrieve_bm25(store, "stored")$origin, path)
})

test_that("writable connections upgrade older v2 stores", {
  skip_on_cran()
  path <- withr::local_tempfile(fileext = ".md")
  writeLines("Stored document.", path)
  location <- local_older_v2_store(path)

  store <- ragnar_store_connect(location, read_only = FALSE)
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
  expect_setequal(
    DBI::dbListFields(store@con, "documents"),
    c("doc_id", "origin", "text", "last_modified", "content_hash", "ingested_at")
  )

  ingest <- function() {
    result <- ragnar_store_ingest(
      store, path, n_workers = 1L, progress = FALSE, build_index = FALSE
    )
    attr(result, "ingest_summary")$status
  }
  expect_equal(ingest(), "ok")
  expect_equal(ingest(), "skipped_unchanged")
})
