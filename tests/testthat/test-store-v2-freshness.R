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

test_that("connecting to an older v2 store asks users to recreate it", {
  skip_on_cran()
  location <- withr::local_tempfile(fileext = ".duckdb")
  store <- ragnar_store_create(location, embed = NULL)
  DBI::dbExecute(store@con, "ALTER TABLE documents DROP COLUMN last_modified")
  DBI::dbDisconnect(store@con, shutdown = TRUE)

  expect_error(
    ragnar_store_connect(location, read_only = FALSE),
    "Recreate it with `ragnar_store_create()`.",
    fixed = TRUE
  )
})
