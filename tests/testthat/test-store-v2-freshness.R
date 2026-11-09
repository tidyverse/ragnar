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

# Builds a store with the v2 schema used before freshness tracking (#200,
# #201): `documents` has no freshness columns, and `embeddings` has a foreign
# key to `documents`. The stored rows and search indexes are kept.
local_older_v2_store <- function(paths, env = parent.frame()) {
  location <- withr::local_tempfile(fileext = ".duckdb", .local_envir = env)
  store <- ragnar_store_create(
    location,
    embed = function(x) matrix(1, nrow = length(x), ncol = 4L)
  )
  for (path in paths) {
    ragnar_store_insert(store, read_as_markdown(path) |> markdown_chunk())
  }
  con <- store@con
  DBI::dbExecute(
    con,
    r"--(
    CREATE TEMP TABLE old_documents AS SELECT doc_id, origin, text FROM documents;
    CREATE TEMP TABLE old_embeddings AS SELECT * FROM embeddings;
    DROP VIEW chunks;
    DROP TABLE embeddings;
    DROP TABLE documents;

    CREATE TABLE documents (
      doc_id INTEGER PRIMARY KEY DEFAULT nextval('doc_id_seq'),
      origin VARCHAR UNIQUE,
      text VARCHAR
    );
    CREATE TABLE embeddings (
      doc_id INTEGER NOT NULL,
      FOREIGN KEY (doc_id) REFERENCES documents (doc_id),
      chunk_id INTEGER DEFAULT nextval('chunk_id_seq'),
      start INTEGER,
      "end" INTEGER,
      PRIMARY KEY (doc_id, start, "end"),
      context VARCHAR,
      embedding FLOAT[4]
    );
    CREATE VIEW chunks AS (
      SELECT d.origin AS origin, e.*, d.text[ e.start : e."end" ] AS text
      FROM documents d JOIN embeddings e USING (doc_id)
    );

    INSERT INTO documents SELECT * FROM old_documents;
    INSERT INTO embeddings SELECT * FROM old_embeddings;
    )--"
  )
  ragnar_store_build_index(store)
  DBI::dbDisconnect(con, shutdown = TRUE)
  location
}

hnsw_indexes <- function(store) {
  DBI::dbGetQuery(
    store@con,
    "SELECT index_name FROM duckdb_indexes()
     WHERE index_name LIKE 'store_hnsw_%' ORDER BY index_name"
  )$index_name
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
  expect_equal(ragnar_retrieve_vss(store, "stored")$origin, path)
})

test_that("writable connections upgrade older v2 stores", {
  skip_on_cran()
  kept <- withr::local_tempfile(fileext = ".md")
  removed <- withr::local_tempfile(fileext = ".md")
  writeLines("Kept document.", kept)
  writeLines("Removed document.", removed)
  location <- local_older_v2_store(c(kept, removed))

  store <- ragnar_store_connect(location, read_only = FALSE)
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
  expect_setequal(
    DBI::dbListFields(store@con, "documents"),
    c("doc_id", "origin", "text", "last_modified", "content_hash", "ingested_at")
  )
  expect_equal(
    hnsw_indexes(store),
    c("store_hnsw_cosine_index", "store_hnsw_ip_index", "store_hnsw_l2sq_index")
  )

  ingest <- function() {
    result <- ragnar_store_ingest(
      store, kept, n_workers = 1L, progress = FALSE, build_index = FALSE
    )
    attr(result, "ingest_summary")$status
  }
  expect_equal(ingest(), "ok")
  expect_equal(ingest(), "skipped_unchanged")

  ragnar_store_sync(store, keep = kept)
  expect_equal(
    DBI::dbGetQuery(store@con, "SELECT origin FROM documents")$origin,
    kept
  )
  expect_equal(ragnar_retrieve_bm25(store, "document")$origin, kept)
})
