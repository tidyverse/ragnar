test_that("existing paths still honor build_index", {
  skip_on_cran()

  path <- withr::local_tempfile(fileext = ".md")
  writeLines("Document for indexing.", path)
  store <- ragnar_store_create(
    embed = function(x) matrix(1, nrow = length(x), ncol = 4L)
  )
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))

  ragnar_store_ingest(
    store, path, n_workers = 1L, progress = FALSE, build_index = FALSE
  )
  indexes <- function() {
    DBI::dbGetQuery(
      store@con,
      "SELECT index_name FROM duckdb_indexes() WHERE index_name LIKE 'store_hnsw_%'"
    )$index_name
  }
  expect_length(indexes(), 0L)

  ragnar_store_ingest(store, path, n_workers = 1L, progress = FALSE)

  expect_setequal(
    indexes(),
    c("store_hnsw_cosine_index", "store_hnsw_l2sq_index", "store_hnsw_ip_index")
  )
  expect_equal(ragnar_retrieve_bm25(store, "indexing")$origin, path)
})
