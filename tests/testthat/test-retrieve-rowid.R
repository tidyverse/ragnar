test_that("VSS selection handles custom columns that shadow physical row IDs", {
  skip_if_cant_load_duckdb_extensions()

  for (version in 1:2) for (column in c("rowid", "ROWID")) local({
    extra_cols <- stats::setNames(
      data.frame(integer(), character(), integer()),
      c(column, "category", "_RAGNAR_ROWID")
    )
    path <- withr::local_tempfile(fileext = ".duckdb")
    store <- ragnar_store_create(
      path,
      version = version,
      embed = \(x) cbind(seq_along(x), 1),
      extra_cols = extra_cols
    )
    chunks <- MarkdownDocument(paste(letters[1:6], collapse = "\n"), "origin") |>
      markdown_chunk(target_size = 2, target_overlap = 0)
    chunks[[column]] <- rep(1L, nrow(chunks))
    chunks$category <- rep(c("odd", "even"), 3)
    chunks$`_RAGNAR_ROWID` <- seq_len(nrow(chunks))
    if (version == 1L) chunks <- as.data.frame(chunks)
    ragnar_store_insert(store, chunks)

    # Reconnected stores must detect the schema from the database as well.
    DBI::dbDisconnect(store@con, shutdown = TRUE)
    store <- ragnar_store_connect(path, read_only = FALSE)
    withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))

    for (indexed in c(FALSE, TRUE)) {
      if (indexed) ragnar_store_build_index(store, type = "vss")
      retrieved <- ragnar_retrieve_vss(
        store, "query", top_k = 1, method = "euclidean_distance",
        query_vector = c(1, 1)
      )
      expect_equal(retrieved$text, chunks$text[1])
      expect_equal(retrieved[[column]], 1L)
      expect_equal(retrieved$`_RAGNAR_ROWID`, 1L)

      retrieved <- ragnar_retrieve_vss(
        store, "query", top_k = 1, method = "euclidean_distance",
        query_vector = c(1, 1), filter = category == "even"
      )
      expect_equal(retrieved$text, chunks$text[2])
      expect_equal(retrieved$category, "even")
      expect_equal(retrieved$`_RAGNAR_ROWID`, 2L)
    }
  })
})
