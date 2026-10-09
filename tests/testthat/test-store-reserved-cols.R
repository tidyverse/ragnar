test_that("store creation rejects reserved metadata names before overwriting", {
  skip_if_cant_load_duckdb_extensions()

  reserved <- c(
    "rowid", "ROWID", "metric_name", "METRIC_NAME", "metric_value",
    "METRIC_VALUE", "_ragnar_rowid", "_RAGNAR_future"
  )
  for (version in 1:2) {
    for (column in reserved) {
      extra_cols <- setNames(data.frame(integer()), column)
      store <- NULL
      expect_error(
        store <- ragnar_store_create(
          embed = NULL, extra_cols = extra_cols, version = version
        ),
        "reserved"
      )
      if (!is.null(store)) DBI::dbDisconnect(store@con, shutdown = TRUE)
    }

    location <- tempfile(fileext = ".duckdb")
    store <- ragnar_store_create(location, embed = NULL, version = version)
    DBI::dbDisconnect(store@con, shutdown = TRUE)
    replacement <- NULL
    expect_error(
      replacement <- ragnar_store_create(
        location, embed = NULL, overwrite = TRUE,
        extra_cols = data.frame(rowid = integer()), version = version
      ),
      "reserved"
    )
    if (!is.null(replacement)) DBI::dbDisconnect(replacement@con, shutdown = TRUE)
    store <- ragnar_store_connect(location)
    expect_false("rowid" %in% DBI::dbListFields(
      store@con, if (version == 1L) "chunks" else "embeddings"
    ))
    DBI::dbDisconnect(store@con, shutdown = TRUE)
    unlink(location)
  }
})

test_that("connecting rejects reserved names in existing store schemas", {
  skip_if_cant_load_duckdb_extensions()

  for (version in 1:2) {
    for (column in c("ROWID", "metric_name", "metric_value", "_RAGNAR_future")) {
      location <- tempfile(fileext = ".duckdb")
      store <- ragnar_store_create(location, embed = NULL, version = version)
      table <- if (version == 1L) "chunks" else "embeddings"
      DBI::dbExecute(store@con, paste(
        "ALTER TABLE", table, "ADD COLUMN", column, "INTEGER"
      ))
      DBI::dbDisconnect(store@con, shutdown = TRUE)

      connected <- NULL
      expect_error(connected <- ragnar_store_connect(location), "reserved")
      if (!is.null(connected)) DBI::dbDisconnect(connected@con, shutdown = TRUE)

      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = location)
      DBI::dbExecute(con, paste(
        "ALTER TABLE", table, "RENAME COLUMN", column, "TO user_field"
      ))
      DBI::dbDisconnect(con, shutdown = TRUE)
      store <- ragnar_store_connect(location)
      expect_true("user_field" %in% DBI::dbListFields(store@con, table))
      DBI::dbDisconnect(store@con, shutdown = TRUE)
      unlink(location)
    }
  }
})

test_that("de-overlapping preserves metadata with former internal names", {
  skip_on_cran()
  skip_if_cant_load_duckdb_extensions()

  store <- ragnar_store_create(
    embed = NULL,
    extra_cols = data.frame(
      overlap_grp = character(), deoverlapped_id = integer(),
      `_user_field` = character(), metric_name_1 = character(),
      check.names = FALSE
    )
  )
  on.exit(DBI::dbDisconnect(store@con, shutdown = TRUE))
  chunks <- MarkdownDocument("foo bar", "someorigin") |> markdown_chunk()
  chunks$overlap_grp <- "user group"
  chunks$deoverlapped_id <- 42L
  chunks[["_user_field"]] <- "user value"
  chunks$metric_name_1 <- "user metric"
  ragnar_store_insert(store, chunks)
  ragnar_store_build_index(store)

  retrieved <- ragnar_retrieve_bm25(store, "foo")
  expect_true(all(c("metric_name", "metric_value") %in% names(retrieved)))
  deoverlapped <- chunks_deoverlap(store, retrieved)
  expect_identical(deoverlapped$overlap_grp, list("user group"))
  expect_identical(deoverlapped$deoverlapped_id, list(42L))
  expect_identical(deoverlapped[["_user_field"]], list("user value"))
  expect_identical(deoverlapped$metric_name_1, list("user metric"))
  expect_false(any(startsWith(names(deoverlapped), "_ragnar_")))
})
