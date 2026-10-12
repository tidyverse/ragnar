test_that("retrieval rejects metadata that conflicts with metric names", {
  skip_if_cant_load_duckdb_extensions()

  for (version in 1:2) for (column in c(
    "metric_name", "metric_value", "METRIC_NAME", "METRIC_VALUE"
  )) local({
    path <- withr::local_tempfile(fileext = ".duckdb")
    store <- ragnar_store_create(
      path, version = version,
      embed = \(x) matrix(1, nrow = length(x), ncol = 2),
      extra_cols = stats::setNames(data.frame(character()), column)
    )
    chunks <- MarkdownDocument("foo", "origin") |> markdown_chunk()
    chunks[[column]] <- "user metadata"
    if (version == 1L) chunks <- as.data.frame(chunks)
    ragnar_store_insert(store, chunks)
    ragnar_store_build_index(store)
    DBI::dbDisconnect(store@con, shutdown = TRUE)
    store <- ragnar_store_connect(path)
    withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))

    err <- expect_error(ragnar_retrieve_vss(store, "foo"), "conflict with retrieval metrics")
    expect_match(conditionMessage(err), column, fixed = TRUE)
    expect_match(conditionMessage(err), "Create a store with different names", fixed = TRUE)
    expect_error(ragnar_retrieve_bm25(store, "foo"), "conflict with retrieval metrics")
    expect_error(ragnar_retrieve(store, "foo"), "conflict with retrieval metrics")
  })
})

test_that("similar metadata names preserve the public metric columns", {
  skip_if_cant_load_duckdb_extensions()

  for (version in 1:2) local({
    store <- ragnar_store_create(
      version = version, embed = \(x) cbind(seq_along(x), 1),
      extra_cols = data.frame(metric_name_1 = character(), metric_value_1 = numeric())
    )
    withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
    chunks <- MarkdownDocument("foo\nbar\nbaz", "origin") |>
      markdown_chunk(target_size = 4, target_overlap = 0)
    chunks$metric_name_1 <- paste0("metadata-", 1:3)
    chunks$metric_value_1 <- 3:1
    if (version == 1L) chunks <- as.data.frame(chunks)
    ragnar_store_insert(store, chunks)
    ragnar_store_build_index(store)

    vss <- ragnar_retrieve_vss(
      store, "foo", method = "euclidean_distance", query_vector = c(1, 1)
    )
    expect_equal(vss$metric_name, rep("euclidean_distance", 3))
    expect_equal(vss$metric_value, c(0, 1, 2))
    expect_equal(vss$metric_name_1, chunks$metric_name_1)
    expect_equal(vss$metric_value_1, as.numeric(chunks$metric_value_1))

    bm25 <- ragnar_retrieve_bm25(store, "foo")
    expect_equal(bm25$metric_name, "bm25")
    expect_true(is.numeric(bm25$metric_value))
    expect_equal(bm25$metric_name_1, chunks$metric_name_1[1])
    expect_equal(bm25$metric_value_1, as.numeric(chunks$metric_value_1[1]))

    combined <- ragnar_retrieve(
      store, "foo", method = "euclidean_distance", query_vector = c(1, 1),
      deoverlap = FALSE
    )
    expect_equal(combined$euclidean_distance, c(0, 1, 2))
    expect_equal(combined$bm25, c(bm25$metric_value, NA_real_, NA_real_))
    expect_equal(combined$metric_name_1, chunks$metric_name_1)
    expect_equal(combined$metric_value_1, as.numeric(chunks$metric_value_1))
  })
})

test_that("BM25-only retrieval rejects conflicting metadata names", {
  skip_if_cant_load_duckdb_extensions()

  for (version in 1:2) local({
    store <- ragnar_store_create(
      version = version, embed = NULL,
      extra_cols = data.frame(metric_name = character())
    )
    withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
    expect_error(ragnar_retrieve(store, "foo"), "conflict with retrieval metrics")
  })
})
