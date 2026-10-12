test_that("VSS metrics have unique names alongside custom metadata", {
  skip_if_cant_load_duckdb_extensions()

  schemas <- list(
    list(
      fields = data.frame(metric_name = character()),
      metrics = c(name = "metric_name_1", value = "metric_value")
    ),
    list(
      fields = data.frame(metric_value = numeric()),
      metrics = c(name = "metric_name", value = "metric_value_1")
    ),
    list(
      fields = data.frame(metric_name = character(), metric_value = numeric()),
      metrics = c(name = "metric_name_1", value = "metric_value_1")
    ),
    list(
      fields = data.frame(
        METRIC_NAME = character(), METRIC_VALUE = numeric(),
        metric_name_1 = character(), metric_value_1 = numeric()
      ),
      metrics = c(name = "metric_name_2", value = "metric_value_2")
    )
  )
  for (version in 2:1) for (schema in schemas) local({
    store <- ragnar_store_create(
      version = version,
      embed = \(x) cbind(seq_along(x), 1),
      extra_cols = schema$fields
    )
    withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
    chunks <- MarkdownDocument(
      paste(c("foo", "bar", "baz", "qux", "zip", "zap"), collapse = "\n"), "origin"
    ) |> markdown_chunk(target_size = 4, target_overlap = 0)
    for (column in names(schema$fields)) {
      chunks[[column]] <- if (is.character(schema$fields[[column]])) {
        paste0("metadata-", letters[1:6])
      } else {
        as.numeric(6:1)
      }
    }
    if (version == 1L) chunks <- as.data.frame(chunks)
    ragnar_store_insert(store, chunks)
    ragnar_store_build_index(store, type = "fts")
    expected_names <- c(DBI::dbListFields(store@con, "chunks"), schema$metrics)
    score <- schema$metrics[["value"]]
    metadata_column <- names(schema$fields)[1]
    metadata_value <- chunks[[metadata_column]][2]

    for (indexed in c(FALSE, TRUE)) {
      if (indexed) ragnar_store_build_index(store, type = "vss")
      retrieved <- ragnar_retrieve_vss(
        store, "query", method = "euclidean_distance", query_vector = c(1, 1)
      )
      expect_setequal(names(retrieved), expected_names)
      expect_equal(retrieved$text, chunks$text[1:3])
      for (column in names(schema$fields)) {
        expect_equal(retrieved[[column]], chunks[[column]][1:3])
      }
      expect_equal(retrieved[[schema$metrics[["name"]]]], rep("euclidean_distance", 3))
      expect_equal(retrieved[[score]], c(0, 1, 2))

      filtered <- ragnar_retrieve_vss(
        store, "query", top_k = 2, method = "euclidean_distance",
        query_vector = c(1, 1), filter = .data[[score]] >= 1
      )
      expect_equal(filtered, retrieved[2:3, ])
      filtered <- ragnar_retrieve_vss(
        store, "query", method = "euclidean_distance", query_vector = c(1, 1),
        filter = .data[[score]] == 1
      )
      expect_equal(filtered, retrieved[2, ])

      bm25 <- ragnar_retrieve_bm25(store, "bar")
      expect_equal(bm25[[schema$metrics[["name"]]]], "bm25")
      expect_true(is.numeric(bm25[[score]]))
      for (column in names(schema$fields)) {
        expect_equal(bm25[[column]], chunks[[column]][2])
      }

      combined <- ragnar_retrieve(
        store, "bar", method = "euclidean_distance", query_vector = c(1, 1),
        deoverlap = FALSE
      )
      expect_equal(nrow(combined), 3L)
      expect_equal(combined$text, chunks$text[1:3])
      expect_equal(combined$euclidean_distance, c(0, 1, 2))
      expect_equal(combined$bm25, c(NA_real_, bm25[[score]], NA_real_))
      expect_false(any(schema$metrics %in% names(combined)))
      for (column in names(schema$fields)) {
        expect_equal(combined[[column]], chunks[[column]][1:3])
      }

      deoverlapped <- ragnar_retrieve(
        store, "bar", method = "euclidean_distance", query_vector = c(1, 1),
        deoverlap = version == 2L
      )
      expect_equal(nrow(deoverlapped), 3L)
      for (column in c(names(schema$fields), "euclidean_distance", "bm25")) {
        expected <- if (version == 2L) as.list(combined[[column]]) else combined[[column]]
        expect_equal(deoverlapped[[column]], expected)
      }

      filtered <- ragnar_retrieve(
        store, "bar", method = "euclidean_distance", query_vector = c(1, 1),
        deoverlap = FALSE, filter = .data[[metadata_column]] == !!metadata_value
      )
      expect_equal(filtered, combined[2, ])

      empty <- ragnar_retrieve(
        store, "bar", method = "euclidean_distance", query_vector = c(1, 1),
        deoverlap = FALSE, filter = FALSE
      )
      expect_identical(
        empty, combined[0, setdiff(names(combined), c("euclidean_distance", "bm25"))]
      )
      filtered <- ragnar_retrieve_vss(
        store, "query", method = "euclidean_distance", query_vector = c(1, 1),
        filter = .data[[metadata_column]] == !!metadata_value
      )
      expect_equal(filtered, retrieved[2, ])
    }
  })
})

test_that("BM25-only combined retrieval preserves metric metadata", {
  skip_if_cant_load_duckdb_extensions()

  for (version in 1:2) local({
    store <- ragnar_store_create(
      version = version, embed = NULL,
      extra_cols = data.frame(metric_name = character(), metric_value = numeric())
    )
    withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
    chunks <- MarkdownDocument("foo", "origin") |> markdown_chunk()
    chunks$metric_name <- "user method"
    chunks$metric_value <- 42
    if (version == 1L) chunks <- as.data.frame(chunks)
    ragnar_store_insert(store, chunks)
    ragnar_store_build_index(store, type = "fts")

    retrieved <- ragnar_retrieve(store, "foo", deoverlap = FALSE)
    expect_equal(nrow(retrieved), 1L)
    expect_equal(retrieved$metric_name, "user method")
    expect_equal(retrieved$metric_value, 42)
    expect_true(is.numeric(retrieved$bm25))
    expect_gt(retrieved$bm25, 0)
  })
})
