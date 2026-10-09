test_that("store connections disable insertion-order preservation", {
  skip_if_cant_load_duckdb_extensions()

  for (version in 1:2) {
    path <- withr::local_tempfile(fileext = ".duckdb")
    store <- ragnar_store_create(path, embed = NULL, version = version)
    expect_false(DBI::dbGetQuery(
      store@con,
      "SELECT current_setting('preserve_insertion_order') AS value"
    )$value)
    DBI::dbDisconnect(store@con, shutdown = TRUE)

    store <- ragnar_store_connect(path)
    expect_false(DBI::dbGetQuery(
      store@con,
      "SELECT current_setting('preserve_insertion_order') AS value"
    )$value)
    DBI::dbDisconnect(store@con, shutdown = TRUE)
  }
})

test_that("filtered VSS retrieval fits a bounded memory budget", {
  skip_on_cran()
  skip_if_cant_load_duckdb_extensions()

  path <- withr::local_tempfile(fileext = ".duckdb")
  store <- ragnar_store_create(
    path,
    embed = \(x) matrix(1, nrow = length(x), ncol = 256)
  )
  chunks <- MarkdownDocument(
    paste(rep("abcdefghij", 6000), collapse = "\n"),
    "memory-fixture"
  ) |> markdown_chunk(target_size = 11, target_overlap = 0)
  ragnar_store_insert(store, chunks)
  DBI::dbDisconnect(store@con, shutdown = TRUE)

  # Reopen so the ingestion cache does not consume the retrieval budget.
  store <- ragnar_store_connect(path)
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
  DBI::dbExecute(store@con, "SET threads = 1; SET memory_limit = '64MB'")

  retrieved <- ragnar_retrieve_vss(
    store,
    "query",
    query_vector = c(1, rep(0, 255)),
    filter = context == ""
  )
  expect_equal(nrow(retrieved), 3L)
  expect_equal(retrieved$text, chunks$text[rep(1, 3)])
  expect_equal(retrieved$embedding, matrix(1, nrow = 3, ncol = 256))
})

test_that("VSS ranking preserves fields, methods, and filters", {
  skip_on_cran()
  skip_if_cant_load_duckdb_extensions()

  for (version in 1:2) local({
    store <- ragnar_store_create(
      version = version,
      embed = \(x) cbind(seq_along(x), 1),
      extra_cols = data.frame(category = character())
    )
    withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
    chunks <- MarkdownDocument(paste(letters[1:6], collapse = "\n"), "origin") |>
      markdown_chunk(target_size = 2, target_overlap = 0)
    chunks$category <- rep(c("odd", "even"), 3)
    if (version == 1L) {
      chunks <- as.data.frame(chunks)
      chunks$origin <- "origin"
    }
    ragnar_store_insert(store, chunks)

    for (indexed in c(FALSE, TRUE)) {
      if (indexed) ragnar_store_build_index(store, type = "vss")
      for (method in c("cosine_distance", "euclidean_distance", "negative_inner_product")) {
        all <- ragnar_retrieve_vss(
          store, "query", top_k = 6, method = method, query_vector = c(1, 0)
        )
        expect_equal(all$metric_name, rep(method, 6))
        expect_equal(order(all$metric_value), seq_len(6))
        expect_equal(all$text, chunks$text[all$embedding[, 1]])
        expect_equal(all$origin, rep("origin", 6))

        filtered <- ragnar_retrieve_vss(
          store, "query", top_k = 2, method = method, query_vector = c(1, 0),
          filter = category == "even" & origin == "origin" & text != "b\n"
        )
        expected <- all[all$category == "even" & all$text != "b\n", ]
        expect_equal(filtered, head(expected, 2))

        filtered <- ragnar_retrieve_vss(
          store, "query", top_k = 2, method = method, query_vector = c(1, 0),
          filter = dbplyr::sql("embedding[1] > 4")
        )
        expect_equal(filtered, head(all[all$embedding[, 1] > 4, ], 2))
      }
    }
  })
})

test_that("VSS filters search beyond the indexed candidate limit", {
  skip_on_cran()

  for (version in 1:2) local({
    store <- ragnar_store_create(
      version = version,
      embed = \(x) cbind(seq_along(x), 1),
      extra_cols = data.frame(position = integer())
    )
    withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
    chunks <- MarkdownDocument(paste(rep("x", 6000), collapse = "\n"), "origin") |>
      markdown_chunk(target_size = 2, target_overlap = 0)
    chunks$position <- seq_len(nrow(chunks))
    if (version == 1L) chunks <- as.data.frame(chunks)
    ragnar_store_insert(store, chunks)

    retrieved <- ragnar_retrieve_vss(
      store, "query", query_vector = c(1, 0), method = "negative_inner_product",
      filter = position <= 3
    )
    expect_equal(retrieved$position, 3:1)
    expect_equal(retrieved$metric_value, -3:-1)

    retrieved <- ragnar_retrieve_vss(
      store, "query", query_vector = c(1, 0), method = "negative_inner_product",
      filter = position <= 3 | position == 6000
    )
    expect_equal(retrieved$position, c(6000L, 3L, 2L))

    retrieved <- ragnar_retrieve_vss(
      store, "query", top_k = 5001, query_vector = c(1, 0),
      method = "negative_inner_product", filter = position > 0
    )
    expect_equal(retrieved$position, 6000:1000)
    expect_equal(retrieved$metric_value, -6000:-1000)
  })
})

test_that("indexed VSS fetches embeddings only for bounded matches", {
  skip_on_cran()
  skip_if_cant_load_duckdb_extensions()

  for (version in 1:2) for (shadow_rowid in c(FALSE, TRUE)) local({
    withr::local_seed(42)
    store <- ragnar_store_create(
      version = version,
      embed = \(x) matrix(stats::runif(length(x) * 8L), ncol = 8L),
      extra_cols = if (shadow_rowid) data.frame(rowid = integer())
    )
    withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
    chunks <- MarkdownDocument(paste(rep("x", 20000), collapse = "\n"), "origin") |>
      markdown_chunk(target_size = 2, target_overlap = 0)
    if (shadow_rowid) chunks$rowid <- rep(1L, nrow(chunks))
    if (version == 1L) chunks <- as.data.frame(chunks)
    ragnar_store_insert(store, chunks)
    ragnar_store_build_index(store, type = "vss")
    expected_names <- c(DBI::dbListFields(store@con, "chunks"), "metric_name", "metric_value")

    profile_path <- withr::local_tempfile(fileext = ".json")
    DBI::dbExecute(store@con, "SET threads = 1; SET memory_limit = '32MB'")
    DBI::dbExecute(store@con, "SET enable_profiling = 'json'")
    DBI::dbExecute(store@con, paste0(
      "SET profiling_output = ", DBI::dbQuoteString(store@con, profile_path)
    ))

    embedding_scan_rows <- function(node) {
      c(
        if ("embedding" %in% unlist(node$extra_info$Projections)) {
          node$operator_cardinality
        },
        unlist(lapply(node$children, embedding_scan_rows))
      )
    }
    for (filtered in c(FALSE, TRUE)) {
      args <- list(
        store = store, query = "query", query_vector = rep(0.5, 8L),
        method = "euclidean_distance"
      )
      if (filtered) args$filter <- TRUE
      retrieved <- do.call(ragnar_retrieve_vss, args)
      profile <- jsonlite::read_json(profile_path)
      rows <- embedding_scan_rows(profile)
      expect_equal(nrow(retrieved), 3L)
      expect_setequal(names(retrieved), expected_names)
      expect_gt(length(rows), 0L)
      # Filtered searches may fetch 5,000 candidates before the final limit.
      expect_true(
        all(rows <= if (filtered) 5000L else 3L),
        info = paste(rows, collapse = ", ")
      )
    }
    DBI::dbExecute(store@con, "PRAGMA disable_profiling")
  })
})
