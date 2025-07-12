system.time(test_that("retrieving works as expected, v1", {
  # The BUILD INDEX command for the VSS and BM25 extensions are multithreaded,
  # and still consume more CPU time than CRAN permits, even if we 'SET THREADS TO 1'.
  # CRAN complains:
  # > Running R code in 'testthat.R' had CPU time 2.6 times elapsed time
  # Unfortunately, this means we can't test properly on CRAN.
  skip_on_cran()

  # Create a simple store and insert some chunks
  store <- ragnar_store_create(
    version = 1,
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100))
  )
  maybe_set_threads(store)
  chunks <- data.frame(
    text = c("foo", "bar", "faz")
  )
  ragnar_store_insert(store, chunks)
  ragnar_store_build_index(store)

  # Can retrieve with vss
  ret <- ragnar_retrieve_vss(store, "hello")
  expect_in("metric_value", names(ret))
  expect_in("metric_name", names(ret))
  expect_equal(nrow(ret), 3)
  # Expect that all columns from the schema (except for embedding)
  # to be in the result set
  expect_true(all(
    setdiff(names(store@schema), "embedding") %in% names(ret)
  ))

  # test edge case where top_k is larger than the number of entries
  # in the store
  ret <- ragnar_retrieve_vss(store, "hello", top_k = 10)
  expect_equal(nrow(ret), 3)

  # Can retrieve with bm25
  ret <- ragnar_retrieve_bm25(store, "foo")
  expect_in("metric_value", names(ret))
  expect_in("metric_name", names(ret))
  expect_equal(nrow(ret), 1)
  # Expect that all columns from the schema (except for embedding)
  # to be in the result set
  expect_true(all(
    setdiff(names(store@schema), "embedding") %in% names(ret)
  ))

  # Can retrieve using the combined method
  ret <- ragnar_retrieve_vss_and_bm25(store, "foo")
  expect_equal(nrow(ret), 3)

  # Can retrieve using ragnar_retrieve
  ret <- ragnar_retrieve(store, "foo")
  expect_equal(nrow(ret), 3)
}))


test_that("retrieving works as expected", {
  skip_on_cran() # See comment (above) in test-retrieve.R
  # Create a simple store and insert some chunks
  store <- ragnar_store_create(
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100))
  )
  maybe_set_threads(store)

  # path <- system.file("README.md", package = "ragnar")
  # doc <- MarkdownDocument(readLines(path), path)
  doc <- MarkdownDocument(c("foo", "bar", "faz"), 'someorigin')
  chunks <- doc |> markdown_chunk(target_size = 4, target_overlap = 0)
  ragnar_store_insert(store, chunks)
  ragnar_store_build_index(store)

  # Can retrieve with vss
  ret <- ragnar_retrieve_vss(store, "hello")
  expect_in(
    c(
      "origin",
      "chunk_id",
      "start",
      "end",
      "text",
      "metric_value",
      "metric_value"
    ),
    names(ret)
  )
  expect_equal(nrow(ret), 3)
  # Expect that all columns from the schema (except for embedding)
  # to be in the result set
  expect_true(all(
    setdiff(names(store@schema), "embedding") %in% names(ret)
  ))

  # test edge case where top_k is larger than the number of entries
  # in the store
  ret <- ragnar_retrieve_vss(store, "hello", top_k = 100)
  expect_equal(nrow(ret), 3)
  expect_equal(order(ret$metric_value, decreasing = FALSE), seq_len(nrow(ret)))

  # Can retrieve with bm25
  ret <- ragnar_retrieve_bm25(store, "foo")
  expect_in("metric_value", names(ret))
  expect_in("metric_name", names(ret))
  expect_equal(nrow(ret), 1)
  # Expect that all columns from the schema (except for embedding)
  # to be in the result set
  expect_true(all(
    setdiff(names(store@schema), "embedding") %in% names(ret)
  ))
  expect_equal(order(ret$metric_value, decreasing = TRUE), seq_len(nrow(ret)))

  # Can retrieve using the combined method
  ret <- ragnar_retrieve_vss_and_bm25(store, "foo")
  expect_equal(nrow(ret), 3)

  # Can retrieve using ragnar_retrieve
  ret <- ragnar_retrieve(store, "foo")
  expect_equal(nrow(ret), 3)
})
