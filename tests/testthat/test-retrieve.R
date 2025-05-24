test_that("retrieving works as expected", {
  # Create a simple store and insert some chunks
  store <- ragnar_store_create(
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
})
