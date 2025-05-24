test_that("ragnar_store_update/insert", {
  store <- ragnar_store_create(
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100))
  )
  maybe_set_threads(store)
  expect_true(grepl("^store_[0-9]+$", store@name))

  chunks <- data.frame(
    origin = "foo",
    hash = "foo",
    text = "foo"
  )
  ragnar_store_update(store, chunks)

  val <- dbGetQuery(store@.con, "select origin, hash, text from chunks")
  expect_equal(val, chunks)

  # now try to update the store again - without changing the hash
  chunks2 <- data.frame(
    origin = "foo",
    hash = "foo",
    text = "bar"
  )
  ragnar_store_update(store, chunks2)

  # Expect that the text is not updated, because the hash is the same
  val <- dbGetQuery(store@.con, "select origin, hash, text from chunks")
  expect_equal(val, chunks)

  # now try to update the store again - changing the hash
  chunks2 <- data.frame(
    origin = "foo",
    hash = "bar",
    text = "bar"
  )
  ragnar_store_update(store, chunks2)

  # Expect that the text is updated
  val <- dbGetQuery(store@.con, "select origin, hash, text from chunks")
  expect_equal(val, chunks2)

  # Finally, try adding a new origin - even with the same hash
  chunks2 <- data.frame(
    origin = "bar",
    hash = "bar",
    text = "bar"
  )
  ragnar_store_update(store, chunks2)

  # Expect the origin is added
  val <- dbGetQuery(store@.con, "select origin, hash, text from chunks")
  expect_equal(nrow(val), 2)

  # Try adding with insert
  chunks2 <- data.frame(
    origin = "bar",
    hash = "bar",
    text = "bar"
  )
  ragnar_store_insert(store, chunks2)

  # Since we used insert, there's no checking if the hash is the same
  val <- dbGetQuery(store@.con, "select origin, hash, text from chunks")
  expect_equal(nrow(val), 3)
})

test_that("behavior when no hash/origin are provided", {
  store <- ragnar_store_create(
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100))
  )
  maybe_set_threads(store)

  chunks <- data.frame(
    text = "foo"
  )
  # users must explicitly set origin and hash when updating!
  expect_error(
    ragnar_store_update(store, chunks),
    "chunks` must have `origin` and `hash`"
  )
  # they can insert though
  ragnar_store_insert(store, chunks)

  val <- dbGetQuery(store@.con, "select origin, hash, text from chunks")
  expect_equal(
    val,
    data.frame(origin = NA_character_, hash = rlang::hash("foo"), text = "foo")
  )

  # if they insert again, even though the text has the same hash, we don't update anything
  ragnar_store_insert(store, chunks)

  # Expect that the text is not updated, because the hash is the same
  val <- dbGetQuery(store@.con, "select origin, hash, text from chunks")
  expect_equal(
    val,
    rbind(
      data.frame(
        origin = NA_character_,
        hash = rlang::hash("foo"),
        text = "foo"
      ),
      data.frame(
        origin = NA_character_,
        hash = rlang::hash("foo"),
        text = "foo"
      )
    )
  )
})

test_that("additional columns", {
  store <- ragnar_store_create(
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100)),
    extra_cols = data.frame(h1 = character(0))
  )
  maybe_set_threads(store)

  chunks <- data.frame(
    text = "foo"
  )
  # You can't insert chunks that miss the column
  expect_error(ragnar_store_insert(store, chunks), regexp = "Missing columns:")

  # Can't insert if they don't match types
  chunks <- data.frame(
    text = "foo",
    h1 = 1
  )
  expect_error(ragnar_store_insert(store, chunks), regexp = "Can't convert")

  # We should include if there's the correct data
  chunks <- data.frame(
    text = "foo",
    h1 = "hello"
  )
  ragnar_store_insert(store, chunks)
  val <- dbGetQuery(store@.con, "select text, h1 from chunks")
  expect_equal(val, chunks)

  # It's fine to insert a chunk if it has an additional column. It's
  # simply ignored.
  chunks <- data.frame(
    text = "foo",
    h1 = "hello",
    h2 = "bye"
  )
  ragnar_store_insert(store, chunks)
  val <- dbGetQuery(store@.con, "select text, h1 from chunks")
  expect_equal(nrow(val), 2)
})

test_that("Allow a NULL embedding function", {
  store <- ragnar_store_create(embed = NULL)
  maybe_set_threads(store)
  chunks <- data.frame(
    origin = c("foo", "bar"),
    hash = c("foo", "bar"),
    text = c("foo", "bar")
  )
  ragnar_store_update(store, chunks)
  # no error, vss is ignored
  expect_error(ragnar_store_build_index(store), regexp = NA)

  ragnar_store_build_index(store, type = "fts")
  ragnar_retrieve_bm25(store, "bar")
  expect_error(ragnar_retrieve(store, "bar"))
})
