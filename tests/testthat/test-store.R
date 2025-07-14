test_that("ragnar_store_update/insert, v1", {
  store <- ragnar_store_create(
    version = 1,
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

  val <- dbGetQuery(store@con, "select origin, hash, text from chunks")
  expect_equal(val, chunks)

  # now try to update the store again - without changing the hash
  chunks2 <- data.frame(
    origin = "foo",
    hash = "foo",
    text = "bar"
  )
  ragnar_store_update(store, chunks2)

  # Expect that the text is not updated, because the hash is the same
  val <- dbGetQuery(store@con, "select origin, hash, text from chunks")
  expect_equal(val, chunks)

  # now try to update the store again - changing the hash
  chunks2 <- data.frame(
    origin = "foo",
    hash = "bar",
    text = "bar"
  )
  ragnar_store_update(store, chunks2)

  # Expect that the text is updated
  val <- dbGetQuery(store@con, "select origin, hash, text from chunks")
  expect_equal(val, chunks2)

  # Finally, try adding a new origin - even with the same hash
  chunks2 <- data.frame(
    origin = "bar",
    hash = "bar",
    text = "bar"
  )
  ragnar_store_update(store, chunks2)

  # Expect the origin is added
  val <- dbGetQuery(store@con, "select origin, hash, text from chunks")
  expect_equal(nrow(val), 2)

  # Try adding with insert
  chunks2 <- data.frame(
    origin = "bar",
    hash = "bar",
    text = "bar"
  )
  ragnar_store_insert(store, chunks2)

  # Since we used insert, there's no checking if the hash is the same
  val <- dbGetQuery(store@con, "select origin, hash, text from chunks")
  expect_equal(nrow(val), 3)
})

test_that("ragnar_store_update/insert", {
  store <- ragnar_store_create(
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100))
  )
  maybe_set_threads(store)
  expect_true(grepl("^store_[0-9]+$", store@name))

  doc <- MarkdownDocument(c("foo", "bar", "faz"), 'someorigin')
  chunks <- doc |> markdown_chunk(target_size = 4, target_overlap = 0)
  ragnar_store_update(store, chunks)

  val <- dbGetQuery(
    store@con,
    'select start, "end", "context", text, from chunks'
  )
  expect_equal(val, as_bare_df(chunks))

  # now try to update the store with the same content
  chunks2 <- doc |> markdown_chunk(target_size = 4, target_overlap = 0)
  ragnar_store_update(store, chunks2)

  # Expect that the text is not updated
  val <- dbGetQuery(store@con, "select origin, text from chunks order by start")
  expect_equal(val$text, chunks$text)

  # now try to update the store again - with different content
  doc <- MarkdownDocument(c("abc", "def", "ghi"), 'someorigin')
  chunks <- doc |> markdown_chunk(target_size = 4, target_overlap = 0)
  ragnar_store_update(store, chunks)

  # Expect that the text is updated
  val <- dbGetQuery(
    store@con,
    'select start, "end", context, text, origin, from chunks'
  )
  expect_equal(val, chunks |> as_bare_df() |> mutate(origin = 'someorigin'))
  expect_equal(nrow(val), 3)

  # Finally, try adding a new origin - even with the same text
  doc2 <- doc |> set_props(origin = "some other origin")
  chunks2 <- doc2 |> markdown_chunk(target_size = 4, target_overlap = 0)
  ragnar_store_update(store, chunks2)

  hoist_origin <- function(x) {
    x$origin <- x@document@origin
    x[unique(c("origin", names(x)))]
  }

  # Expect the origin is added
  val <- dbGetQuery(
    store@con,
    "select * exclude (chunk_id, doc_id, embedding) from chunks"
  )
  expect_equal(nrow(val), 6)
  val <- val |> arrange(origin, start) |> as_bare_df()
  expected <- vec_rbind(
    chunks |> hoist_origin() |> as_bare_df(),
    chunks2 |> hoist_origin() |> as_bare_df()
  ) |>
    arrange(origin, start)

  expect_equal(expected, val)
})

test_that("behavior when no hash/origin are provided", {
  store <- ragnar_store_create(
    version = 1,
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

  val <- dbGetQuery(store@con, "select origin, hash, text from chunks")
  expect_equal(
    val,
    data.frame(origin = NA_character_, hash = rlang::hash("foo"), text = "foo")
  )

  # if they insert again, even though the text has the same hash, we don't update anything
  ragnar_store_insert(store, chunks)

  # Expect that the text is not updated, because the hash is the same
  val <- dbGetQuery(store@con, "select origin, hash, text from chunks")
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
    version = 1,
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100)),
    extra_cols = data.frame(h1 = character(0), int = integer(0))
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
    h1 = 1L, # h1 should be character, not integer
    int = 1L
  )
  expect_error(ragnar_store_insert(store, chunks), regexp = "Can't convert")

  # We should include if there's the correct data
  chunks <- data.frame(
    text = "foo",
    h1 = "hello",
    int = 1 # numerics should be allowed and converted to integer
  )
  ragnar_store_insert(store, chunks)
  val <- dbGetQuery(store@con, "select text, h1 from chunks")
  expect_equal(val, chunks |> select(-int))

  # It's fine to insert a chunk if it has an additional column. It's
  # simply ignored.
  chunks <- data.frame(
    text = "foo",
    h1 = "hello",
    h2 = "bye",
    int = 1L
  )
  ragnar_store_insert(store, chunks)
  val <- dbGetQuery(store@con, "select text, h1 from chunks")
  expect_equal(nrow(val), 2)
})

test_that("Allow a NULL embedding function", {
  skip_on_cran() # See comment in test-retrieve.R
  store <- ragnar_store_create(embed = NULL, version = 1)
  maybe_set_threads(store)
  chunks <- data.frame(
    origin = c("foo", "bar"),
    hash = c("foo", "bar"),
    text = c("foo", "bar")
  )
  ragnar_store_update(store, chunks)
  # no error, vss is ignored
  expect_no_error(ragnar_store_build_index(store))

  ragnar_store_build_index(store, type = "fts")
  ragnar_retrieve_bm25(store, "bar")
  expect_no_error(ragnar_retrieve(store, "bar"))
})

test_that("works with MotherDuck", {
  skip_if_cant_use_motherduck()
  skip_on_cran() # See comment in test-retrieve.R

  store <- ragnar_store_create(
    version = 1,
    "md:ragnartest",
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100)),
    overwrite = TRUE
  )
  maybe_set_threads(store)

  chunks <- data.frame(
    origin = "foo",
    hash = "foo",
    text = "foo"
  )

  expect_error(ragnar_store_insert(store, chunks), regexp = NA)
  expect_warning(
    ragnar_store_build_index(store),
    regexp = "MotherDuck does not support"
  )
  expect_error(ragnar_retrieve(store, "hello"), regexp = NA)

  # Since we used insert, there's no checking if the hash is the same
  val <- dbGetQuery(store@con, "select origin, hash, text from chunks")
  expect_equal(nrow(val), 1)

  # connect to the motherduck store
  store <- ragnar_store_connect("md:ragnartest")
  maybe_set_threads(store)
  expect_error(ragnar_retrieve(store, "hello"), regexp = NA)

  val <- dbGetQuery(store@con, "select origin, hash, text from chunks")
  expect_equal(nrow(val), 1)
})

test_that("embed functions get the defaults stored", {
  store <- ragnar_store_create(embed = function(x) ragnar::embed_openai(x))
  expect_snapshot(store@embed)

  # here embed_openai is implicitly obtained from ragnar::embed_openai
  store <- ragnar_store_create(embed = function(x) embed_openai(x))
  expect_snapshot(store@embed)

  # if the embed function takes ..., they're preserved
  store <- ragnar_store_create(
    embed = function(x, ...) ragnar::embed_openai(x, ...)
  )
  expect_snapshot(store@embed)

  # when using the partialized version, we should also add the defaults
  store <- ragnar_store_create(embed = embed_openai())
  expect_snapshot(store@embed)

  # test other embed funcs
  skip_if(inherits(try(silent = TRUE, ragnar::embed_ollama("hi")), "try-error"))
  store <- ragnar_store_create(embed = function(x) ragnar::embed_ollama(x))
  expect_snapshot(store@embed)
})

test_that("store v1 accepts markdown chunks (from v2)", {
  skip_on_cran() # See comment in test-retrieve.R
  store <- ragnar_store_create(
    version = 1,
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100))
  )
  maybe_set_threads(store)

  chunks <- test_doc() |>
    read_as_markdown() |>
    markdown_chunk()

  # insert directly ignores al other columns
  expect_no_error(ragnar_store_insert(store, chunks))
  ragnar_store_build_index(store)
  expect_equal(ragnar_retrieve(store, "r")$origin[1], chunks@document@origin)

  # when the store has start, end and origin as extra cols they get included
  store <- ragnar_store_create(
    version = 1,
    extra_cols = data.frame(
      start = integer(0),
      end = integer(0),
      context = character(0)
    ),
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100))
  )
  maybe_set_threads(store)

  chunks <- test_doc() |>
    read_as_markdown() |>
    markdown_chunk()

  expect_no_error(ragnar_store_insert(store, chunks))
  ragnar_store_build_index(store)
  expect_equal(ragnar_retrieve(store, "r")$origin[1], chunks@document@origin)

  # when the chunks data.frame already contains an origin column, it's used instead.
  store <- ragnar_store_create(
    version = 1,
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100))
  )
  maybe_set_threads(store)

  chunks2 <- chunks |> mutate(origin = "a")
  expect_no_error(ragnar_store_insert(store, chunks2))
  ragnar_store_build_index(store)
  expect_equal(ragnar_retrieve(store, "r")$origin[1], "a")
})
