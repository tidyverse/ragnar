test_that("ragnar_store_update/insert, v2", {
  skip_on_cran() # See comment in test-retrieve.R
  store <- ragnar_store_create(
    version = 2,
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100))
  )
  maybe_set_threads(store)
  expect_true(grepl("^store_[0-9]+$", store@name))

  doc <- test_doc()
  chunks <- doc |> read_as_markdown() |> markdown_chunk()
  ragnar_store_update(store, chunks)

  expect_equal(
    tbl(store@con, "chunks") |> collect() |> nrow(),
    nrow(chunks)
  )

  # same chunks, different origin -----
  chunks2 <- chunks
  chunks2@document@origin <- "foo"
  ragnar_store_update(store, chunks2)

  expect_equal(
    tbl(store@con, "chunks") |> collect() |> nrow(),
    nrow(chunks) * 2
  )

  # same chunks again, no insert
  ragnar_store_update(store, chunks) # this doesn't insert again
  expect_equal(
    tbl(store@con, "chunks") |> collect() |> nrow(),
    nrow(chunks) * 2
  )

  # same origin, different chunks ------
  chunks3 <- doc |> read_as_markdown() |> markdown_chunk(target_size = 50)
  ragnar_store_update(store, chunks)

  expect_equal(chunks@document@origin, chunks3@document@origin)
  expect_equal(
    tbl(store@con, "chunks") |> collect() |> nrow(),
    nrow(chunks2) + nrow(chunks)
  )

  # fails to insert duplicated document
  expect_error(
    ragnar_store_insert(store, chunks3),
    regexp = 'Duplicate key "origin'
  )
})

test_that("insert chunks with pre-compuited embeddings", {
  skip_on_cran() # See comment in test-retrieve.R
  store <- ragnar_store_create(
    version = 2,
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100))
  )
  maybe_set_threads(store)

  chunks <- test_doc() |> read_as_markdown() |> markdown_chunk()
  embeddings <- matrix(nrow = nrow(chunks), ncol = 100, as.numeric(1:100))
  chunks <- chunks |>
    mutate(
      embedding = embeddings
    )

  ragnar_store_insert(store, chunks)
  chunks_ret <- tbl(store@con, "chunks") |>
    collect()

  # pre computed embeddings are used
  expect_equal(
    chunks_ret$embedding,
    embeddings
  )
})

test_that("update + extra cols", {
  skip_on_cran() # See comment in test-retrieve.R
  store <- ragnar_store_create(
    version = 2,
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100)),
    extra_cols = data.frame(
      number = numeric(0)
    )
  )
  maybe_set_threads(store)

  chunks <- test_doc() |> read_as_markdown() |> markdown_chunk()

  chunks <- chunks |> mutate(number = 1.23)
  ragnar_store_insert(store, chunks)

  chunks <- chunks |> mutate(number = 10)
  ragnar_store_update(store, chunks)

  # updated the number, even if it's the only thing that changed
  num <- tbl(store@con, "chunks") |> select(number) |> collect() |> unique()
  expect_equal(num$number, 10)

  # calling it again shouldn't change the db
  # since embed raises, adding chunks will raise if it incorrectly detects they are already equal
  store@embed <- function(x) {
    stop("something went wrong, tried inserting chunks")
  }
  expect_error(
    ragnar_store_update(store, chunks),
    regexp = NA
  )
})

test_that("works with MotherDuck", {
  skip_if_cant_use_motherduck()
  skip_on_cran() # See comment in test-retrieve.R

  store <- ragnar_store_create(
    "md:ragnartest",
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100)),
    overwrite = TRUE
  )
  maybe_set_threads(store)

  expect_true(is_motherduck_con(store@con))

  chunks <- test_doc() |>
    read_as_markdown() |>
    markdown_chunk()

  expect_error(ragnar_store_insert(store, chunks), regexp = NA)
  expect_warning(
    ragnar_store_build_index(store),
    regexp = "MotherDuck does not support"
  )
  expect_error(ragnar_retrieve(store, "hello"), regexp = NA)

  # Since we used insert, there's no checking if the hash is the same
  val <- tbl(store@con, "chunks") |> collect()
  expect_equal(nrow(val), nrow(chunks))

  # connect to the motherduck store
  store <- ragnar_store_connect("md:ragnartest")
  maybe_set_threads(store)
  expect_error(ragnar_retrieve(store, "hello"), regexp = NA)

  val <- tbl(store@con, "chunks") |> collect()
  expect_equal(nrow(val), nrow(chunks))

  expect_error(
    {
      store <- ragnar_store_create(
        "md:ragnartest",
        embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100)),
        overwrite = FALSE
      )
    },
    regexp = "Database already exists"
  )
})

test_that("ragnar_store_create overwrite is correct", {
  skip_on_cran() # See comment in test-retrieve.R
  temp <- tempfile(fileext = ".duckdb")

  store <- ragnar_store_create(
    location = temp,
    version = 2,
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100))
  )
  maybe_set_threads(store)

  chunks <- test_doc() |>
    read_as_markdown() |>
    markdown_chunk()

  ragnar_store_insert(store, chunks)
  DBI::dbDisconnect(store@con)

  expect_error(
    {
      ragnar_store_create(
        location = temp,
        version = 2,
        embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100)),
        overwrite = FALSE
      )
    },
    regexp = "File already exists"
  )

  expect_error(
    {
      store <- ragnar_store_create(
        location = temp,
        version = 2,
        embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100)),
        overwrite = TRUE
      )
    },
    regexp = NA
  )
})

test_that("Can insert chunks with no origin", {
  skip_on_cran() # See comment in test-retrieve.R
  store <- ragnar_store_create(
    version = 2,
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100))
  )
  maybe_set_threads(store)
  expect_true(grepl("^store_[0-9]+$", store@name))

  doc <- test_doc()
  chunks <- doc |> read_as_markdown() |> markdown_chunk()
  chunks2 <- chunks
  chunks2@document@origin <- NULL

  expect_no_error(ragnar_store_insert(store, chunks2))
  expect_no_error(ragnar_store_insert(store, chunks2))

  n_docs <- tbl(store@con, "chunks") |> distinct(doc_id) |> collect() |> nrow()
  expect_equal(n_docs, 2)

  expect_error(ragnar_store_update(store, chunks2), "Can't update")
})
