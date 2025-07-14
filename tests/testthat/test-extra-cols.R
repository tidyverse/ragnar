test_that("extra cols works", {
  skip_on_cran() # See comment in test-retrieve.R and test-read-markdown.R
  store <- ragnar_store_create(
    version = 2,
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100)),
    extra_cols = data.frame(
      number = numeric(0),
      integer = integer(0),
      char = character(0),
      date = as.Date(character(0))
    )
  )
  maybe_set_threads(store)

  chunks <- test_doc() |>
    read_as_markdown() |>
    markdown_chunk()

  chunks_not_ok <- chunks |>
    mutate(
      number = "not a number",
      integer = "not an integer",
      char = 1,
      date = "not a date"
    )
  chunks_not_ok <- chunks |>
    mutate(
      number = "not a number",
      integer = "not an integer",
      char = 1,
      date = "not a date"
    )

  # Can't insert chunks that don't match the types
  expect_error(
    ragnar_store_insert(store, chunks_not_ok),
    regexp = "Can't convert"
  )

  chunks_ok <- chunks |>
    mutate(
      number = 1.23,
      integer = 42, # integers that are not formal integers are correctly converted
      char = "hello",
      date = as.Date("2023-10-01")
    )

  ragnar_store_insert(store, chunks_ok)
  ragnar_store_build_index(store)

  retrieve <- ragnar_retrieve(store, "utils", deoverlap = FALSE)
  expect_equal(
    retrieve[1, c("number", "integer", "char", "date")],
    tibble::tibble(
      number = 1.23,
      integer = 42L,
      char = "hello",
      date = as.Date("2023-10-01")
    )
  )

  # Also works for ragnar_store_update
  chunks_ok <- chunks |>
    mutate(
      number = 1.23,
      integer = 42, # integers that are not formal integers are correctly converted
      char = "hello2",
      date = as.Date("2023-10-01")
    )
  ragnar_store_update(store, chunks_ok)
  ragnar_store_build_index(store)

  retrieve <- ragnar_retrieve(store, "utils", deoverlap = FALSE)
  expect_equal(
    retrieve[1, c("char")],
    tibble::tibble(
      char = "hello2",
    )
  )
})

test_that("can specify extra cols with a full df", {
  skip_on_cran() # See comment in test-retrieve.R and test-read-markdown.R
  chunks <- test_doc() |>
    read_as_markdown() |>
    markdown_chunk()

  # no extra cols in chunks, but this works
  expect_no_error({
    store <- ragnar_store_create(
      version = 2,
      embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100)),
      extra_cols = chunks
    )
  })

  # now add some extra cols to chunks
  chunks_extra <- chunks |>
    mutate(
      number = 2.5,
      integer = 1L,
      char = "foo",
      date = as.Date("2023-10-01")
    )

  expect_no_error({
    store <- ragnar_store_create(
      version = 2,
      embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100)),
      extra_cols = chunks_extra
    )
  })

  expect_true(all(
    # text is not in schema
    names(chunks_extra |> select(-text)) %in% names(store@schema)
  ))
})
