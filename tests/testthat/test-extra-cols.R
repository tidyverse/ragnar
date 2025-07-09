test_that("extra cols works", {

  store <- ragnar_store_create(
    version = 2,
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100)),
    extra_cols = data.frame(
      number = numeric(0),
      integer = integer(0),
      char = character(0),
      date  = as.Date(character(0))
    )
  )

  chunks <- test_doc() |> 
    read_as_markdown() |> 
    markdown_chunk()

  chunks_not_ok <- chunks |> mutate(
    number = "not a number",
    integer = "not an integer",
    char = 1,
    date = "not a date"
  )

  # Can't insert chunks that don't match the types
  expect_error(
    ragnar_store_insert(store, chunks_not_ok),
    regexp = "Could not convert string"
  )
  
  chunks_ok <- chunks |> mutate(
    number = 1.23,
    integer = 42L,
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

})