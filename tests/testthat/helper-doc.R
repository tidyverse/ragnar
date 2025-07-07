test_doc <- function() {
  tmp <- tempfile(fileext = ".html")

  httr2::request("https://r4ds.hadley.nz/base-R.html") |>
    httr2::req_cache(path = tempdir()) |>
    httr2::req_perform(path = tmp)

  tmp
}

maybe_on_cran <- function() {
  !identical(Sys.getenv("NOT_CRAN"), "true")
}

maybe_set_threads <- function(store) {
  if (maybe_on_cran()) {
    DBI::dbExecute(store@con, "SET threads TO 1;")
  }
  store
}

test_store <- function() {
  store <- ragnar_store_create(
    embed = \(x) matrix(nrow = length(x), ncol = 100, stats::runif(100))
  )
  
  doc <- test_doc()
  chunks <- read_as_markdown(doc) |> markdown_chunk()
  ragnar_store_insert(store, chunks)
  ragnar_store_build_index(store)
}
