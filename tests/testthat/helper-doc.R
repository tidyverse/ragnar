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
    DBI::dbExecute(store@.con, "SET threads TO 1;")
  }
  store
}
