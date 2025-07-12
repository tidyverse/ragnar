test_doc <- function() {
  tmp <- tempfile(fileext = ".html")

  httr2::request("https://r4ds.hadley.nz/base-R.html") |>
    httr2::req_cache(path = tempdir()) |>
    httr2::req_perform(path = tmp)

  tmp
}

maybe_on_cran <- function() {
  ## CRAN never sets these envvars,
  ## they're just so we can set them during our tests.
  Sys.getenv("NOT_CRAN") != "true" || Sys.getenv("CRAN") == "true"
}

maybe_set_threads <- function(store, force = FALSE) {
  if (maybe_on_cran() || force) {
    DBI::dbExecute(
      store@con,
      "
        SET threads TO 1;
        SET worker_threads TO 1;
      "
    )
    if (is_windows()) skip("limiting duckdb threads on windows doesn't work")
  }
  store
}

# mask testthat::skip_on_cran() for local testing
skip_on_cran <- function() {
  skip_if(maybe_on_cran(), "Maybe On CRAN")
}

skip_if_cant_use_motherduck <- function() {
  if (Sys.getenv("motherduck_token") == "") {
    testthat::skip("motherduck_token not set")
  }
  testthat::skip("motherduck_token not set")

  tryCatch(
    {
      con <- DBI::dbConnect(duckdb::duckdb(), array = "matrix")
      DBI::dbExecute(con, "INSTALL 'motherduck'")
      DBI::dbExecute(con, "LOAD 'motherduck'")
      DBI::dbExecute(con, "ATTACH 'md:'")
    },
    error = function(e) {
      if (grepl("Please use DuckDB v", e$message, fixed = TRUE)) {
        testthat::skip("DuckDB version not supported by MotherDuck")
      }
    }
  )
}
