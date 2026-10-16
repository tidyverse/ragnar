## Tests for #205: worker errors emit structured conditions

# The production change that would make these tests fail:
# reverting to bare warning(cond) without a ragnar_ingest_worker_error class.

test_that("worker error emits a classed ragnar_ingest_worker_error warning", {
  skip_on_cran()

  store <- ragnar_store_create(
    tempfile(fileext = ".store"),
    embed = NULL,
    overwrite = TRUE
  )
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))

  bad_prepare <- function(path) stop("intentional worker failure")

  expect_warning(
    ragnar_store_ingest(
      store,
      paths = "https://example.com/page1.html",
      prepare = bad_prepare,
      progress = FALSE,
      build_index = FALSE
    ),
    regexp = "intentional worker failure",
    class = "ragnar_ingest_worker_error"
  )
})

test_that("ragnar_ingest_worker_error warning carries the failed path", {
  skip_on_cran()

  store <- ragnar_store_create(
    tempfile(fileext = ".store"),
    embed = NULL,
    overwrite = TRUE
  )
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))

  for (prepare in list(
    function(path) stop("boom"),
    function(path) NULL
  )) {
    captured <- NULL
    withCallingHandlers(
      ragnar_store_ingest(
        store,
        paths = "https://example.com/fail-me.html",
        prepare = prepare,
        n_workers = 1L,
        progress = FALSE,
        build_index = FALSE
      ),
      ragnar_ingest_worker_error = function(w) {
        captured <<- w
        invokeRestart("muffleWarning")
      }
    )

    expect_s3_class(captured, "ragnar_ingest_worker_error")
    expect_equal(captured$path, "https://example.com/fail-me.html")
  }
})

test_that("ingest continues past worker errors (does not abort)", {
  skip_on_cran()

  good_path <- tempfile(fileext = ".md")
  writeLines(c("# Hello", "Some content."), good_path)

  store <- ragnar_store_create(
    tempfile(fileext = ".store"),
    embed = NULL,
    overwrite = TRUE
  )
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))

  bad_prepare <- function(path) {
    if (grepl("bad", path)) {
      stop("intentional failure")
    }
    path |> ragnar::read_as_markdown() |> ragnar::markdown_chunk()
  }

  expect_warning(
    ragnar_store_ingest(
      store,
      paths = c("bad://not-real/page.html", good_path),
      prepare = bad_prepare,
      progress = FALSE,
      build_index = FALSE,
      n_workers = 1L
    ),
    class = "ragnar_ingest_worker_error"
  )

  # The good path should still have been ingested
  n <- DBI::dbGetQuery(store@con, "SELECT COUNT(*) AS n FROM documents")$n
  expect_equal(n, 1L)
})

test_that("worker exits warn with their path and allow ingestion to finish", {
  skip_on_cran()

  store <- ragnar_store_create(
    withr::local_tempfile(fileext = ".store"),
    embed = NULL
  )
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))

  prepare <- function(path) {
    if (path == "exiting-worker") {
      quit(save = "no", status = 1L)
    }
    ragnar::MarkdownDocument("Some content.", origin = path) |>
      ragnar::markdown_chunk()
  }

  captured <- NULL
  withCallingHandlers(
    result <- ragnar_store_ingest(
      store,
      paths = c("exiting-worker", "valid-worker"),
      prepare = prepare,
      n_workers = 2L,
      progress = FALSE,
      build_index = FALSE
    ),
    ragnar_ingest_worker_error = function(w) {
      captured <<- w
      invokeRestart("muffleWarning")
    }
  )

  expect_identical(result, store)
  expect_s3_class(captured, "ragnar_ingest_worker_error")
  expect_equal(captured$path, "exiting-worker")
  expect_equal(
    DBI::dbGetQuery(store@con, "SELECT origin FROM documents")$origin,
    "valid-worker"
  )
})
