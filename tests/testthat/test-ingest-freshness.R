local_freshness_site <- function(last_modified, .local_envir = parent.frame()) {
  app <- webfakes::new_app()
  app$locals$revision <- 1L
  app$locals$n_gets <- 0L
  app$locals$last_modified <- last_modified
  app$use(function(req, res, locals) {
    if (req$path == "/requests") {
      return(res$send_json(list(n_gets = locals$n_gets)))
    }
    if (locals$last_modified) {
      modified <- if (locals$revision == 1L) {
        "Fri, 16 Oct 2026 12:00:00 GMT"
      } else {
        "Fri, 16 Oct 2026 12:00:01 GMT"
      }
      res$set_header("Last-Modified", modified)
    }
    if (req$method == "head") {
      return(res$send(""))
    }
    body <- if (locals$revision == 1L) "First revision." else "Second revision."
    locals$revision <- 2L
    locals$n_gets <- locals$n_gets + 1L
    res$set_header("Content-Type", "text/plain; charset=utf-8")
    res$send(body)
  })
  server <- webfakes::new_app_process(app)
  withr::defer(server$stop(), envir = .local_envir)
  server
}

test_that("HTTP freshness describes the content that was prepared", {
  skip_on_cran()

  for (last_modified in c(FALSE, TRUE)) {
    local({
      server <- local_freshness_site(last_modified)
      path <- server$url("/document")
      store <- ragnar_store_create(embed = NULL)
      withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
      prepare <- function(path) {
        text <- httr2::request(path) |>
          httr2::req_perform() |>
          httr2::resp_body_string()
        ragnar::MarkdownDocument(text, origin = path) |> ragnar::markdown_chunk()
      }

      result <- ragnar_store_ingest(
        store, path, prepare = prepare,
        n_workers = 1L, progress = FALSE, build_index = FALSE
      )
      expect_equal(attr(result, "ingest_summary")$status, "ok")
      expect_equal(
        DBI::dbGetQuery(store@con, "SELECT text FROM documents")$text,
        "First revision."
      )
      requests <- httr2::request(server$url("/requests")) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE)
      expect_equal(requests$n_gets, 1L)

      result <- ragnar_store_ingest(
        store, path, prepare = prepare,
        n_workers = 1L, progress = FALSE, build_index = FALSE
      )
      expect_equal(attr(result, "ingest_summary")$status, "ok")
      expect_equal(
        DBI::dbGetQuery(store@con, "SELECT text FROM documents")$text,
        "Second revision."
      )

      result <- ragnar_store_ingest(
        store, path, prepare = prepare,
        n_workers = 1L, progress = FALSE, build_index = FALSE
      )
      expect_equal(attr(result, "ingest_summary")$status, "skipped_unchanged")
      requests <- httr2::request(server$url("/requests")) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE)
      expect_equal(requests$n_gets, if (last_modified) 2L else 3L)
    })
  }
})

test_that("files changed during preparation are ingested again", {
  skip_on_cran()

  path <- withr::local_tempfile(fileext = ".md")
  writeLines("First revision.", path)
  Sys.setFileTime(path, as.POSIXct("2026-10-16 12:00:00", tz = "UTC"))
  store <- ragnar_store_create(embed = NULL)
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
  prepare <- function(path) {
    text <- readLines(path)
    if (identical(text, "First revision.")) {
      writeLines("Second revision.", path)
      Sys.setFileTime(path, as.POSIXct("2026-10-16 12:00:01", tz = "UTC"))
    }
    ragnar::MarkdownDocument(text, origin = path) |> ragnar::markdown_chunk()
  }

  ragnar_store_ingest(
    store, path, prepare = prepare,
    n_workers = 1L, progress = FALSE, build_index = FALSE
  )
  expect_equal(
    DBI::dbGetQuery(store@con, "SELECT text FROM documents")$text,
    "First revision."
  )
  result <- ragnar_store_ingest(
    store, path, prepare = prepare,
    n_workers = 1L, progress = FALSE, build_index = FALSE
  )
  expect_equal(attr(result, "ingest_summary")$status, "ok")
  expect_equal(
    DBI::dbGetQuery(store@con, "SELECT text FROM documents")$text,
    "Second revision."
  )
})
