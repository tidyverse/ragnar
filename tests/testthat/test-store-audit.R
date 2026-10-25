local_audit_store <- function(..., .local_envir = parent.frame()) {
  store <- ragnar_store_create(embed = NULL, ...)
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE), .local_envir)
  chunks <- MarkdownDocument("ABCDE", origin = "present") |>
    markdown_chunk(target_size = 1L, target_overlap = 0, max_snap_dist = 0)
  ragnar_store_update(store, chunks)
  store
}

test_that("ragnar_store_audit() summarizes distinct expected origins", {
  skip_on_cran()
  store <- local_audit_store()

  result <- ragnar_store_audit(store, c("missing", "present", "present"))

  expect_identical(
    result,
    tibble::tibble(
      expected = 2L,
      ingested = 1L,
      missing = 1L,
      chunks = 5L,
      missing_urls = list("missing")
    )
  )
  expect_identical(
    ragnar_store_audit(store, "present"),
    tibble::tibble(
      expected = 1L,
      ingested = 1L,
      missing = 0L,
      chunks = 5L,
      missing_urls = list(character())
    )
  )
})

test_that("ragnar_store_audit() groups expected data frame sources", {
  skip_on_cran()
  store <- local_audit_store()
  expected <- tibble::tibble(
    source = c("present", "missing", "present", "present", "other"),
    guide = factor(c("b", "b", "b", "a", NA), levels = c("a", "b", "c")),
    last_modified = as.POSIXct("2026-01-01", tz = "UTC")
  )

  expect_identical(
    ragnar_store_audit(store, expected, group_by = "guide"),
    tibble::tibble(
      guide = factor(c("b", "a", NA), levels = c("a", "b", "c")),
      expected = c(2L, 1L, 1L),
      ingested = c(1L, 1L, 0L),
      missing = c(1L, 0L, 1L),
      chunks = c(5L, 5L, 0L),
      missing_urls = list("missing", character(), "other")
    )
  )
  expect_identical(
    ragnar_store_audit(store, as.data.frame(expected)),
    tibble::tibble(
      expected = 3L,
      ingested = 1L,
      missing = 2L,
      chunks = 5L,
      missing_urls = list(c("missing", "other"))
    )
  )
  expect_identical(
    ragnar_store_audit(store, dplyr::group_by(expected, guide)),
    ragnar_store_audit(store, expected)
  )
})

test_that("ragnar_store_audit() reports empty expectations consistently", {
  skip_on_cran()
  store <- local_audit_store()

  expect_identical(
    ragnar_store_audit(store, character()),
    tibble::tibble(
      expected = 0L,
      ingested = 0L,
      missing = 0L,
      chunks = 0L,
      missing_urls = list(character())
    )
  )
  expected <- tibble::tibble(source = character(), guide = as.Date(character()))
  expect_identical(
    ragnar_store_audit(store, expected, group_by = "guide"),
    tibble::tibble(
      guide = as.Date(character()),
      expected = integer(),
      ingested = integer(),
      missing = integer(),
      chunks = integer(),
      missing_urls = list()
    )
  )
})

test_that("ragnar_store_audit() accepts discovered links", {
  skip_on_cran()
  store <- local_audit_store()
  sitemap <- withr::local_tempfile(fileext = ".xml")
  writeLines(
    '<urlset><url><loc>https://example.com/missing</loc></url></urlset>',
    sitemap
  )
  links <- ragnar_find_links(sitemap, children_only = FALSE, validate = FALSE)

  expect_identical(
    ragnar_store_audit(store, links),
    tibble::tibble(
      expected = 1L,
      ingested = 0L,
      missing = 1L,
      chunks = 0L,
      missing_urls = list("https://example.com/missing")
    )
  )
})

test_that("ragnar_store_audit() works on a read-only store connection", {
  skip_on_cran()
  path <- withr::local_tempfile(fileext = ".duckdb")
  local({
    store <- local_audit_store(path)
    expect_equal(ragnar_store_audit(store, "present")$chunks, 5L)
  })
  store <- ragnar_store_connect(path, read_only = TRUE)
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))

  expect_identical(
    ragnar_store_audit(store, c("present", "missing")),
    tibble::tibble(
      expected = 2L,
      ingested = 1L,
      missing = 1L,
      chunks = 5L,
      missing_urls = list("missing")
    )
  )
})

test_that("ragnar_store_audit() rejects unsupported inputs", {
  skip_on_cran()
  store <- local_audit_store()
  expect_error(ragnar_store_audit(store, NA_character_), "NA values")
  expect_error(ragnar_store_audit(store, data.frame(url = "present")), "source")
  expect_error(
    ragnar_store_audit(store, "present", group_by = "guide"),
    "guide"
  )
  expect_error(
    ragnar_store_audit(
      store,
      data.frame(source = "present", expected = "guide"),
      group_by = "expected"
    ),
    "summary column"
  )

  v1 <- ragnar_store_create(embed = NULL, version = 1)
  withr::defer(DBI::dbDisconnect(v1@con, shutdown = TRUE))
  expect_error(ragnar_store_audit(v1, character()), "only supports version 2")
})
