test_that("ragnar_find_links() returns a character vector when there are no links", {
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = "<html><body><p>No links.</p></body></html>"
  )

  expect_equal(ragnar_find_links(html), character(), ignore_attr = TRUE)
})

test_that("ragnar_find_links() returns no links when no children match", {
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = '<html><body><a href="https://example.com/page">Page</a></body></html>'
  )

  expect_equal(
    ragnar_find_links(html, children_only = "https://example.com/other"),
    character(), ignore_attr = TRUE
  )
})

test_that("ragnar_find_links() passes character vectors to url_filter", {
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = '<html><body><a href="https://example.com/page">Page</a></body></html>'
  )

  expect_equal(
    ragnar_find_links(html, url_filter = function(urls) {
      stopifnot(is.character(urls))
      urls[FALSE]
    }),
    character(), ignore_attr = TRUE
  )
})

test_that("ragnar_find_links() attaches a last_modified attribute", {
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = '<html><body><a href="https://example.com/page">Page</a></body></html>'
  )

  result <- ragnar_find_links(html)
  lm <- attr(result, "last_modified")
  expect_s3_class(lm, "POSIXct")
  expect_named(lm)
})

test_that("ragnar_find_links() last_modified is NA for local files", {
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = '<html><body><a href="https://example.com/page">Page</a></body></html>'
  )

  result <- ragnar_find_links(html)
  lm <- attr(result, "last_modified")
  # Local files are not fetched via curl so modification time is unknown
  expect_true(all(is.na(lm)))
})

test_that("ragnar_find_links() last_modified names match resolved URLs", {
  html_a <- withr::local_tempfile(
    fileext = ".html",
    lines = '<html><body><a href="https://example.com/b">B</a></body></html>'
  )
  html_b <- withr::local_tempfile(
    fileext = ".html",
    lines = "<html><body><p>Page B</p></body></html>"
  )

  result <- ragnar_find_links(html_a)
  lm <- attr(result, "last_modified")
  # Names must be a subset of or equal to the returned URLs (resolved)
  expect_true(all(names(lm) %in% c(result, html_a)))
})

test_that("ragnar_find_links() returns a ragnar_links object", {
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = '<html><body><a href="https://example.com/page">Page</a></body></html>'
  )

  result <- ragnar_find_links(html)
  expect_s3_class(result, "ragnar_links")
})

test_that("print.ragnar_links() snapshot with last_modified", {
  urls <- paste0("https://example.com/page-", 1:15)
  lm <- structure(
    as.POSIXct(
      paste0("2026-01-0", 1:15, " 10:00:00"),
      tz = "UTC"
    ),
    names = urls
  )
  x <- structure(urls, class = "ragnar_links", last_modified = lm)
  expect_snapshot(print(x))
})

test_that("print.ragnar_links() snapshot without last_modified", {
  urls <- paste0("https://example.com/page-", 1:15)
  x <- structure(urls, class = "ragnar_links")
  expect_snapshot(print(x))
})

test_that("print.ragnar_links() truncates at 10 URLs", {
  urls <- paste0("https://example.com/page-", 1:15)
  x <- structure(urls, class = "ragnar_links")
  out <- capture.output(print(x))
  expect_true(any(grepl("5 more", out)))
  url_lines <- grep("example.com/page-1[1-5]", out, value = TRUE)
  expect_length(url_lines, 0L)
})

test_that("print.ragnar_links() prints problems separately", {
  urls <- c("https://example.com/a", "https://example.com/b")
  problems <- data.frame(link = "https://example.com/bad", problem = "404")
  x <- structure(urls, class = "ragnar_links", problems = problems)
  out <- capture.output(print(x))
  expect_true(any(grepl("problem", out, ignore.case = TRUE)))
})

test_that("print.ragnar_links() returns invisibly", {
  x <- structure(character(0), class = "ragnar_links")
  expect_invisible(print(x))
})

test_that("ragnar_find_links() get_timestamps=FALSE only has timestamps for visited pages", {
  # depth=0: only the start page is visited; discovered links are not fetched
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = c(
      '<html><body>',
      '<a href="https://example.com/a">A</a>',
      '<a href="https://example.com/b">B</a>',
      '</body></html>'
    )
  )

  result <- ragnar_find_links(html, get_timestamps = FALSE)
  lm <- attr(result, "last_modified")
  # discovered links (example.com) were not visited, so no timestamps for them
  expect_false(any(names(lm) %in% c("https://example.com/a", "https://example.com/b")))
})

test_that("ragnar_find_links() get_timestamps=TRUE covers all discovered URLs", {
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = c(
      '<html><body>',
      '<a href="https://example.com/a">A</a>',
      '<a href="https://example.com/b">B</a>',
      '</body></html>'
    )
  )

  result <- ragnar_find_links(html, get_timestamps = TRUE)
  lm <- attr(result, "last_modified")
  # every discovered URL should have an entry (NA is fine, but must be present)
  expect_true(all(result %in% names(lm)))
})

test_that("ragnar_find_links() returns sorted, unique links", {
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = c(
      '<html><body><a href="https://example.com/b">B</a>',
      '<a href="https://example.com/a">A</a>',
      '<a href="https://example.com/b#section">B section</a></body></html>'
    )
  )

  expect_equal(
    ragnar_find_links(html),
    c("https://example.com/a", "https://example.com/b"),
    ignore_attr = TRUE
  )
})
