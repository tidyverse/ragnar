test_that("ragnar_find_links() returns metadata rows only when requested", {
  urls <- c("https://example.com/b", "https://example.com/a")
  sitemap <- withr::local_tempfile(fileext = ".xml", lines = paste0(
    "<urlset>",
    "<url><loc>", urls[1], "</loc><lastmod>2024-03-15</lastmod></url>",
    "<url><loc>", urls[2], "</loc><lastmod>2024-04-16</lastmod></url>",
    "<url><loc>", urls[1], "</loc><lastmod>2024-03-15</lastmod></url>",
    "</urlset>"
  ))

  links <- ragnar_find_links(sitemap)
  expect_identical(ragnar_find_links(sitemap, metadata = FALSE), links)
  expect_s3_class(links, "ragnar_links")
  rows <- ragnar_find_links(sitemap, metadata = TRUE)
  expect_equal(rows, tibble(
    source = rev(urls),
    last_modified = as.POSIXct(c("2024-04-16", "2024-03-15"), tz = "UTC")
  ))
  expect_null(attr(rows, "last_modified"))

  expect_equal(
    ragnar_find_links(sitemap, metadata = TRUE, url_filter = \(x) x[1]),
    rows[1, ]
  )
})

test_that("metadata rows cover only returned links with typed missing dates", {
  url <- "https://example.com/page"
  html <- withr::local_tempfile(
    fileext = ".html", lines = paste0('<a href="', url, '">Page</a>')
  )
  expected <- tibble(source = url, last_modified = .POSIXct(NA_real_, tz = "UTC"))
  expect_equal(ragnar_find_links(html, metadata = TRUE), expected)
  expect_equal(
    ragnar_find_links(html, metadata = TRUE, url_filter = \(x) character()),
    expected[FALSE, ]
  )
  rows <- ragnar_find_links(html, metadata = TRUE, include_root = TRUE)
  expect_identical(rows$source, sort(c(normalizePath(html, winslash = "/"), url)))
  expect_s3_class(rows$last_modified, "POSIXct")
  expect_true(all(is.na(rows$last_modified)))
})

test_that("metadata columns preserve timestamps through joins and row-binding", {
  site <- local_link_site(c(
    "/" = '<a href="/topic.html">Topic</a>',
    "/topic.html" = "<p>Topic</p>"
  ))
  rows <- ragnar_find_links(site$url(), metadata = TRUE, get_timestamps = TRUE)
  expected <- tibble(
    source = site$url("/topic.html"),
    last_modified = as.POSIXct("2015-10-21 07:28:00", tz = "UTC")
  )
  expect_equal(rows, expected)
  expect_equal(dplyr::bind_rows(rows, rows), expected[c(1, 1), ])
  expect_equal(
    dplyr::left_join(tibble(source = rows$source), rows, by = "source"),
    expected
  )
})

test_that("metadata results retain crawl problems for links removed by validation", {
  site <- local_link_site(
    c("/" = '<a href="/missing.html">Missing</a>'),
    status_codes = c("/missing.html" = 404L)
  )
  expect_warning(
    rows <- ragnar_find_links(site$url(), metadata = TRUE, validate = TRUE),
    "could not be followed"
  )
  expect_named(rows, c("source", "last_modified"))
  expect_equal(nrow(rows), 0L)
  problems <- attr(rows, "problems")
  expect_equal(problems$link, site$url("/missing.html"))
  expect_match(problems$problem, "404")
})
