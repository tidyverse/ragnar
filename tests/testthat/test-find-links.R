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
  site <- local_link_site(c(
    "/" = '<a href="/a">A</a><a href="/b">B</a>',
    "/a" = "<p>A</p>",
    "/b" = "<p>B</p>"
  ))

  result <- ragnar_find_links(site$url(), get_timestamps = TRUE)
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

test_that("ragnar_find_links() children_only=TRUE scopes to the start URL stem", {
  site <- local_link_site(c(
    "/reference-architectures.html" = paste0(
      '<a href="/reference-architectures/page.html">Child</a>',
      '<a href="/other-section/page.html">Sibling</a>'
    ),
    "/reference-architectures/page.html" = "<p>Child</p>"
  ))

  expect_equal(
    ragnar_find_links(
      site$url("/reference-architectures.html"),
      children_only = TRUE,
      depth = 1,
      progress = FALSE
    ),
    site$url("/reference-architectures/page.html"),
    ignore_attr = TRUE
  )
})

test_that("ragnar_find_links() excludes .md source file links", {
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = c(
      '<html><body>',
      '<a href="https://example.com/page.html">Page</a>',
      '<a href="appendix/configuration/index.md">Source</a>',
      '</body></html>'
    )
  )

  result <- ragnar_find_links(html)
  expect_false(any(grepl("\\.md$", result)))
  expect_true(any(grepl("page\\.html$", result)))
})

test_that("ragnar_find_links() preserves valid repeated path segments", {
  urls <- c(
    "https://example.com/appendix/library/appendix/library/page.html",
    "https://example.com/appendix/library/page.html",
    "https://github.com/duckdb/duckdb"
  )
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = paste0('<a href="', urls, '">Link</a>')
  )

  expect_equal(ragnar_find_links(html), sort(urls), ignore_attr = TRUE)
})

test_that("ragnar_find_links() preserves and crawls distinct path segments", {
  site <- local_link_site(c(
    "/" = paste0(
      '<a href="/guide/guides.html">Guides</a>',
      '<a href="/en/enterprise.html">Enterprise</a>',
      '<a href="guide/guide/page.html">Repeated</a>'
    ),
    "/guide/guides.html" = '<a href="details.html">Details</a>',
    "/en/enterprise.html" = '<a href="details.html">Details</a>',
    "/guide/guide/page.html" = '<a href="details.html">Details</a>'
  ))

  expect_equal(
    ragnar_find_links(site$url(), depth = 1, progress = FALSE),
    sort(paste0(
      site$url(),
      c(
        "guide/guides.html",
        "guide/details.html",
        "en/enterprise.html",
        "en/details.html",
        "guide/guide/page.html",
        "guide/guide/details.html"
      )
    )),
    ignore_attr = TRUE
  )
})

test_that("ragnar_find_links() preserves links with repetitions in hosts and queries", {
  urls <- c(
    "https://example.com/example.com/topic.html",
    "https://example.com/topic.html?next=/guide/guide"
  )
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = paste0('<a href="', urls, '">Topic</a>')
  )

  expect_equal(ragnar_find_links(html), urls, ignore_attr = TRUE)
})

test_that("ragnar_find_links() scopes index.html pages to their directory", {
  site <- local_link_site(c(
    "/manual/index.html" = paste0(
      '<a href="topic.html">Topic</a>',
      '<a href="/manual-other/topic.html">Sibling</a>'
    ),
    "/manual/topic.html" = '<a href="details.html">Details</a>'
  ))

  expect_equal(
    ragnar_find_links(
      site$url("/manual/index.html"),
      children_only = TRUE,
      depth = 1,
      progress = FALSE
    ),
    paste0(site$url(), c("manual/details.html", "manual/topic.html")),
    ignore_attr = TRUE
  )
})

test_that("ragnar_find_links() follows the default entry across JS redirects", {
  site <- local_link_site(c(
    "/redirect.html" = paste0(
      '<script>var redirects = {"old": "/wrong.html", "": "nested/hop.html"};',
      'window.location.replace(redirect);</script>'
    ),
    "/nested/hop.html" = paste0(
      '<script>var redirects = {"": \'../right.html\'};',
      'window.location.replace(redirect);</script>'
    ),
    "/right.html" = '<a href="right-child.html">Child</a>',
    "/wrong.html" = '<a href="wrong-child.html">Child</a>'
  ))

  result <- ragnar_find_links(site$url("/redirect.html"))
  expect_equal(result, site$url("/right-child.html"), ignore_attr = TRUE)
  expect_named(attr(result, "last_modified"), site$url("/right.html"))
})

test_that("ragnar_find_links() parses HEAD timestamps in a non-English locale", {
  old_locale <- Sys.getlocale("LC_TIME")
  withr::defer(Sys.setlocale("LC_TIME", old_locale))
  locale_name <- if (.Platform$OS.type == "windows") {
    "French_France.utf8"
  } else {
    "fr_FR.UTF-8"
  }
  locale <- suppressWarnings(Sys.setlocale("LC_TIME", locale_name))
  skip_if(identical(locale, ""), "French LC_TIME is unavailable")

  site <- local_link_site(c(
    "/" = '<a href="/topic.html">Topic</a>',
    "/topic.html" = "<p>Topic</p>"
  ))
  result <- ragnar_find_links(site$url(), get_timestamps = TRUE)
  timestamps <- attr(result, "last_modified")
  expected <- as.POSIXct("2015-10-21 07:28:00", tz = "UTC")

  expect_equal(as.numeric(timestamps[site$url()]), as.numeric(expected))
  expect_equal(
    as.numeric(timestamps[site$url("/topic.html")]),
    as.numeric(expected)
  )
  expect_identical(Sys.getlocale("LC_TIME"), locale)
})

test_that("ragnar_find_links() detects JavaScript redirect cycles promptly", {
  site <- local_link_site(
    c(
      "/a.html" = paste0(
        '<script>var redirects = {"": "/b.html"};',
        'window.location.replace(redirect);</script>'
      ),
      "/b.html" = paste0(
        '<script>var redirects = {"": "/a.html"};',
        'window.location.replace(redirect);</script>'
      )
    ),
    max_requests = 6
  )

  expect_error(
    ragnar_find_links(site$url("/a.html")),
    "JavaScript redirect cycle"
  )
  requests <- httr2::request(site$url("/requests")) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
  expect_equal(requests$path, c("/a.html", "/b.html"))
})

test_that("ragnar_find_links() reuses validation HEAD responses for timestamps", {
  site <- local_link_site(c(
    "/" = '<a href="/a">A</a><a href="/b">B</a>',
    "/a" = "<p>A</p>",
    "/b" = "<p>B</p>"
  ))

  result <- ragnar_find_links(site$url(), validate = TRUE, get_timestamps = TRUE)
  urls <- paste0(site$url(), c("a", "b"))
  expect_equal(result, urls, ignore_attr = TRUE)
  timestamps <- attr(result, "last_modified")
  expected <- as.POSIXct("2015-10-21 07:28:00", tz = "UTC")
  expect_equal(as.numeric(timestamps[urls]), rep(as.numeric(expected), 2))

  requests <- httr2::request(site$url("/requests")) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
  expect_equal(sort(requests$path[requests$method == "head"]), c("/a", "/b"))
})
