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
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = '<html><body><a href="https://example.com/b">B</a></body></html>'
  )

  result <- ragnar_find_links(html)
  lm <- attr(result, "last_modified")
  # Only the start page was visited; its timestamp uses the resolved path.
  expect_named(lm, normalizePath(html, winslash = "/"))
})

test_that("ragnar_find_links() returns a ragnar_links object", {
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = '<html><body><a href="https://example.com/page">Page</a></body></html>'
  )

  result <- ragnar_find_links(html)
  expect_s3_class(result, "ragnar_links")
  expect_s3_class(result, "character")
})

test_that("a single discovered link can be read and stored", {
  skip_on_cran() # See comment in test-read-markdown.R
  site <- local_link_site(c(
    "/" = '<a href="/topic.html">Topic</a>',
    "/topic.html" = "<h1>Topic</h1><p>A document to ingest.</p>"
  ))
  links <- ragnar_find_links(site$url())
  expect_length(links, 1L)

  chunks <- links |> read_as_markdown() |> markdown_chunk()
  store <- ragnar_store_create(
    embed = \(x) matrix(1, nrow = length(x), ncol = 3)
  )
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))

  expect_no_error(ragnar_store_update(store, chunks))
  expect_no_error(ragnar_store_update(store, chunks))
  documents <- tbl(store@con, "documents") |> collect()
  expect_equal(documents$origin, site$url("/topic.html"))
  expect_equal(tbl(store@con, "chunks") |> collect() |> nrow(), nrow(chunks))
})

test_that("discovered links can be joined with cached character URLs", {
  site <- local_link_site(c(
    "/" = '<a href="/a">A</a><a href="/b">B</a>',
    "/a" = "<p>A</p>",
    "/b" = "<p>B</p>"
  ))
  links <- ragnar_find_links(site$url(), get_timestamps = TRUE)
  urls <- as.character(links)
  modified <- .POSIXct(as.numeric(attr(links, "last_modified")[urls]), tz = "UTC")
  discovered <- tibble(url = links, last_modified = modified)
  cached <- tibble(url = rev(urls), content = c("B", "A"))

  expect_equal(
    left_join(discovered, cached, by = "url"),
    tibble(url = urls, last_modified = modified, content = c("A", "B"))
  )
  expect_equal(
    left_join(cached, discovered, by = "url"),
    tibble(url = rev(urls), content = c("B", "A"), last_modified = rev(modified))
  )
})

test_that("discovered links can be row-bound across separate crawls", {
  html_a <- withr::local_tempfile(
    fileext = ".html",
    lines = '<a href="https://example.com/a">A</a>'
  )
  html_b <- withr::local_tempfile(
    fileext = ".html",
    lines = '<a href="https://example.com/b">B</a>'
  )
  first <- tibble(url = ragnar_find_links(html_a))
  second <- tibble(url = ragnar_find_links(html_b))

  expect_equal(
    bind_rows(first, second),
    tibble(url = c("https://example.com/a", "https://example.com/b"))
  )
  expect_equal(
    bind_rows(second, first),
    tibble(url = c("https://example.com/b", "https://example.com/a"))
  )
  expect_equal(
    bind_rows(first, first),
    tibble(url = rep("https://example.com/a", 2))
  )
  cached <- tibble(url = "https://example.com/cached")
  expect_equal(
    bind_rows(first, cached),
    tibble(url = c("https://example.com/a", "https://example.com/cached"))
  )
  expect_equal(
    bind_rows(cached, first),
    tibble(url = c("https://example.com/cached", "https://example.com/a"))
  )
})

test_that("empty crawls can be joined and row-bound with character URLs", {
  html <- withr::local_tempfile(fileext = ".html", lines = "<p>No links.</p>")
  empty <- tibble(url = ragnar_find_links(html))
  cached <- tibble(url = "https://example.com/cached")

  expect_equal(left_join(empty, cached, by = "url"), tibble(url = character()))
  expect_equal(left_join(cached, empty, by = "url"), cached)
  expect_equal(bind_rows(empty, cached), cached)
  expect_equal(bind_rows(cached, empty), cached)
  expect_equal(bind_rows(empty, empty), tibble(url = character()))
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

test_that("ragnar_find_links() normalizes local paths before child filtering", {
  fixture <- withr::local_tempdir()
  dir.create(file.path(fixture, "site"))
  writeLines(
    '<a href="topic.html">Topic</a><a href="../other.html">Other</a>',
    file.path(fixture, "site", "index.html")
  )
  writeLines(
    '<a href="details.html">Details</a>',
    file.path(fixture, "site", "topic.html")
  )
  writeLines("<p>Details</p>", file.path(fixture, "site", "details.html"))
  writeLines("<p>Other</p>", file.path(fixture, "other.html"))
  withr::local_dir(fixture)

  site_path <- normalizePath("site", winslash = "/")
  paths <- c(
    "site/index.html",
    "./site/index.html",
    "./site/../site/index.html",
    paste0(site_path, "/index.html")
  )
  for (path in paths) {
    for (depth in 0:1) {
      result <- ragnar_find_links(
        path, children_only = TRUE, depth = depth, progress = FALSE
      )
      children <- if (depth == 0) "topic.html" else c("details.html", "topic.html")
      expected <- paste0(site_path, "/", children)
      expect_equal(result, expected, ignore_attr = TRUE)
      visited <- if (depth == 0) "index.html" else c("index.html", "topic.html")
      expect_named(attr(result, "last_modified"), paste0(site_path, "/", visited))
    }
  }
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

test_that("ragnar_find_links() ignores timestamps from failed HEAD responses", {
  site <- local_link_site(
    c(
      "/" = paste0(
        '<a href="/ok">OK</a>',
        '<a href="/missing">Missing</a>',
        '<a href="/unavailable">Unavailable</a>'
      ),
      "/ok" = "<p>OK</p>"
    ),
    status_codes = c("/missing" = 404L, "/unavailable" = 503L)
  )

  result <- ragnar_find_links(site$url(), get_timestamps = TRUE, validate = FALSE)
  urls <- paste0(site$url(), c("missing", "ok", "unavailable"))
  expect_equal(result, urls, ignore_attr = TRUE)
  timestamps <- attr(result, "last_modified")
  expect_named(timestamps, sort(c(site$url(), urls)))
  expect_true(all(is.na(timestamps[site$url(c("/missing", "/unavailable"))])))
  expect_equal(
    as.numeric(timestamps[site$url("/ok")]),
    as.numeric(as.POSIXct("2015-10-21 07:28:00", tz = "UTC"))
  )
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
