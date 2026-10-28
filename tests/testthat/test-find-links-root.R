test_that("ragnar_find_links() includes the root only when requested", {
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = '<html><body><a href="https://example.com/page">Page</a></body></html>'
  )

  url <- "https://example.com/page"
  expect_equal(ragnar_find_links(html), url, ignore_attr = TRUE)
  expect_equal(
    ragnar_find_links(html, include_root = FALSE), url, ignore_attr = TRUE
  )
  expect_equal(
    ragnar_find_links(html, include_root = TRUE),
    sort(c(normalizePath(html, winslash = "/"), url)),
    ignore_attr = TRUE
  )
})

test_that("ragnar_find_links() controls root inclusion independently of child scoping", {
  site <- local_link_site(c(
    "/manual/index.html" = paste0(
      '<a href="topic.html">Child</a>',
      '<a href="/other/topic.html">Sibling</a>'
    ),
    "/manual/topic.html" = "<p>Child</p>",
    "/other/topic.html" = "<p>Sibling</p>"
  ))
  root <- site$url("/manual/index.html")

  for (children_only in c(FALSE, TRUE)) {
    urls <- site$url("/manual/topic.html")
    if (!children_only) {
      urls <- c(urls, site$url("/other/topic.html"))
    }
    expect_equal(
      ragnar_find_links(root, children_only = children_only),
      urls,
      ignore_attr = TRUE
    )
    for (include_root in c(FALSE, TRUE)) {
      expected <- if (include_root) sort(c(root, urls)) else urls
      expect_equal(
        ragnar_find_links(
          root, children_only = children_only, include_root = include_root
        ),
        expected,
        ignore_attr = TRUE
      )
    }
  }
})

test_that("ragnar_find_links() filters an explicitly included root", {
  url <- "https://example.com/page"
  html <- withr::local_tempfile(
    fileext = ".html", lines = paste0('<a href="', url, '">Page</a>')
  )
  root <- normalizePath(html, winslash = "/")

  expect_equal(
    ragnar_find_links(
      html, children_only = "https://example.com/", include_root = TRUE
    ),
    url,
    ignore_attr = TRUE
  )
  expect_equal(
    ragnar_find_links(
      html, url_filter = \(urls) setdiff(urls, root), include_root = TRUE
    ),
    url,
    ignore_attr = TRUE
  )
})

test_that("ragnar_find_links() deduplicates roots with absolute self-links", {
  app <- webfakes::new_app()
  app$use(function(req, res) {
    url <- paste0("http://", req$get_header("host"), req$path)
    res$send(paste0(
      '<a href="', url, '">Self</a>',
      '<a href="topic.html">Topic</a>'
    ))
  })
  site <- webfakes::new_app_process(app)
  withr::defer(site$stop())

  for (path in c("/", "/manual/")) {
    for (children_only in c(FALSE, TRUE)) {
      for (include_root in c(FALSE, TRUE)) {
        result <- ragnar_find_links(
          site$url(path), children_only = children_only, include_root = include_root
        )
        expect_equal(
          result,
          site$url(c(path, paste0(path, "topic.html"))),
          ignore_attr = TRUE
        )
        expect_named(attr(result, "last_modified"), site$url(path))
      }
    }
  }
})

test_that("an absolute root self-link is ingested as one document", {
  skip_on_cran() # See comment in test-read-markdown.R
  app <- webfakes::new_app()
  app$get("/", function(req, res) {
    url <- paste0("http://", req$get_header("host"), "/")
    res$send(paste0(
      '<h1>Root</h1><p>A document to ingest.</p>',
      '<a href="', url, '">Self</a>'
    ))
  })
  site <- webfakes::new_app_process(app)
  withr::defer(site$stop())
  links <- ragnar_find_links(site$url(), include_root = TRUE)
  expect_equal(links, site$url(), ignore_attr = TRUE)

  store <- ragnar_store_create(
    embed = \(x) matrix(1, nrow = length(x), ncol = 3)
  )
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))

  expect_no_error(for (link in links) {
    chunks <- link |> read_as_markdown() |> markdown_chunk()
    ragnar_store_insert(store, chunks)
  })
  documents <- tbl(store@con, "documents") |> collect()
  expect_equal(documents$origin, site$url())
})

test_that("recursive back-links ingest the root only once", {
  skip_on_cran() # See comment in test-read-markdown.R
  app <- webfakes::new_app()
  app$get("/", function(req, res) {
    res$send('<h1>Root</h1><a href="child.html">Child</a>')
  })
  app$get("/child.html", function(req, res) {
    url <- paste0("http://", req$get_header("host"), "/")
    res$send(paste0('<h1>Child</h1><a href="', url, '">Root</a>'))
  })
  site <- webfakes::new_app_process(app)
  withr::defer(site$stop())
  urls <- site$url(c("/", "/child.html"))

  for (depth in 1:2) {
    links <- ragnar_find_links(
      site$url(), depth = depth, progress = FALSE, include_root = TRUE
    )
    expect_equal(links, urls, ignore_attr = TRUE)
    expect_named(attr(links, "last_modified"), urls)
  }

  store <- ragnar_store_create(
    embed = \(x) matrix(1, nrow = length(x), ncol = 3)
  )
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
  links <- ragnar_find_links(
    site$url(), depth = 1, progress = FALSE, include_root = TRUE
  )
  expect_no_error(for (link in links) {
    chunks <- link |> read_as_markdown() |> markdown_chunk()
    ragnar_store_insert(store, chunks)
  })
  documents <- tbl(store@con, "documents") |> collect()
  expect_equal(sort(documents$origin), urls)
})

test_that("known redirected root aliases are ingested only once", {
  skip_on_cran() # See comment in test-read-markdown.R
  app <- webfakes::new_app()
  app$use(function(req, res) {
    if (req$path == "/manual") {
      return(res$redirect("/manual/"))
    }
    if (req$path == "/redirect.html") {
      return(res$send(paste0(
        '<script>var redirects = {"": "/js-manual/"};',
        'window.location.replace(redirect);</script>'
      )))
    }
    alias <- if (req$path == "/js-manual/") "/redirect.html" else "/manual"
    res$send(paste0(
      '<h1>Manual</h1><p>A document to ingest.</p>',
      '<a href="', alias, '">Alias</a>'
    ))
  })
  site <- webfakes::new_app_process(app)
  withr::defer(site$stop())

  targets <- c("/manual" = "/manual/", "/redirect.html" = "/js-manual/")
  for (alias in names(targets)) {
    for (depth in 0:1) {
      links <- ragnar_find_links(
        site$url(alias), depth = depth, progress = FALSE,
        include_root = TRUE
      )
      expect_equal(links, site$url(targets[[alias]]), ignore_attr = TRUE)
      expect_named(attr(links, "last_modified"), site$url(targets[[alias]]))
    }
  }

  store <- ragnar_store_create(
    embed = \(x) matrix(1, nrow = length(x), ncol = 3)
  )
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
  links <- ragnar_find_links(site$url("/manual"), include_root = TRUE)
  expect_no_error(for (link in links) {
    chunks <- link |> read_as_markdown() |> markdown_chunk()
    ragnar_store_insert(store, chunks)
  })
  documents <- tbl(store@con, "documents") |> collect()
  expect_equal(documents$origin, site$url("/manual/"))
})

test_that("ragnar_find_links() preserves distinct slashless and directory pages", {
  site <- local_link_site(c(
    "/manual/" = '<a href="/manual">Slashless</a>',
    "/manual" = '<a href="/manual/">Directory</a><a href="/manual/details.html">Details</a>',
    "/manual/details.html" = "<p>Details</p>"
  ))

  for (depth in 0:1) {
    result <- ragnar_find_links(
      site$url("/manual/"), depth = depth, progress = FALSE,
      include_root = TRUE
    )
    paths <- c("/manual", "/manual/")
    if (depth == 1) {
      paths <- c(paths, "/manual/details.html")
    }
    expect_equal(result, site$url(paths), ignore_attr = TRUE)
    visited <- if (depth == 0) "/manual/" else c("/manual", "/manual/")
    expect_named(attr(result, "last_modified"), site$url(visited))
  }

  expect_equal(
    ragnar_find_links(site$url("/manual"), include_root = TRUE),
    site$url(c("/manual", "/manual/", "/manual/details.html")),
    ignore_attr = TRUE
  )
})

test_that("ragnar_find_links() treats an empty HTTP path as the site root", {
  app <- webfakes::new_app()
  app$get("/", function(req, res) {
    url <- paste0("http://", req$get_header("host"))
    res$send(paste0(
      '<a href="', url, '">Root</a>',
      '<a href="', url, '/">Root with slash</a>'
    ))
  })
  site <- webfakes::new_app_process(app)
  withr::defer(site$stop())

  for (url in c(sub("/$", "", site$url()), site$url())) {
    for (depth in 0:1) {
      result <- ragnar_find_links(
        url, depth = depth, progress = FALSE, include_root = TRUE
      )
      expect_equal(result, site$url(), ignore_attr = TRUE)
      expect_named(attr(result, "last_modified"), site$url())
    }
  }
})

test_that("ragnar_find_links() normalizes fragments in starting URLs", {
  site <- local_link_site(c(
    "/page.html" = paste0(
      '<a href="page.html#intro">Intro</a>',
      '<a href="page/topic.html">Topic</a>'
    ),
    "/page/topic.html" = "<p>Topic</p>"
  ))
  urls <- site$url(c("/page.html", "/page/topic.html"))

  for (depth in 0:1) {
    for (children_only in c(FALSE, TRUE)) {
      result <- ragnar_find_links(
        site$url("/page.html#intro"),
        depth = depth,
        children_only = children_only,
        progress = FALSE,
        include_root = TRUE
      )
      expect_equal(result, urls, ignore_attr = TRUE)
      visited <- if (depth == 0) urls[1] else urls
      expect_named(attr(result, "last_modified"), visited)
    }
  }
})

test_that("ragnar_find_links() preserves queries when stripping root fragments", {
  site <- local_link_site(c("/page.html" = "<p>No links.</p>"))
  url <- site$url("/page.html?view=full")

  result <- ragnar_find_links(paste0(url, "#intro"), include_root = TRUE)
  expect_equal(result, url, ignore_attr = TRUE)
  expect_named(attr(result, "last_modified"), url)
})

test_that("ragnar_find_links() preserves hashes in local root filenames", {
  html <- withr::local_tempfile(fileext = "#intro.html", lines = "<p>No links.</p>")

  expect_equal(
    ragnar_find_links(html, include_root = TRUE),
    normalizePath(html, winslash = "/"),
    ignore_attr = TRUE
  )
})

test_that("ragnar_find_links() excludes sitemap documents from its results", {
  urls <- c("https://example.com/a.html", "https://example.com/b.html")
  xml <- paste0(
    '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
    paste0("<url><loc>", urls, "</loc></url>", collapse = ""),
    "</urlset>"
  )
  sitemap <- withr::local_tempfile(fileext = "sitemap.xml", lines = xml)
  site <- local_link_site(c("/sitemap.xml" = xml))

  inputs <- list(
    sitemap,
    site$url("/sitemap.xml"),
    xml2::read_xml(site$url("/sitemap.xml"))
  )
  for (input in inputs) {
    for (include_root in c(FALSE, TRUE)) {
      expect_equal(
        ragnar_find_links(input, include_root = include_root),
        urls,
        ignore_attr = TRUE
      )
    }
  }
})

test_that("ragnar_find_links() preserves caller-owned XML namespaces", {
  xml <- paste0(
    '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
    '<url><loc>https://example.com/page.html</loc>',
    '<lastmod>2026-10-14</lastmod></url></urlset>'
  )
  site <- local_link_site(c("/sitemap.xml" = xml))
  doc <- xml2::read_xml(site$url("/sitemap.xml"))
  before <- as.character(doc)
  namespaces <- xml2::xml_ns(doc)
  ns <- c(sm = "http://www.sitemaps.org/schemas/sitemap/0.9")

  result <- ragnar_find_links(doc, include_root = TRUE)

  expect_equal(result, "https://example.com/page.html", ignore_attr = TRUE)
  expect_equal(as.character(doc), before)
  expect_equal(xml2::xml_ns(doc), namespaces)
  expect_equal(
    xml2::xml_text(xml2::xml_find_all(doc, "//sm:lastmod", ns)),
    "2026-10-14"
  )
})

test_that("ragnar_find_links() reads lastmod dates from sitemap XML", {
  urls <- c("https://example.com/a.html", "https://example.com/b.html")
  lastmods <- c("2024-03-15", "2025-11-01")
  xml <- paste0(
    '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
    paste0(
      "<url><loc>", urls, "</loc><lastmod>", lastmods, "</lastmod></url>",
      collapse = ""
    ),
    "</urlset>"
  )
  sitemap <- withr::local_tempfile(fileext = "sitemap.xml", lines = xml)

  result <- ragnar_find_links(sitemap)
  lm <- attr(result, "last_modified")

  expect_equal(
    as.numeric(lm[urls[1]]),
    as.numeric(as.POSIXct("2024-03-15", tz = "UTC"))
  )
  expect_equal(
    as.numeric(lm[urls[2]]),
    as.numeric(as.POSIXct("2025-11-01", tz = "UTC"))
  )
})

test_that("ragnar_find_links() reports relative sitemap locations", {
  xml <- paste0(
    '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
    '<url><loc>/page1/</loc><lastmod>2024-03-15</lastmod></url>',
    '<url><loc>/page2/</loc><lastmod>2025-11-01</lastmod></url>',
    '</urlset>'
  )
  site <- local_link_site(c("/sitemap.xml" = xml))
  expect_warning(result <- ragnar_find_links(site$url("/sitemap.xml")), "could not be followed")
  expect_equal(as.character(result), character())
  expect_equal(nrow(attr(result, "problems")), 2L)
})

test_that("ragnar_find_links() warns and records problems for non-HTTP sitemap <loc> values", {
  # Some sitemaps contain broken <loc> values such as "None" (unexpanded Quarto
  # template) or "$DEPLOY_PREVIEW_URL/..." (unexpanded CI variable). These are
  # not valid HTTP URLs and should be reported via the problems mechanism rather
  # than silently returned as URLs.
  xml <- paste0(
    '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
    '<url><loc>https://example.com/good/</loc><lastmod>2024-03-15</lastmod></url>',
    '<url><loc>None</loc><lastmod>2024-06-01</lastmod></url>',
    '<url><loc>$DEPLOY_PREVIEW_URL/page/</loc></url>',
    '</urlset>'
  )
  site <- local_link_site(c("/sitemap.xml" = xml))
  expect_warning(
    result <- ragnar_find_links(site$url("/sitemap.xml")),
    "could not be followed"
  )
  # Good URL is returned; bad ones are not
  expect_true(any(grepl("good", result)))
  expect_false(any(grepl("None|DEPLOY", result)))
  # Bad locs appear in problems
  problems <- attr(result, "problems")
  expect_true(any(grepl("None|DEPLOY", problems$link)))
})

test_that("ragnar_find_links() validates local paths and HTTP links together", {
  site <- local_link_site(c("/remote.html" = "<p>Remote</p>"))
  fixture <- withr::local_tempdir()
  writeLines(
    paste0(
      '<a href="topic.html">Local</a>',
      '<a href="', site$url("/remote.html"), '">Remote</a>'
    ),
    file.path(fixture, "index.html")
  )
  writeLines("<p>Local</p>", file.path(fixture, "topic.html"))
  withr::local_dir(fixture)

  expect_no_warning(result <- ragnar_find_links(
    "index.html", validate = TRUE, get_timestamps = TRUE,
    include_root = TRUE
  ))
  paths <- normalizePath(c("index.html", "topic.html"), winslash = "/")
  urls <- sort(c(paths, site$url("/remote.html")))
  expect_equal(result, urls, ignore_attr = TRUE)
  expect_null(attr(result, "problems"))
  timestamps <- attr(result, "last_modified")
  expect_named(timestamps, urls)
  expect_true(all(is.na(timestamps[paths])))
  expect_equal(
    as.numeric(timestamps[site$url("/remote.html")]),
    as.numeric(as.POSIXct("2015-10-21 07:28:00", tz = "UTC"))
  )

  requests <- httr2::request(site$url("/requests")) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
  expect_equal(requests$method, "head")
  expect_equal(requests$path, "/remote.html")
})

test_that("ragnar_find_links() validates file URIs without changing the URLs", {
  fixture <- withr::local_tempdir()
  writeLines("<p>Topic</p>", file.path(fixture, "topic.html"))
  writeLines("<p>Space</p>", file.path(fixture, "topic space.html"))
  paths <- normalizePath(
    file.path(fixture, c("topic.html", "topic space.html")), winslash = "/"
  )
  uris <- paste0("file:///", sub("^/", "", paths))
  uris <- gsub(" ", "%20", uris, fixed = TRUE)
  uris <- c(uris, sub("^file://", "file://localhost", uris[1]))
  html <- file.path(fixture, "index.html")
  writeLines(paste0('<a href="', uris, '">Topic</a>'), html)

  expect_no_warning(result <- ragnar_find_links(
    html, validate = TRUE, get_timestamps = TRUE,
    include_root = TRUE
  ))
  expect_equal(
    result, sort(c(normalizePath(html, winslash = "/"), uris)),
    ignore_attr = TRUE
  )
  expect_null(attr(result, "problems"))
  expect_true(all(is.na(attr(result, "last_modified"))))
})

test_that("ragnar_find_links() reports malformed file URIs and keeps valid links", {
  site <- local_link_site(c("/topic.html" = "<p>Remote</p>"))
  fixture <- normalizePath(withr::local_tempdir(), winslash = "/")
  writeLines("<p>Local</p>", file.path(fixture, "topic.html"))
  writeLines("<p>Space</p>", file.path(fixture, "topic space.html"))
  uri <- paste0("file:///", sub("^/", "", fixture), "/topic.html")
  malformed <- paste0("file:///", sub("^/", "", fixture), "/topic space.html")
  malformed <- c(malformed, sub("^file", "FILE", malformed))
  html <- file.path(fixture, "index.html")
  writeLines(
    paste0('<a href="', c(uri, site$url("/topic.html"), malformed), '">Link</a>'),
    html
  )

  expect_warning(
    result <- ragnar_find_links(
      html, validate = TRUE, get_timestamps = TRUE, include_root = TRUE
    ),
    "Some links could not be followed"
  )
  expect_equal(
    result, sort(c(normalizePath(html, winslash = "/"), uri, site$url("/topic.html"))),
    ignore_attr = TRUE
  )
  problems <- attr(result, "problems")
  expect_equal(problems$link, sort(malformed))
  expect_match(problems$problem, "Failed to parse URL", all = TRUE)
})

test_that("ragnar_find_links() validates and timestamps schemes regardless of case", {
  site <- local_link_site(c(
    "/" = "<p>Root</p>",
    "/TOPIC.html" = "<p>Uppercase</p>",
    "/Mixed.html" = "<p>Mixed case</p>"
  ))
  fixture <- normalizePath(withr::local_tempdir(), winslash = "/")
  writeLines("<p>Local</p>", file.path(fixture, "local.html"))
  writeLines("<p>Space</p>", file.path(fixture, "local space.html"))
  prefix <- paste0("file:///", sub("^/", "", fixture), "/")
  uris <- c(
    sub("^file", "FILE", paste0(prefix, "local.html")),
    sub("^file", "fIlE", paste0(prefix, "local%20space.html"))
  )
  remote <- c(
    sub("^http", "HTTP", site$url("/TOPIC.html")),
    sub("^http", "hTtP", site$url("/Mixed.html")),
    sub("^http", "HTTP", sub("/$", "", site$url()))
  )
  html <- file.path(fixture, "index.html")
  writeLines(paste0('<a href="', c(remote, uris), '">Link</a>'), html)
  expected_remote <- c(remote[1:2], paste0(remote[3], "/"))
  local <- c(normalizePath(html, winslash = "/"), uris)
  urls <- sort(c(local, expected_remote))

  for (validate in c(FALSE, TRUE)) {
    expect_no_warning(result <- ragnar_find_links(
      html, validate = validate, get_timestamps = TRUE,
      include_root = TRUE
    ))
    expect_equal(result, urls, ignore_attr = TRUE)
    expect_null(attr(result, "problems"))
    timestamps <- attr(result, "last_modified")
    expect_named(timestamps, urls)
    expect_equal(
      as.numeric(timestamps[expected_remote]),
      rep(as.numeric(as.POSIXct("2015-10-21 07:28:00", tz = "UTC")), 3)
    )
    expect_true(all(is.na(timestamps[local])))
  }

  requests <- httr2::request(site$url("/requests")) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
  expect_equal(sort(requests$path), sort(rep(c("/", "/TOPIC.html", "/Mixed.html"), 2)))
  expect_equal(requests$method, rep("head", 6))
})

test_that("ragnar_find_links() reports missing file URIs as their original URLs", {
  fixture <- normalizePath(withr::local_tempdir(), winslash = "/")
  uri <- paste0("file:///", sub("^/", "", fixture), "/missing%20topic.html")
  html <- file.path(fixture, "index.html")
  writeLines(paste0('<a href="', uri, '">Missing</a>'), html)

  expect_warning(
    result <- ragnar_find_links(html, validate = TRUE, include_root = TRUE),
    "Some links could not be followed"
  )
  expect_equal(result, normalizePath(html, winslash = "/"), ignore_attr = TRUE)
  expect_equal(attr(result, "problems"), tibble(link = uri, problem = "File does not exist"))
})

test_that("ragnar_find_links() validates a local root without outgoing links", {
  html <- withr::local_tempfile(fileext = ".html", lines = "<p>No links.</p>")

  expect_no_warning(result <- ragnar_find_links(
    html, validate = TRUE, include_root = TRUE
  ))
  expect_equal(result, normalizePath(html, winslash = "/"), ignore_attr = TRUE)
  expect_null(attr(result, "problems"))
})

test_that("ragnar_find_links() reports missing local files during validation", {
  fixture <- withr::local_tempdir()
  writeLines('<a href="missing.html">Missing</a>', file.path(fixture, "index.html"))
  withr::local_dir(fixture)

  expect_warning(
    result <- ragnar_find_links("index.html", validate = TRUE, include_root = TRUE),
    "Some links could not be followed"
  )
  root <- normalizePath("index.html", winslash = "/")
  expect_equal(result, root, ignore_attr = TRUE)
  expect_equal(
    attr(result, "problems"),
    tibble(link = file.path(dirname(root), "missing.html"), problem = "File does not exist")
  )
})
