test_that("ragnar_find_links() preserves sitemap datetime precision and offsets", {
  urls <- paste0("https://example.com/", letters[1:5])
  dates <- c(
    "2024-03-15",
    "2024-03-15T18:25:30Z",
    "2024-03-15T18:25:30+02:00",
    "2024-03-15T18:25:30.125-05:30",
    "2024-03-15T18:25Z"
  )
  sitemap <- withr::local_tempfile(
    fileext = "sitemap.xml",
    lines = paste0(
      "<urlset>",
      paste0(
        "<url><loc>",
        urls,
        "</loc><lastmod>",
        dates,
        "</lastmod></url>",
        collapse = ""
      ),
      "</urlset>"
    )
  )

  result <- ragnar_find_links(sitemap)
  expected <- as.POSIXct(
    c(
      "2024-03-15 00:00:00",
      "2024-03-15 18:25:30",
      "2024-03-15 16:25:30",
      "2024-03-15 23:55:30.125",
      "2024-03-15 18:25:00"
    ),
    format = "%Y-%m-%d %H:%M:%OS",
    tz = "UTC"
  )
  expect_equal(
    as.numeric(attr(result, "last_modified")[urls]),
    as.numeric(expected)
  )
})

test_that("ragnar_find_links() keeps dates aligned with normalized sitemap URLs", {
  sitemap <- withr::local_tempfile(
    fileext = "sitemap.xml",
    lines = paste0(
      "<urlset>",
      "<url><loc>https://example.com</loc><lastmod>2024-03-15</lastmod></url>",
      "<url><loc>https://example.com/page#heading</loc><lastmod>2024-06-01</lastmod></url>",
      "</urlset>"
    )
  )

  result <- ragnar_find_links(sitemap)
  urls <- c("https://example.com/", "https://example.com/page")
  expect_equal(as.character(result), urls)
  expect_equal(
    as.numeric(attr(result, "last_modified")[urls]),
    as.numeric(as.POSIXct(c("2024-03-15", "2024-06-01"), tz = "UTC"))
  )
})

test_that("ragnar_find_links() requests only missing sitemap dates", {
  app <- webfakes::new_app()
  app$locals$requests <- data.frame(method = character(), path = character())
  app$use(function(req, res, locals) {
    if (req$path == "/requests") {
      return(res$send_json(locals$requests))
    }
    locals$requests <- rbind(
      locals$requests,
      data.frame(method = req$method, path = req$path)
    )
    if (req$path == "/sitemap.xml") {
      root <- paste0("http://", req$get_header("host"))
      return(res$send(paste0(
        "<urlset>",
        "<url><loc>",
        root,
        "/dated</loc><lastmod>2024-03-15</lastmod></url>",
        "<url><loc>",
        root,
        "/missing</loc></url>",
        "<url><loc>",
        root,
        "/invalid</loc><lastmod>2024-03-15-invalid</lastmod></url>",
        "</urlset>"
      )))
    }
    res$set_header("Last-Modified", "Wed, 21 Oct 2015 07:28:00 GMT")
    res$send("<p>Page</p>")
  })
  site <- webfakes::new_app_process(app)
  withr::defer(site$stop())

  ragnar_find_links(
    site$url("/sitemap.xml"),
    get_timestamps = TRUE,
    url_filter = \(urls) urls[endsWith(urls, "/dated")]
  )
  requests <- httr2::request(site$url("/requests")) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
  expect_equal(requests$path[requests$method == "head"], character())

  result <- ragnar_find_links(site$url("/sitemap.xml"), get_timestamps = TRUE)
  urls <- site$url(c("/dated", "/invalid", "/missing"))
  expect_equal(as.character(result), urls)
  expect_equal(
    as.numeric(attr(result, "last_modified")[urls]),
    as.numeric(as.POSIXct(
      c("2024-03-15 00:00:00", rep("2015-10-21 07:28:00", 2)),
      tz = "UTC"
    ))
  )
  requests <- httr2::request(site$url("/requests")) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
  expect_equal(
    sort(requests$path[requests$method == "head"]),
    c("/invalid", "/missing")
  )
})

test_that("ragnar_find_links() reports absent and non-HTTP sitemap locations", {
  sitemap <- withr::local_tempfile(
    fileext = "sitemap.xml",
    lines = paste0(
      "<urlset>",
      "<url><loc>https://example.com/page</loc></url>",
      "<url><loc>ftp://example.com/file</loc></url>",
      "<url><lastmod>2024-03-15</lastmod></url>",
      "</urlset>"
    )
  )
  expect_warning(result <- ragnar_find_links(sitemap), "could not be followed")
  expect_equal(as.character(result), "https://example.com/page")
  expect_equal(nrow(attr(result, "problems")), 2L)
})

test_that("ragnar_find_links() retains sitemap dates when crawled pages omit headers", {
  app <- webfakes::new_app()
  app$use(function(req, res) {
    if (req$path == "/sitemap.xml") {
      root <- paste0("http://", req$get_header("host"))
      return(res$send(paste0(
        "<urlset>",
        "<url><loc>",
        root,
        "/dated</loc><lastmod>2024-03-15</lastmod></url>",
        "<url><loc>",
        root,
        "/redirect</loc><lastmod>2024-06-01</lastmod></url>",
        "<url><loc>",
        root,
        "/header</loc><lastmod>2024-06-01</lastmod></url>",
        "</urlset>"
      )))
    }
    if (req$path == "/redirect") {
      return(res$redirect("/final"))
    }
    if (req$path == "/header") {
      res$set_header("Last-Modified", "Wed, 21 Oct 2015 07:28:00 GMT")
    }
    res$send("<p>Page</p>")
  })
  site <- webfakes::new_app_process(app)
  withr::defer(site$stop())

  result <- ragnar_find_links(
    site$url("/sitemap.xml"),
    depth = 1,
    progress = FALSE
  )
  expect_equal(
    as.numeric(attr(result, "last_modified")[site$url("/dated")]),
    as.numeric(as.POSIXct("2024-03-15", tz = "UTC"))
  )
  expect_equal(
    as.numeric(attr(result, "last_modified")[site$url("/final")]),
    as.numeric(as.POSIXct("2024-06-01", tz = "UTC"))
  )
  expect_equal(
    as.numeric(attr(result, "last_modified")[site$url("/header")]),
    as.numeric(as.POSIXct("2015-10-21 07:28:00", tz = "UTC"))
  )
})
