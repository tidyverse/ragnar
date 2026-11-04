test_that("link discovery does not initialize Python", {
  local_mocked_bindings(
    ensure_python_initialized = function(...) {
      stop("Python is unavailable")
    },
    .package = "reticulate"
  )

  html <- withr::local_tempfile(
    fileext = ".html",
    lines = '<a href="https://example.com/b">B</a><a href="https://example.com/a">A</a><a href="https://example.com/b">B again</a>'
  )
  expect_equal(
    as.character(ragnar_find_links(html)),
    c("https://example.com/a", "https://example.com/b")
  )

  sitemap <- withr::local_tempfile(
    fileext = ".xml",
    lines = paste0(
      "<urlset><url><loc>https://example.com/a</loc>",
      "<lastmod>2024-03-15</lastmod></url></urlset>"
    )
  )
  expect_equal(
    ragnar_find_links(sitemap, metadata = TRUE),
    tibble(
      source = "https://example.com/a",
      last_modified = as.POSIXct("2024-03-15", tz = "UTC")
    )
  )
})
