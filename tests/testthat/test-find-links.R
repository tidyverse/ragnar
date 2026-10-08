test_that("ragnar_find_links() returns a character vector when there are no links", {
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = "<html><body><p>No links.</p></body></html>"
  )

  expect_identical(ragnar_find_links(html), character())
})

test_that("ragnar_find_links() returns no links when no children match", {
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = '<html><body><a href="https://example.com/page">Page</a></body></html>'
  )

  expect_identical(
    ragnar_find_links(html, children_only = "https://example.com/other"),
    character()
  )
})

test_that("ragnar_find_links() passes character vectors to url_filter", {
  html <- withr::local_tempfile(
    fileext = ".html",
    lines = '<html><body><a href="https://example.com/page">Page</a></body></html>'
  )

  expect_identical(
    ragnar_find_links(html, url_filter = function(urls) {
      stopifnot(is.character(urls))
      urls[FALSE]
    }),
    character()
  )
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

  expect_identical(
    ragnar_find_links(html),
    c("https://example.com/a", "https://example.com/b")
  )
})
