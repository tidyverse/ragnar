test_that("crawl problems keep the documented link and problem columns", {
  site <- local_link_site(
    c("/" = '<a href="/missing.html">Missing</a>'),
    status_codes = c("/missing.html" = 404L)
  )

  expect_warning(
    links <- ragnar_find_links(site$url(), depth = 1, progress = FALSE),
    "could not be followed"
  )
  problems <- attr(links, "problems")
  expect_named(problems, c("link", "problem"))
  expect_equal(problems$link, site$url("/missing.html"))
  expect_type(problems$problem, "character")
})

test_that("sitemap and validation failures share the documented problem schema", {
  site <- local_link_site(
    c("/good.html" = "<p>Good</p>"),
    status_codes = c("/missing.html" = 404L)
  )
  sitemap <- withr::local_tempfile(fileext = ".xml", lines = paste0(
    "<urlset>",
    "<url><loc>None</loc></url>",
    "<url><loc>", site$url("/missing.html"), "</loc></url>",
    "<url><loc>", site$url("/good.html"), "</loc></url>",
    "</urlset>"
  ))

  expect_warning(
    links <- ragnar_find_links(sitemap, validate = TRUE),
    "could not be followed"
  )
  expect_equal(as.character(links), site$url("/good.html"))
  problems <- attr(links, "problems")
  expect_named(problems, c("link", "problem"))
  expect_equal(problems$link, c("None", site$url("/missing.html")))
  expect_match(problems$problem[1], "Sitemap <loc>")
  expect_match(problems$problem[2], "404")
  expect_false(anyNA(problems))
})
