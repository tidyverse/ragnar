test_that("ragnar_read", {
  skip_on_cran()
  doc <- test_doc()

  # Reading the document without any arguments yields a single row data frame
  document <- ragnar_read(doc)
  expect_equal(nrow(document), 1)
  expect_equal(colnames(document), c("origin", "hash", "text"))

  # Reading the document with frame_by_tags argument yields a data frame with
  # multiple rows and additional columns
  tags <- c("h1", "h2", "h3")
  document <- ragnar_read(doc, frame_by_tags = tags)
  expect_gt(nrow(document), 1)
  expect_in(tags, colnames(document))

  # We can also read the document and just split by some tags
  tags <- c("p", "pre")
  document <- ragnar_read(doc, split_by_tags = tags)
  expect_gt(nrow(document), 1)
  expect_in("tag", colnames(document))
  expect_in(tags, document$tag)

  # We can do both, frame by tag and split by them
  frame_tags <- c("h1", "h2", "h3")
  split_tags <- c("p", "pre")
  document <- ragnar_read(
    doc,
    frame_by_tags = frame_tags,
    split_by_tags = split_tags
  )
  expect_gt(nrow(document), 1)
  expect_in(frame_tags, colnames(document))
  expect_in("tag", colnames(document))
  expect_in(split_tags, document$tag)
})


test_that("ragnar_read() empty doc", {
  jpg <- file.path(R.home("doc"), "html", "logo.jpg")
  expect_no_error(ragnar_read(jpg, frame_by_tags = "h1"))
})


test_that("markitdown patches", {
  skip_if_offline()
  url = "https://quarto.org/docs/computations/r.html"
  htmlfile <- withr::local_tempfile(fileext = ".html")
  download.file(url, htmlfile, quiet = TRUE)

  # ensure that 'main_only' returns a shorter document (omitting sidebar)
  main_only <- read_as_markdown(htmlfile, main_only = TRUE)
  not_main_only <- read_as_markdown(htmlfile, main_only = FALSE)
  expect_lt(stri_length(main_only), stri_length(not_main_only))

  # ensure that 'main_only' is a strict slice of the full converted document
  # (modulo the page title)
  main_only_sans_title <- stri_replace_first_regex(main_only, "^# .*\n+", "")
  expect_true(stri_detect_fixed(not_main_only, main_only_sans_title))

  # ensure that some fences have been expanded
  expect_true(stri_detect_fixed(main_only, "````"))
  expect_true(stri_detect_fixed(not_main_only, "````"))
})
