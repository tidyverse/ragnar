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
