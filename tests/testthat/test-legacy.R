test_that("legacy chunking functions are deprecated", {
  text <- "First sentence. Second sentence."

  expect_snapshot({
    x <- ragnar_chunk(text, max_size = 20)
    x <- ragnar_segment(text)
    x <- ragnar_chunk_segments(c("First. ", "Second. "), max_size = 20)
  })
})
