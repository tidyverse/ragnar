test_that("Gemini embeddings are consistent across batch sizes", {
  testthat::skip_if(Sys.getenv("GEMINI_API_KEY") == "")

  embs1 <- embed_google_gemini("hello world")
  embs2 <- embed_google_gemini("another hello world")
  embs <- embed_google_gemini(c("hello world", "another hello world"))

  expect_equal(embs1[1, ], embs[1, ])
  expect_equal(embs2[1, ], embs[2, ])
})
