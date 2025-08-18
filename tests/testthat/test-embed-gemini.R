test_that("gemini embeddings works", {
  testthat::skip_if(Sys.getenv("GEMINI_API_KEY") == "")

  models <- c("gemini-embedding-001", "gemini-embedding-exp-03-07")

  for (model in models) {
    embs1 <- embed_google_gemini(
      "hello world",
      model = model
    )

    embs2 <- embed_google_gemini(
      "another hello world",
      model = model
    )

    embs <- embed_google_gemini(
      c("hello world", "another hello world"),
      model = model
    )

    expect_equal(embs1[1, ], embs[1, ])
    expect_equal(embs2[1, ], embs[2, ])
  }
})
