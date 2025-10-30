test_that("azure embeddings works", {
  testthat::skip_if(Sys.getenv("AZURE_OPENAI_ENDPOINT") == "")
  testthat::skip_if(Sys.getenv("AZURE_OPENAI_API_KEY") == "")

  model <- "text-embedding-3-large"

  embs1 <- embed_azure_openai(
    "hello world",
    model = model
  )

  embs2 <- embed_azure_openai(
    "another hello world",
    model = model
  )

  embs <- embed_azure_openai(
    c("hello world", "another hello world"),
    model = model,
    batch_size = 1
  )

  expect_equal(embs1[1, ], embs[1, ])
  expect_equal(embs2[1, ], embs[2, ])
})
