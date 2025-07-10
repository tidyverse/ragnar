test_that("google vertex embeddings works", {
  testthat::skip_if(Sys.getenv("GOOGLE_PROJECT_ID") == "")

  project_id <- Sys.getenv("GOOGLE_PROJECT_ID")

  models <- c("gemini-embedding-001", "text-embedding-005")

  for (model in models) {
    embs1 <- embed_google_vertex(
      "hello world",
      model = model,
      project = project_id,
      location = "us-central1"
    )

    embs2 <- embed_google_vertex(
      "another hello world",
      model = model,
      project = project_id,
      location = "us-central1"
    )

    embs <- embed_google_vertex(
      c("hello world", "another hello world"),
      model = model,
      project = project_id,
      location = "us-central1"
    )

    expect_equal(embs1[1, ], embs[1, ])
    expect_equal(embs2[1, ], embs[2, ])

    # error when max tokens is reached
    expect_error(
      embed_google_vertex(
        paste(rep(letters, 10000), collapse = ""),
        model = model,
        project = project_id,
        location = "us-central1"
      ),
      regexp = "are longer than the maximum number of tokens"
    )
  }
})
