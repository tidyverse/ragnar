test_that("snowflake embeddings works", {
  account <- Sys.getenv("SNOWFLAKE_ACCOUNT")
  testthat::skip_if(account == "")
  testthat::skip_if(!snowflake_credentials_exist(account))

  model <- "snowflake-arctic-embed-m-v1.5"
  text <- c("hello world", "another hello world")

  embs_single_1 <- embed_snowflake(
    text[1],
    account = account,
    model = model
  )

  embs_single_2 <- embed_snowflake(
    text[2],
    account = account,
    model = model
  )

  embs_batch <- embed_snowflake(
    text,
    account = account,
    model = model,
    batch_size = 1L
  )

  expect_equal(embs_single_1[1, ], embs_batch[1, ])
  expect_equal(embs_single_2[1, ], embs_batch[2, ])
})

test_that("embed_snowflake returns NULL for empty inputs", {
  expect_null(
    embed_snowflake(
      character(),
      account = "placeholder",
      credentials = function() list()
    )
  )
})
