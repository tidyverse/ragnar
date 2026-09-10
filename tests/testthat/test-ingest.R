test_that("basic ragnar ingest test", {
  skip_on_cran()

  # Use local documents so ingestion does not depend on remote servers.
  PATHS <- file.path(withr::local_tempdir(), paste0(seq_len(10), ".md"))
  for (i in seq_along(PATHS)) {
    writeLines(
      c(paste("# Document", i), "", paste("Contents of document", i)),
      PATHS[[i]]
    )
  }

  temp_store <- tempfile(fileext = ".store")

  # 1. test that we correctly forward the error
  store <- ragnar_store_create(
    temp_store,
    embed = \(x) matrix(runif(100), nrow = 1),
    overwrite = TRUE
  )

  expect_error(
    ragnar_store_ingest(store, PATHS, progress = FALSE),
    regexp = 'could not find function "runif"'
  )

  # 2. test that we can ingest with a working embedder
  store <- ragnar_store_create(
    temp_store,
    embed = \(x) matrix(stats::runif(100), nrow = 1),
    overwrite = TRUE
  )

  expect_error(
    ragnar_store_ingest(store, PATHS, progress = FALSE),
    regexp = NA
  )

  # basic test we have all paths in the store
  n_docs <- dbGetQuery(store@con, "SELECT COUNT(*) as n FROM documents")
  expect_equal(n_docs$n, length(PATHS))
})
