test_that("basic ragnar ingest test", {
  skip_if_offline()
  skip_on_cran()

  PATHS <- c(
    "https://quarto.org/about.html",
    "https://quarto.org/bug-reports.html",
    "https://quarto.org/docs/advanced/environment-vars.html",
    "https://quarto.org/docs/advanced/html/external-sources.html",
    "https://quarto.org/docs/advanced/index.html",
    "https://quarto.org/docs/advanced/inspect/index.html",
    "https://quarto.org/docs/advanced/jupyter/kernel-execution.html",
    "https://quarto.org/docs/advanced/typst/brand-yaml.html",
    "https://quarto.org/docs/advanced/typst/typst-css.html",
    "https://quarto.org/docs/authoring/appendices.html"
  )

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
