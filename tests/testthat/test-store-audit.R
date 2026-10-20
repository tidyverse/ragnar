local_md_paths <- function(n = 2) {
  paths <- replicate(n, {
    p <- tempfile(fileext = ".md")
    writeLines(c("# Test", "", "Some content."), p)
    p
  })
  paths
}

local_store <- function() {
  ragnar_store_create(
    embed = \(x) matrix(nrow = length(x), ncol = 4L, stats::runif(4L * length(x)))
  )
}

test_that("ragnar_store_audit() returns present for ingested URLs", {
  skip_on_cran()
  paths <- local_md_paths(2)
  store <- local_store()
  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  result <- ragnar_store_audit(store, paths)

  expect_s3_class(result, "tbl_df")
  expect_setequal(result$origin, paths)
  expect_true(all(result$status == "present"))
  expect_true(all(result$chunks > 0L))
  expect_true(all(!is.na(result$ingested_at)))
})

test_that("ragnar_store_audit() returns missing for URLs not in store", {
  skip_on_cran()
  store <- local_store()

  result <- ragnar_store_audit(store, c("https://example.com/a", "https://example.com/b"))

  expect_equal(nrow(result), 2L)
  expect_true(all(result$status == "missing"))
  expect_true(all(result$chunks == 0L))
  expect_true(all(is.na(result$ingested_at)))
})

test_that("ragnar_store_audit() handles mixed present and missing URLs", {
  skip_on_cran()
  paths <- local_md_paths(2)
  store <- local_store()
  ragnar_store_ingest(store, paths[1], progress = FALSE, build_index = FALSE)

  result <- ragnar_store_audit(store, paths)

  expect_equal(nrow(result), 2L)
  expect_equal(result$status[result$origin == paths[[1]]], "present")
  expect_equal(result$status[result$origin == paths[[2]]], "missing")
})

test_that("ragnar_store_audit() accepts ragnar_links objects", {
  skip_on_cran()
  paths <- local_md_paths(2)
  store <- local_store()
  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  links <- structure(paths, class = c("ragnar_links", "character"))
  result <- ragnar_store_audit(store, links)

  expect_s3_class(result, "tbl_df")
  expect_true(all(result$status == "present"))
})

test_that("ragnar_store_audit() deduplicates expected URLs", {
  skip_on_cran()
  paths <- local_md_paths(1)
  store <- local_store()
  ragnar_store_ingest(store, paths, progress = FALSE, build_index = FALSE)

  result <- ragnar_store_audit(store, c(paths[[1]], paths[[1]]))

  expect_equal(nrow(result), 1L)
})

test_that("ragnar_store_audit() rejects version 1 stores", {
  skip_on_cran()
  store <- ragnar_store_create(
    embed = \(x) matrix(nrow = length(x), ncol = 4L, stats::runif(4L * length(x))),
    version = 1
  )
  expect_error(
    ragnar_store_audit(store, character()),
    "only supports version 2"
  )
})
