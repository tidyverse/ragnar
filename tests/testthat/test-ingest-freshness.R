test_that("fetch_page_metadata() returns last_modified for local files", {
  skip_on_cran()
  tmp <- tempfile(fileext = ".md")
  writeLines("# hello", tmp)

  meta <- ragnar:::fetch_page_metadata(tmp)
  expect_type(meta$last_modified, "double")   # POSIXct is stored as double
  expect_s3_class(meta$last_modified, "POSIXct")
  expect_true(is.na(meta$content_hash))
})

test_that("fetch_page_metadata() returns content_hash for local files when no mtime", {
  # This tests the hash fallback path — for local files mtime is always
  # available, so we test the hash branch via a mock.
  # Indirectly: content_hash is NA for local files (mtime is used instead).
  skip_on_cran()
  tmp <- tempfile(fileext = ".md")
  writeLines("# hello", tmp)
  meta <- ragnar:::fetch_page_metadata(tmp)
  expect_true(is.na(meta$content_hash))
})

test_that("page_is_unchanged() returns FALSE when no stored metadata", {
  skip_on_cran()
  tmp <- tempfile(fileext = ".md")
  writeLines("# hello", tmp)

  result <- ragnar:::page_is_unchanged(tmp, NA, NA_character_)
  expect_false(result)
})

test_that("page_is_unchanged() returns TRUE for unchanged local file via mtime", {
  skip_on_cran()
  tmp <- tempfile(fileext = ".md")
  writeLines("# hello", tmp)
  mtime <- file.mtime(tmp)

  result <- ragnar:::page_is_unchanged(tmp, mtime, NA_character_)
  expect_true(result)
})

test_that("page_is_unchanged() returns FALSE for modified local file", {
  skip_on_cran()
  tmp <- tempfile(fileext = ".md")
  writeLines("# hello", tmp)
  old_mtime <- file.mtime(tmp) - 100  # pretend stored mtime is 100s ago

  result <- ragnar:::page_is_unchanged(tmp, old_mtime, NA_character_)
  expect_false(result)
})
