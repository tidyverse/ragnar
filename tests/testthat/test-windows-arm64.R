local_windows_arm64 <- function(env = parent.frame()) {
  local_mocked_bindings(is_windows = function() TRUE, .env = env)
  local_mocked_bindings(
    R.version = modifyList(R.version, list(arch = "aarch64")),
    .package = "base", .env = env
  )
}

test_that("Windows ARM64 declines MarkItDown before conversion", {
  local_windows_arm64()
  local_mocked_bindings(ragnartools.markitdown = list(
    convert_to_markdown = function(...) stop("MarkItDown was initialized")
  ))

  expect_error(
    read_as_markdown("document.html"),
    "MarkItDown is not currently supported on Windows ARM64.", fixed = TRUE
  )
  expect_equal(markdown_chunk("# Heading\n\nSome text.")$text,
               "# Heading\n\nSome text.")
})

test_that("Windows ARM64 declines DuckDB before opening a store", {
  local_windows_arm64()
  local_mocked_bindings(
    duckdb = function(...) stop("DuckDB was initialized"), .package = "duckdb"
  )

  message <- "DuckDB stores are not currently supported on Windows ARM64."
  expect_error(ragnar_store_create(version = 1), message, fixed = TRUE)
  expect_error(ragnar_store_create(version = 2), message, fixed = TRUE)
  expect_error(ragnar_store_connect(":memory:"), message, fixed = TRUE)
})

test_that("Windows ARM64 loads without registering MarkItDown requirements", {
  local_windows_arm64()
  local_mocked_bindings(methods_register = function() NULL, .package = "S7")
  local_mocked_bindings(
    py_require = function(...) stop("MarkItDown requirements were registered"),
    import_from_path = function(...) stop("MarkItDown was initialized"),
    .package = "reticulate"
  )

  expect_silent(.onLoad(NULL, NULL))
})
