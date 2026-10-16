test_that("ragnar_store_create() v2 creates a page_metadata table", {
  skip_on_cran()
  store <- ragnar_store_create(embed = NULL)
  tables <- DBI::dbListTables(store@con)
  expect_true("page_metadata" %in% tables)
})

test_that("page_metadata has the expected columns", {
  skip_on_cran()
  store <- ragnar_store_create(embed = NULL)
  cols <- DBI::dbGetQuery(
    store@con,
    "SELECT column_name FROM information_schema.columns
     WHERE table_name = 'page_metadata'"
  )$column_name
  expect_setequal(cols, c("origin", "last_modified", "content_hash", "ingested_at"))
})

test_that("ragnar_store_connect() recognises stores with page_metadata", {
  skip_on_cran()
  tmp <- tempfile(fileext = ".duckdb")
  store <- ragnar_store_create(tmp, embed = NULL)
  DBI::dbDisconnect(store@con, shutdown = TRUE)

  store2 <- ragnar_store_connect(tmp, read_only = FALSE)
  tables <- DBI::dbListTables(store2@con)
  expect_true("page_metadata" %in% tables)
  DBI::dbDisconnect(store2@con, shutdown = TRUE)
})

test_that("connecting to an older v2 store asks users to recreate it", {
  skip_on_cran()
  location <- withr::local_tempfile(fileext = ".duckdb")
  store <- ragnar_store_create(location, embed = NULL)
  DBI::dbExecute(store@con, "DROP TABLE page_metadata")
  DBI::dbDisconnect(store@con, shutdown = TRUE)

  expect_error(
    ragnar_store_connect(location, read_only = FALSE),
    "Recreate it with `ragnar_store_create()`.",
    fixed = TRUE
  )
})
