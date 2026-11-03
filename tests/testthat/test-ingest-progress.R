test_that("ingest progress treats document origins literally", {
  skip_on_cran()

  withr::local_envvar(RAGNAR_INGEST_PROBE = NA)
  withr::local_options(cli.width = 1000)
  directory <- withr::local_tempdir()
  store <- ragnar_store_create(
    embed = function(x) matrix(1, nrow = length(x), ncol = 4L)
  )
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))

  filenames <- c(
    "{Sys.setenv(RAGNAR_INGEST_PROBE = 'ran')}.md",
    "The {BC} report.md",
    "{.md",
    "}{.md",
    "{{x}}.md"
  )
  for (filename in filenames) {
    path <- file.path(directory, filename)
    writeLines("Document for ingest progress.", path)

    expect_message(
      result <- ragnar_store_ingest(
        store, path, n_workers = 1L, build_index = FALSE
      ),
      paste("ingested:", path),
      fixed = TRUE
    )

    expect_identical(Sys.getenv("RAGNAR_INGEST_PROBE", unset = NA), NA_character_)
    expect_identical(attr(result, "ingest_summary")$status, "ok")
    expect_identical(attr(result, "ingest_summary")$n_chunks, 1L)
  }
})
