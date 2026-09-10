test_that("ragnar_store_atlas() serves projected store embeddings", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("nanoarrow", minimum_version = "0.8.0")

  reticulate::py_require(c("embedding-atlas>=0.20.0", "nanoarrow"))
  mock <- reticulate::import("unittest.mock")
  # Exercise the real projection and HTTP app without starting a blocking server.
  server_run_patch <- mock$patch("uvicorn.Server.run", autospec = TRUE)
  server_run <- server_run_patch$start()
  withr::defer(server_run_patch$stop())
  TestClient <- reticulate::import("starlette.testclient")$TestClient

  lapply(c(1L, 20L), function(n) {
    store <- ragnar_store_create(
      version = 1,
      embed = \(x) matrix(sin(seq_len(length(x) * 10)), nrow = length(x))
    )
    withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))
    text <- paste("chunk", seq_len(n))
    ragnar_store_insert(store, data.frame(text))

    expect_invisible(ragnar_store_atlas(store, launch.browser = FALSE))
    app <- server_run$call_args$args[[1]]$config$app
    client <- TestClient(app)
    withr::defer(client$close())

    metadata <- client$get("/data/metadata.json")$json()
    columns <- metadata$props$data
    expect_type(columns, "list")
    response <- client$post("/data/query", json = reticulate::dict(
      type = "json",
      sql = "SELECT * FROM dataset ORDER BY _row_index"
    ))
    expect_equal(response$status_code, 200L)
    data <- response$json()

    expect_length(data, n)
    expect_identical(vapply(data, `[[`, "", columns$text), text)
    expect_identical(vapply(data, `[[`, 0L, columns$id), seq_len(n) - 1L)
    expect_true(all(is.finite(vapply(data, `[[`, 0, columns$projection$x))))
    expect_true(all(is.finite(vapply(data, `[[`, 0, columns$projection$y))))
    expect_true(all(vapply(data, \(row) columns$neighbors %in% names(row), NA)))
  })
})
