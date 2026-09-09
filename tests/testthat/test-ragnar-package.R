test_that("ONNX Runtime telemetry is disabled", {
  expect_identical(Sys.getenv("ORT_DISABLE_TELEMETRY"), "1")
})

test_that("ragnar loads without warnings", {
  script <- tempfile(fileext = ".R")
  withr::defer(unlink(script))
  writeLines(c(
    paste0(".libPaths(", paste(capture.output(dput(.libPaths())), collapse = "\n"), ")"),
    "options(warn = 2)",
    "library(ragnar)"
  ), script)

  output <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script)),
    stdout = TRUE,
    stderr = TRUE
  ))

  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
})
