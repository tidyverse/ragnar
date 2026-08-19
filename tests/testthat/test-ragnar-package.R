test_that("ONNX Runtime telemetry is disabled", {
  expect_identical(Sys.getenv("ORT_DISABLE_TELEMETRY"), "1")
})
