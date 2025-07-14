# tools/configure_reticulate.R

# Make reticulate setup the ephemeral venv in advance,
# primarily so CRAN examples run quickly and don't trigger a warning

Sys.setenv("RETICULATE_PYTHON" = "managed")

library(reticulate)
py_require(c(
  "markitdown[all]",
  if (identical(.Platform$OS.type, "windows")) {
    py_require("onnxruntime<=1.20.1")
  }
))
try({
  print(py_config())
  import("markitdown")
})
