# tools/configure_reticulate.R

# Make reticulate setup the ephemeral venv in advance,
# primarily so CRAN examples run quickly and don't trigger a warning

Sys.setenv(
  RETICULATE_PYTHON = "managed",
  ORT_DISABLE_TELEMETRY = "1"
)

library(reticulate)
py_require(
  "markitdown[all]",
  # Magika 0.6.3 (via MarkItDown) pins onnxruntime<=1.20.1 on Windows.
  # That ONNX Runtime version has no Python 3.14 wheels.
  # https://github.com/posit-dev/raghilda/issues/93
  python_version = "<3.14"
)
try({
  print(py_config())
  import("markitdown")
})


is_windows <- function() identical(.Platform$OS.type, "windows")
rscript_exe <- function() {
  file.path(
    R.home("bin"),
    if (is_windows()) "Rscript.exe" else "Rscript"
  )
}

load_duckdb_extensions_in_subprocess <- function() {
  # download duckdb extensions (which are also cached by duckdb)
  # same motivation as reticulate, avoid NOTE due to first-run download:
  #   'Examples with CPU (user + system) or elapsed time > 5s
  # We do this in a subprocess in case of segfaults with mismatched ABI,
  # see comments in package code.
  try(system2(
    rscript_exe(),
    "-",
    input = c(
      "con <- DBI::dbConnect(duckdb::duckdb())",
      "DBI::dbExecute(con, 'INSTALL fts; INSTALL vss;')",
      "DBI::dbExecute(con, 'LOAD fts; LOAD vss;')"
    )
  ))
}

load_duckdb_extensions_in_subprocess()

NULL
