# Warm dependency caches so examples do not pay the first-use download cost.
# Run native imports in child processes: try() cannot catch a segmentation
# fault, and a cache warm-up failure must not abort package installation.
is_windows <- function() identical(.Platform$OS.type, "windows")
rscript_exe <- function() {
  file.path(
    R.home("bin"),
    if (is_windows()) "Rscript.exe" else "Rscript"
  )
}

load_python_in_subprocess <- function() {
  status <- try(system2(
    rscript_exe(),
    "-",
    input = c(
      "Sys.setenv(RETICULATE_PYTHON = 'managed', ORT_DISABLE_TELEMETRY = '1')",
      # Keep dependency constraints in sync with .onLoad().
      "reticulate::py_require(",
      "  c('markitdown[all]', 'onnxruntime<=1.20.1; sys_platform == \"win32\"'),",
      "  python_version = '<3.14')",
      "print(reticulate::py_config())",
      "reticulate::import('markitdown')"
    )
  ))
  if (!identical(status, 0L)) {
    detail <- if (is.numeric(status)) sprintf(" (exit status %s)", status) else ""
    message("Python cache warm-up failed", detail, "; continuing installation.")
  }
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

if (is_windows() && R.version$arch %in% c("aarch64", "arm64")) {
  message("MarkItDown and DuckDB stores are not currently supported on Windows ARM64.")
} else {
  load_python_in_subprocess()
  load_duckdb_extensions_in_subprocess()
}

NULL
