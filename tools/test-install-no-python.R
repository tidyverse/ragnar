# Run with: Rscript tools/test-install-no-python.R path/to/ragnar.tar.gz
# Exercise R CMD INSTALL, including configure, in an isolated library. An
# optional Python import must not be able to terminate package installation.
local({
  source_package <- commandArgs(trailingOnly = TRUE)[[1L]]
  root <- tempfile("ragnar-install-test-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  lib <- file.path(root, "library")
  dir.create(lib)
  python_path <- file.path(root, "python")
  dir.create(python_path)
  marker <- file.path(root, "python-imported")
  writeLines(c(
    "import os",
    "from pathlib import Path",
    "Path(os.environ['RAGNAR_TEST_PYTHON_MARKER']).touch()",
    "os._exit(86)"
  ), file.path(python_path, "markitdown.py"))

  # This module models the native-process termination seen on CRAN. Unlike an
  # R/Python exception, os._exit() cannot be caught by try(import(...)).
  Sys.setenv(PYTHONPATH = python_path, RAGNAR_TEST_PYTHON_MARKER = marker)
  r <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
  status <- system2(r, c(
    "CMD", "INSTALL", shQuote(paste0("--library=", lib)),
    shQuote(normalizePath(source_package, mustWork = TRUE))
  ))
  stopifnot(status == 0L, !file.exists(marker))

  script <- file.path(root, "load.R")
  writeLines(c(
    paste0(".libPaths(c(", deparse(lib), ", .libPaths()))"),
    "library(ragnar)",
    "stopifnot(!reticulate::py_available(initialize = FALSE))",
    "chunks <- ragnar::markdown_chunk('# Heading\\n\\nSome text.')",
    "stopifnot(nrow(chunks) == 1L, grepl('Some text.', chunks$text, fixed = TRUE))",
    "stopifnot(!reticulate::py_available(initialize = FALSE))"
  ), script)
  rscript <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  stopifnot(system2(rscript, c("--vanilla", shQuote(script))) == 0L)
  stopifnot(!file.exists(marker))
  cat("Installation, loading, and Markdown chunking did not initialize Python.\n")
})
