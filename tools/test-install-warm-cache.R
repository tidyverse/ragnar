# Run with: Rscript tools/test-install-warm-cache.R path/to/ragnar.tar.gz
# Exercise R CMD INSTALL, including configure, in an isolated library. Python
# cache warm-up must be attempted, but cannot terminate package installation.
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
  log <- file.path(root, "install.log")
  status <- system2(r, c(
    "CMD", "INSTALL", shQuote(paste0("--library=", lib)),
    shQuote(normalizePath(source_package, mustWork = TRUE))
  ), stdout = log, stderr = log)
  output <- readLines(log, warn = FALSE)
  cat(output, sep = "\n")
  stopifnot(
    status == 0L,
    file.exists(marker),
    any(grepl("Python cache warm-up failed (exit status 86)", output, fixed = TRUE))
  )
  unlink(marker)

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
  cat("Python warm-up was attempted; its failure did not prevent installation.\n")
  cat("Loading and Markdown chunking did not initialize Python.\n")

  # A successful install must also leave real document conversion working.
  Sys.unsetenv(c("PYTHONPATH", "RAGNAR_TEST_PYTHON_MARKER"))
  html <- file.path(root, "document.html")
  writeLines("<html><body><h1>Heading</h1><p>Some text.</p></body></html>", html)
  writeLines(c(
    paste0(".libPaths(c(", deparse(lib), ", .libPaths()))"),
    paste0("doc <- ragnar::read_as_markdown(", deparse(html), ")"),
    "stopifnot(as.character(doc) == '# Heading\\n\\nSome text.')"
  ), script)
  stopifnot(system2(rscript, c("--vanilla", shQuote(script))) == 0L)
  cat("Public HTML conversion succeeded in a fresh process.\n")
})
