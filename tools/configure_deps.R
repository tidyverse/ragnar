# Python requirements are declared in .onLoad() and initialized on first use.
# Do not import MarkItDown here: native crashes cannot be caught by try(), and
# must not prevent installation or use of the functions that need only R.
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
