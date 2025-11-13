#' Install the `ragnar` cli application.
#'
#' @inheritDotParams Rapp::install_pkg_cli_apps -package -lib.loc
#' @export
install_ragnar_cli <- function(...) {
  Rapp::install_pkg_cli_apps(package = "ragnar", lib.loc = NULL, ...)
}

# This uses rg. rg by default respects .gitignore,
# but only is really useful for searching/listing
# under the current working directory.
resolve_project_files_to_ingest <- function(globs = NULL) {
  # magika is a dependancy of markitdown
  # if that ever changes, we can just call py_require("magika")
  if (Sys.which("rg") == "") {
    stop("Please install `rg` (https://github.com/BurntSushi/ripgrep)")
  }
  append(globs) <- c("!*.svg", "!*.png", "!*.Rd", "!*.ico", "!LICENSE*")
  rg_args <- "--files"
  if (length(globs)) {
    globs <- sub("~", path.expand("~"), globs, fixed = TRUE)
    rg_args <- c(rg_args, as.vector(rbind("--glob", globs)))
  }

  rg_args <- reticulate:::maybe_shQuote(rg_args)
  paths <- reticulate:::system2t("rg", rg_args, stdout = TRUE, stderr = FALSE)
  paths <- paths[file.exists(paths) & !dir.exists(paths)]

  magika <- reticulate::import("magika")$Magika()
  results <- magika$identify_paths(paths)
  paths <- paths[map_lgl(results, \(r) r$ok && r$output$is_text)]
  # big files first; minimize stragglers in parallel ingestion workers
  paths <- paths[order(file.size(paths), decreasing = TRUE)]
  # paths <- sub(getwd(), ".", paths, fixed = TRUE)
  paths <- sub(path.expand("~"), "~", paths, fixed = TRUE)
  paths
}

resolve_files_to_ingest <- function(globs = NULL) {
  paths <- if (length(globs)) Sys.glob(globs) else "."
  paths <- list.files(
    paths,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE
  )
  # maybe use system("git check-ignore") to filter?
  paths <- paths[!endsWith(paths, ".svg")]
  magika <- reticulate::import("magika")$Magika()
  results <- magika$identify_paths(paths)
  paths <- paths[map_lgl(results, \(r) r$ok && r$output$is_text)]
  paths <- paths[order(file.size(paths), decreasing = TRUE)]
  paths <- sub(path.expand("~"), "~", paths, fixed = TRUE)
  paths
}
