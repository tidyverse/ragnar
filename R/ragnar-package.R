#' @keywords internal
"_PACKAGE"


.globals <- new.env(parent = emptyenv())

ragnartools.markitdown <- NULL

.onLoad <- function(libname, pkgname) {
  Sys.setenv(RETICULATE_PYTHON = "managed")
  S7::methods_register()
  reticulate::py_require(c(
    # Pin onnxruntime until this is resolved:
    #  https://github.com/microsoft/markitdown/issues/1266
    # New VC++ version requirement begins:
    # https://github.com/Microsoft/onnxruntime/releases/tag/v1.21.0
    if (is_windows()) "onnxruntime<=1.20.1",
    "markitdown[all]"
  ))

  uv_set_override_latest_youtube_transcript_api()

  ragnartools.markitdown <<- reticulate::import_from_path(
    "_ragnartools.markitdown",
    system.file("python", package = "ragnar"),
    delay_load = TRUE
  )
}


uv_set_override_latest_youtube_transcript_api <- function() {
  # markitdown[all] pins youtube_transcript_api to an older version, which is broken
  set_envvar(
    "UV_OVERRIDE",
    pkg_file("python/unpin-youtube-transcript-api.txt"),
    action = "append",
    sep = " ",
    unique = TRUE
  )
}


set_envvar <- function(
  name,
  value,
  action = c("replace", "append", "prepend"),
  sep = .Platform$path.sep,
  unique = FALSE
) {
  old <- Sys.getenv(name, NA)

  if (is.null(value) || is.na(value)) {
    Sys.unsetenv(name)
    return(invisible(old))
  }

  value <- paste0(value, collapse = sep)
  if (!is.na(old)) {
    value <- switch(
      match.arg(action),
      replace = value,
      append = paste(old, value, sep = sep),
      prepend = paste(value, old, sep = sep)
    )
    if (unique) {
      value <- unique(unlist(strsplit(value, sep, fixed = TRUE)))
      value <- value[nzchar(value)]
      value <- paste0(value, collapse = sep)
    }
  }

  value <- list(value)
  names(value) <- name
  do.call(Sys.setenv, value)
  invisible(old)
}

pkg_file <- function(..., package = parent.pkg()) {
  path <- system.file(..., package = package, mustWork = FALSE)
  if (!file.exists(path)) {
    warning("package file does not exist: ", path)
    return("")
  }
  if (is_windows()) {
    path <- utils::shortPathName(path)
  }
  if (grepl("\\s", path, perl = TRUE)) {
    name <- basename(path)
    new_path <- tempfile(
      pattern = tools::file_path_sans_ext(name),
      fileext = tools::file_ext(name)
    ) |>
      normalizePath(mustWork = FALSE)
    if (is_windows()) {
      path <- utils::shortPathName(path)
    }
    file.copy(path, new_path)
    path <- new_path
  }
  path
}
