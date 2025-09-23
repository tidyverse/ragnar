#' Concurrently ingest documents into a Ragnar store
#'
#' @description
#' `ragnar_store_ingest()` distributes document preparation work over multiple
#' processes using [mirai](https://mirai.r-lib.org). Each worker calls `prepare`
#' on a single path and returns the resulting chunks (and any warnings) to the
#' main process, which then writes them to the store.
#'
#' @param store A `RagnarStore`. Currently only version 2 stores are supported.
#' @param paths Character vector of file paths or URLs to ingest.
#' @param prepare Function that converts a single path into a
#'   `MarkdownDocumentChunks` object (optionally with an `embedding` column). It
#'   must accept, at minimum, arguments `path` and `embed`. By default it calls
#'   [read_as_markdown()], [markdown_chunk()], and, when available, the store's
#'   embedding function.
#' @param n_workers Number of worker processes to use. Defaults to the smaller of
#'   `length(paths)` and `parallel::detectCores()` (with a minimum of 1).
#' @param progress Logical; if `TRUE`, show a CLI progress bar.
#' @param prepare_args Named list of additional arguments forwarded to `prepare`.
#' @returns `store`, invisibly.
#' @export
ragnar_store_ingest <- function(
  store,
  paths,
  prepare = \(path) path |> read_as_markdown() |> markdown_chunk(),
  n_workers = NULL,
  progress = TRUE
) {
  if (!rlang::is_installed("mirai")) {
    cli::cli_abort(
      "Package {.pkg mirai} is required for {.fn ragnar_store_ingest}()."
    )
  }

  stopifnot(S7::S7_inherits(store, DuckDBRagnarStore))
  if (!identical(store@version, 2L)) {
    cli::cli_abort(
      "Only stores created with {.code version = 2} are supported."
    )
  }

  paths <- as.character(paths)
  if (!length(paths)) {
    return(invisible(store))
  }

  if (is.null(n_workers)) {
    n_workers <- min(length(paths), parallel::detectCores() %||% 4L)
  }
  check_number_whole(n_workers, min = 1)

  prepare <- rlang::as_function(prepare)

  # set up mirai daemons on a dedicated compute profile
  compute_id <- sprintf(
    "ragnar-ingest-%s-%06d",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    sample.int(1e6, 1L)
  )
  mirai::daemons(n = n_workers, .compute = compute_id)
  on.exit(mirai::daemons(0L, .compute = compute_id), add = TRUE)
  # browser()

  mirai::everywhere(
    {
      devtools::load_all()
      library(ragnar)
      read_as_markdown("README.md")
    },
    prepare = prepare,
    store = store,
    do_ingest_remote_work = do_ingest_remote_work,
    .compute = compute_id
  )

  pending <- rev(as.list(paths))
  active <- list()
  total <- length(paths)
  completed <- 0L

  pb <- NULL

  if (isTRUE(progress)) {
    pb <- cli::cli_progress_bar("Ingesting", total = total)
  }

  launch_one <- function() {
    path <- pending[[lp <- length(pending)]]
    length(pending) <<- lp - 1L
    active[[length(active) + 1L]] <<- mirai::mirai(
      do_ingest_remote_work(path, store, prepare), # is_update
      path = path,
      .compute = compute_id
    )
  }

  harvest <- function(job) {
    # need to check for errors here
    ragnar_store_update(store, job$data)
  }

  repeat {
    while (length(pending) && length(active) < n_workers) {
      launch_one()
    }

    if (!length(active)) {
      break
    }

    done <- !vapply(active, mirai::unresolved, TRUE)

    if (!any(done)) {
      Sys.sleep(0.01)
      next
    }

    for (task in active[done]) {
      # print(task)
      # print(task$data)
      # browser()
      if (mirai::is_error_value(task)) {
        cli::cli_warn("bad result")
        browser()
      }

      chunks <- task$data
      if (!S7_inherits(chunks, MarkdownDocumentChunks)) {
        browser()
      }
      ragnar_store_update(store, task$data)
      # browser()
      cli::cli_progress_update()
    }

    active <- active[!done]

    if (length(active) == 0L && length(pending) == 0L) {
      break
    }
  }

  if (!is.null(pb)) {
    cli::cli_progress_done()
  }

  invisible(store)
}


default_prepare <- function(path, ...) {
  chunks <- path |> read_as_markdown() |> markdown_chunk()

  if (!is.null(store@embed)) {
    if (!"text" %in% names(chunks)) {
      chunks$text <- stringi::stri_sub(
        chunks@document,
        chunks$start,
        chunks$end
      )
    }
  }

  chunks
}

do_ingest_remote_work <- function(path, store, prepare, is_update = FALSE) {
  tryCatch(
    {
      chunks <- prepare(path)
      if (!is_update) {
        chunks <- do_embed(store, chunks)
      }
      chunks
    },
    error = function(e) {
      sink("worker.log", append = TRUE)
      print(e)
      print(getwd())
      read_as_markdown("README.md")
      print(ee <- reticulate::py_last_error())
      print(ee)
      sink()
      stop(ee)
    }
  )
}

do_embed <- function(store, chunks) {
  context <- chunks[["context"]] %||% rep("", nrow(chunks))
  context[is.na(context)] <- ""
  text <- chunks[["text"]] %||%
    stringi::stri_sub(
      chunks@document,
      chunks$start,
      chunks$end
    )

  input <- ifelse(nzchar(context), paste0(context, "\n", text), text)
  chunks$embedding <- store@embed(input)
  chunks
}


if (FALSE) {
  system.time({
    devtools::load_all()
    PATHS <- ragnar_find_links("https://quarto.org/sitemap.xml")
    store <- ragnar_store_create(
      "quarto-fast.ragnar.store",
      overwrite = TRUE
      # embed = \(x) embed_openai(x)
    )
    system.time({
      ragnar_store_ingest(store, PATHS, n_workers = 32)
    })
  })

  ragnar_store_ingest(store, PATHS[1:4])
}


if (FALSE) {
  PATHS <- c(
    "https://quarto.org/about.html",
    "https://quarto.org/bug-reports.html",
    "https://quarto.org/docs/advanced/environment-vars.html",
    "https://quarto.org/docs/advanced/html/external-sources.html",
    "https://quarto.org/docs/advanced/index.html",
    "https://quarto.org/docs/advanced/inspect/index.html",
    "https://quarto.org/docs/advanced/jupyter/kernel-execution.html",
    "https://quarto.org/docs/advanced/typst/brand-yaml.html",
    "https://quarto.org/docs/advanced/typst/typst-css.html",
    "https://quarto.org/docs/authoring/appendices.html"
  )
}


if (FALSE) {
  library(mirai)
  daemons(10)
  lapply(1:5, \(i) {
    mirai({
      ragnar::read_as_markdown("README.Rmd")
    })
  }) -> res

  lapply(res, \(r) r$data)
  task <-
    task$data
}
