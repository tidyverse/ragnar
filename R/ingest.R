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
#'   `MarkdownDocumentChunks` object. It is called with an argument `path` and
#'   should return the prepared chunks (with or without an `embedding` column).
#' @param n_workers Number of worker processes to use. Defaults to the smaller of
#'   `length(paths)` and `parallel::detectCores()` (with a minimum of 1).
#' @param progress Logical; if `TRUE`, show a CLI progress bar.
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

  check_character(paths, allow_na = FALSE)
  length(paths) || return(invisible(store))
  paths <- prepare_ingest_paths(paths, store)

  prepare <- rlang::as_function(prepare)

  if (is.null(n_workers)) {
    n_workers <-
      max(na.rm = TRUE, parallel::detectCores(), 4L) |>
      min(nrow(paths) %/% 2L) |>
      max(2L)
    if (progress) cli::cli_inform("Launching {n_workers} parallel workers.")
  }
  check_number_whole(n_workers, min = 1)

  # set up mirai daemons on a dedicated compute profile
  compute_id <- sprintf("ragnar-ingest-%s", Sys.getpid())
  mirai::daemons(n = n_workers, .compute = compute_id)
  on.exit(mirai::daemons(0L, .compute = compute_id), add = TRUE)
  mirai::local_daemons(compute_id)

  task_queue <- mirai_queue(
    max_uncollected = n_workers + 1L,
    .compute = compute_id
  )

  withr::local_options(list(
    ragnar.embed.req_retry = list(
      max_tries = 30L,
      max_seconds = 90
    )
  ))

  task_queue$everywhere(
    {
      library(ragnar)
      options(ragnar.embed.req_retry = FALSE)
    },
    prepare = prepare,
    store = store,
    do_ingest_remote_work = do_ingest_remote_work,
    do_embed = do_embed
  )

  work_expr <- quote(do_ingest_remote_work(path, store, prepare, embed))
  for (i in seq_len(nrow(paths))) {
    task_queue$push_mirai(
      work_expr,
      path = paths$origin[[i]],
      embed = paths$is_new_origin[[i]]
    )
  }

  if (isTRUE(progress)) {
    cli::cli_progress_bar("Ingesting", total = nrow(paths))
  }

  repeat {
    result <- task_queue$pop_result(unavailable = break)

    if (mirai::is_error_value(result)) {
      cond <- attributes(result)
      class(cond) <- cond$condition.class
      stop(cond)
    }

    if (!S7::S7_inherits(result, MarkdownDocumentChunks)) {
      stop(
        "Unexpected result from `prepare()`. Expected a `MarkdownDocumentChunks` object."
      )
    }
    ragnar_store_update(store, result)
    if (progress) cli::cli_progress_update()
  }

  if (progress) {
    cli::cli_progress_done()
  }

  invisible(store)
}

do_ingest_remote_work <- function(path, store, prepare, embed = TRUE) {
  chunks <- prepare(path)
  if (embed) {
    tryCatch(
      chunks <- do_embed(store, chunks),
      error = function(e) NULL
    )
  }
  chunks
}

do_embed <- function(store, chunks) {
  if (is.null(store@embed) || "embedding" %in% names(chunks)) {
    return(chunks)
  }

  context <- chunks[["context"]] %||% rep("", nrow(chunks))
  context[is.na(context)] <- ""

  text <- chunks[["text"]] %||%
    stri_sub(chunks@document, chunks$start, chunks$end)

  input <- ifelse(nzchar(context), paste0(context, "\n", text), text)

  chunks$embedding <- store@embed(input)
  chunks
}

mirai_queue <- function(max_uncollected = NULL, .compute = NULL) {
  .pending <- .active <- .finished <- list()
  force(.compute)

  everywhere <- function(...) {
    mirai::everywhere(..., .compute = .compute)
  }

  .launch_jobs <- function() {
    while (
      length(.pending) &&
        (length(.finished) + length(.active)) < max_uncollected
    ) {
      mirai_args <- .pending[[1L]]
      .pending <<- .pending[-1L]
      .active[[length(.active) + 1L]] <<-
        inject(mirai::mirai(!!!mirai_args, .compute = .compute))
    }
  }

  pop_result <- function(unavailable = NULL) {
    if (length(.finished)) {
      out <- .finished[[1L]]
      .finished <<- .finished[-1]
      .launch_jobs()
      return(out$data)
    }

    if (length(.active)) {
      mirai::race_mirai(.active)
      done <- !vapply(.active, mirai::unresolved, TRUE)
      .finished <<- c(.finished, .active[done])
      .active <<- .active[!done]
      return(pop_result(unavailable = unavailable))
    }

    if (length(.pending)) {
      .launch_jobs()
      return(pop_result(unavailable = unavailable))
    }

    unavailable
  }

  push_mirai <- function(...) {
    .pending[[length(.pending) + 1L]] <<- list(...)
    .launch_jobs()
  }

  size <- function() {
    sum(length(.pending), length(.active), length(.finished))
  }

  environment()
}


prepare_ingest_paths <- function(paths, store) {
  paths <- unique(as.character(paths))
  paths <- tibble(origin = paths)

  # add 'is_new_origin' col
  local_duckdb_register(store@con, "paths_to_ingest", paths)
  paths <- DBI::dbGetQuery(
    store@con,
    "
    SELECT p.origin, d.origin IS NULL AS is_new_origin
    FROM paths_to_ingest p
    LEFT JOIN documents d USING(origin)
    "
  )

  # uniformly distribute new and old origins
  paths |>
    mutate(
      .by = is_new_origin,
      rank = dplyr::percent_rank(row_number())
    ) |>
    arrange(rank, !is_new_origin) |>
    select(-rank)
}
