#' Concurrently ingest documents into a Ragnar store
#'
#' @description
#' `ragnar_store_ingest()` distributes document preparation work over multiple
#' processes using [mirai](https://mirai.r-lib.org). Each worker calls `prepare`
#' on a single path and returns the resulting chunks (and any warnings) to the
#' main process, which then writes them to the store.
#'
#' Worker errors signal a `ragnar_ingest_worker_error` warning with a `path`
#' field identifying the failed input. Ingestion continues with the remaining
#' paths.
#'
#' Unchanged modification times skip document preparation. When a modification
#' time is unavailable or changes, `prepare()` runs and a hash of the prepared
#' Markdown avoids embedding and storing unchanged content.
#'
#' @param store A `RagnarStore`. Currently only version 2 stores are supported.
#' @param paths Character vector of file paths or URLs to ingest.
#' @param prepare Function that converts a single path into a
#'   `MarkdownDocumentChunks` object. It is called with an argument `path` and
#'   should return the prepared chunks (with or without an `embedding` column).
#' @param n_workers Number of worker processes to use. The default is
#'   `max(2L, min(n_cores, n_paths %/% 4L, 8L))`, where `n_paths` is the number of
#'   unique paths requiring ingestion and `n_cores` is the count returned
#'   by [parallel::detectCores()] (or 4 if unavailable).
#' @param progress Logical; if `TRUE`, show a CLI progress bar.
#' @param build_index Logical; whether to call `ragnar_store_build_index()` after ingestion.
#' @returns `store`, invisibly, with an `ingest_summary` attribute: a tibble
#'   with one row per input path, in input order, and columns:
#'   * `path` (character): the input file path or URL.
#'   * `status` (character): `"ok"` (processed successfully), `"skipped_unchanged"`
#'     (origin in the store and confirmed unchanged), or `"error"` (worker failed).
#'   * `n_chunks` (integer): the number of chunks stored for the document's
#'     origin after ingestion, or `0L` for worker failures.
#'   * `message` (character): the worker error message, or `NA` otherwise.
#'
#'   Duplicate input paths share an outcome and are processed once.
#' @export
ragnar_store_ingest <- function(
  store,
  paths,
  prepare = \(path) path |> read_as_markdown() |> markdown_chunk(),
  n_workers = NULL,
  progress = TRUE,
  build_index = TRUE
) {
  if (!rlang::is_installed("mirai")) {
    cli::cli_abort(
      "Package {.pkg mirai} is required for {.fn ragnar_store_ingest}()."
    )
  }

  stopifnot(S7::S7_inherits(store, DuckDBRagnarStore))

  check_character(paths, allow_na = FALSE)

  summary <- tibble(
    path = as.character(paths),
    status = NA_character_,
    n_chunks = 0L,
    message = NA_character_
  )

  if (!length(paths)) {
    attr(store, "ingest_summary") <- summary
    return(invisible(store))
  }

  origins <- summary$path
  finish_ingest <- function() {
    completed <- summary$status != "error"
    local_duckdb_register(
      store@con,
      "ingest_summary_origins",
      tibble(origin = unique(origins[completed]))
    )
    counts <- DBI::dbGetQuery(
      store@con,
      "
      SELECT p.origin, COUNT(e.chunk_id)::INTEGER AS n_chunks
      FROM ingest_summary_origins p
      LEFT JOIN documents d USING(origin)
      LEFT JOIN embeddings e USING(doc_id)
      GROUP BY p.origin
      "
    )
    summary$n_chunks[completed] <- counts$n_chunks[
      match(origins[completed], counts$origin)
    ]
    if (build_index) {
      ragnar_store_build_index(store)
    }
    attr(store, "ingest_summary") <- summary
    invisible(store)
  }

  paths <- prepare_ingest_paths(paths, store)

  prepare <- rlang::as_function(prepare)

  path_idx <- match(summary$path, paths$origin)
  skipped <- !paths$is_new_origin & paths$is_unchanged
  summary$status[skipped[path_idx]] <- "skipped_unchanged"
  new_paths <- paths[!skipped, , drop = FALSE]

  if (!nrow(new_paths)) {
    return(finish_ingest())
  }

  if (is.null(n_workers)) {
    n_cores <- parallel::detectCores()
    if (is.na(n_cores)) {
      n_cores <- 4L
    }
    n_workers <- max(2L, min(n_cores, nrow(new_paths) %/% 4L, 8L))
    if (progress) {
      cli::cli_inform("Launching {n_workers} parallel workers.")
    }
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
      max_tries = max(n_workers * 3L, 20L),
      max_seconds = 180
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
    fetch_last_modified = fetch_last_modified,
    do_embed = do_embed
  )

  work_expr <- quote(
    do_ingest_remote_work(path, store, prepare, stored_content_hash)
  )
  for (i in seq_len(nrow(new_paths))) {
    task_queue$push_mirai(
      work_expr,
      path = new_paths$origin[[i]],
      stored_content_hash = new_paths$stored_content_hash[[i]]
    )
  }

  if (isTRUE(progress)) {
    cli::cli_progress_bar(
      "Ingesting",
      total = nrow(new_paths),
      format_done = "{.alert-success Finished ingesting {nrow(new_paths)} files in {cli::pb_elapsed}}",
      format_failed = "{.alert-danger {cli::pb_name} failed after {cli::pb_elapsed}}"
    )
  }

  repeat {
    task <- task_queue$pop_result(unavailable = break)
    result <- task$data

    if (mirai::is_error_value(result)) {
      message <- if (mirai::is_mirai_error(result)) {
        conditionMessage(result)
      } else if (mirai::is_mirai_interrupt(result)) {
        # mirai does not retain the interrupt condition's message.
        "Worker interrupted."
      } else {
        nanonext::nng_error(result)
      }
      rlang::warn(
        message = message,
        class = "ragnar_ingest_worker_error",
        path = task$path,
        call = NULL
      )
      rows <- summary$path == task$path
      summary$status[rows] <- "error"
      summary$n_chunks[rows] <- 0L
      summary$message[rows] <- message
      next
    }

    chunks <- result$chunks
    if (!result$is_unchanged) {
      ragnar_store_update(store, chunks)
    }

    origin <- chunks@document@origin
    # An unchanged task must not replace newer metadata from another input
    # that prepared the same document origin.
    DBI::dbExecute(
      store@con,
      "
      UPDATE documents SET
        last_modified = ?::TIMESTAMPTZ,
        content_hash = ?,
        ingested_at = now()
      WHERE origin = ? AND (NOT ? OR content_hash = ?)
      ",
      params = list(
        result$last_modified, result$content_hash, origin,
        result$is_unchanged, result$content_hash
      )
    )

    rows <- summary$path == task$path
    summary$status[rows] <- if (result$is_unchanged) "skipped_unchanged" else "ok"
    origins[rows] <- origin

    if (progress) {
      cli::cli_alert_info(paste("ingested:", origin))
      cli::cli_progress_update()
    }
  }

  if (progress) {
    cli::cli_progress_done()
  }

  finish_ingest()
}

do_ingest_remote_work <- function(path, store, prepare, stored_content_hash) {
  tryCatch(
    {
      last_modified <- fetch_last_modified(path)
      chunks <- prepare(path)
      if (!S7::S7_inherits(chunks, MarkdownDocumentChunks)) {
        stop("Expected a MarkdownDocumentChunks object.", call. = FALSE)
      }

      if (
        !identical(chunks@document@origin, path) || is.na(last_modified) ||
          !isTRUE(last_modified == fetch_last_modified(path))
      ) {
        last_modified <- .POSIXct(NA_real_)
      }
      content_hash <- rlang::hash(as.character(chunks@document))
      is_unchanged <- identical(content_hash, stored_content_hash)

      if (!is_unchanged) {
        tryCatch(
          chunks <- do_embed(store, chunks),
          error = function(e) NULL
        )
      }
      list(
        chunks = chunks,
        last_modified = last_modified,
        content_hash = content_hash,
        is_unchanged = is_unchanged
      )
    },
    error = function(e) {
      fmt <- '`prepare("%s")` signaled an Error: %s'
      e$message <- sprintf(fmt, path, conditionMessage(e))
      stop(e)
    }
  )
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
      .active[[length(.active) + 1L]] <<- list(
        mirai = inject(mirai::mirai(!!!mirai_args, .compute = .compute)),
        path = mirai_args$path
      )
    }
  }

  pop_result <- function(unavailable = NULL) {
    if (length(.finished)) {
      out <- .finished[[1L]]
      .finished <<- .finished[-1]
      .launch_jobs()
      return(list(data = out$mirai$data, path = out$path))
    }

    if (length(.active)) {
      idx <- mirai::race_mirai(lapply(.active, `[[`, "mirai"))
      .finished <<- c(.finished, .active[idx])
      .active <<- .active[-idx]
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
  con <- store@con

  # Only fetch modification times for origins with stored freshness metadata.
  local_duckdb_register(con, "paths_to_ingest", paths)
  modification_times <- DBI::dbGetQuery(
    con,
    "
    SELECT d.origin
    FROM paths_to_ingest p
    JOIN documents d USING(origin)
    WHERE d.last_modified IS NOT NULL
    "
  )
  modification_times$last_modified <- .POSIXct(vapply(
    modification_times$origin, fetch_last_modified, numeric(1)
  ))
  local_duckdb_register(con, "ingest_modification_times", modification_times)

  # Compare all timestamps using the same DuckDB conversion as storage.
  paths <- DBI::dbGetQuery(
    con,
    "
    SELECT
      p.origin,
      d.origin IS NULL AS is_new_origin,
      d.content_hash AS stored_content_hash,
      COALESCE(m.last_modified::TIMESTAMPTZ = d.last_modified, FALSE) AS is_unchanged
    FROM paths_to_ingest p
    LEFT JOIN documents d USING(origin)
    LEFT JOIN ingest_modification_times m USING(origin)
    "
  )

  # uniformly distribute new and old origins
  paths |>
    mutate(
      .by = "is_new_origin",
      rank = dplyr::percent_rank(row_number())
    ) |>
    arrange(rank, !.data$is_new_origin) |>
    select(-rank)
}

fetch_last_modified <- function(origin) {
  if (grepl("^https?://", origin)) {
    resp <- tryCatch(
      httr2::request(origin) |>
        httr2::req_method("HEAD") |>
        httr2::req_perform(),
      error = function(e) NULL
    )
    if (!is.null(resp)) {
      return(curl::parse_date(httr2::resp_header(resp, "Last-Modified") %||% ""))
    }
    return(.POSIXct(NA_real_))
  }

  file.mtime(origin)
}
