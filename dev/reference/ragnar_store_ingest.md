# Concurrently ingest documents into a Ragnar store

`ragnar_store_ingest()` distributes document preparation work over
multiple processes using [mirai](https://mirai.r-lib.org). Each worker
calls `prepare` on a single path and returns the resulting chunks (and
any warnings) to the main process, which then writes them to the store.

Worker errors signal a `ragnar_ingest_worker_error` warning with a
`path` field identifying the failed input. Ingestion continues with the
remaining paths.

Unchanged modification times skip document preparation. When a
modification time is unavailable or changes, `prepare()` runs and a hash
of the prepared Markdown avoids embedding and storing unchanged content.

## Usage

``` r
ragnar_store_ingest(
  store,
  paths,
  prepare = function(path) markdown_chunk(read_as_markdown(path)),
  n_workers = NULL,
  progress = TRUE,
  build_index = TRUE
)
```

## Arguments

- store:

  A `RagnarStore`. Currently only version 2 stores are supported.

- paths:

  Character vector of file paths or URLs to ingest.

- prepare:

  Function that converts a single path into a `MarkdownDocumentChunks`
  object. It is called with an argument `path` and should return the
  prepared chunks (with or without an `embedding` column).

- n_workers:

  Number of worker processes to use. The default is
  `max(2L, min(n_cores, n_paths %/% 4L, 8L))`, where `n_paths` is the
  number of unique paths requiring ingestion and `n_cores` is the count
  returned by
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html)
  (or 4 if unavailable).

- progress:

  Logical; if `TRUE`, show a CLI progress bar.

- build_index:

  Logical; whether to call
  [`ragnar_store_build_index()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_build_index.md)
  after ingestion.

## Value

`store`, invisibly, with an `ingest_summary` attribute: a tibble with
one row per input path, in input order, and columns:

- `path` (character): the input file path or URL.

- `status` (character): `"ok"` (processed successfully),
  `"skipped_unchanged"` (origin in the store and confirmed unchanged),
  or `"error"` (worker failed).

- `n_chunks` (integer): the number of chunks stored for the document's
  origin after ingestion, or `0L` for worker failures.

- `message` (character): the worker error message, or `NA` otherwise.

Duplicate input paths share an outcome and are processed once.
