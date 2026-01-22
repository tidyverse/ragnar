# Concurrently ingest documents into a Ragnar store

`ragnar_store_ingest()` distributes document preparation work over
multiple processes using [mirai](https://mirai.r-lib.org). Each worker
calls `prepare` on a single path and returns the resulting chunks (and
any warnings) to the main process, which then writes them to the store.

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

  Number of worker processes to use. Defaults to the smaller of
  `length(paths)` and
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html)
  (with a minimum of 1).

- progress:

  Logical; if `TRUE`, show a CLI progress bar.

- build_index:

  Logical; whether to call
  [`ragnar_store_build_index()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_build_index.md)
  after ingestion.

## Value

`store`, invisibly.
