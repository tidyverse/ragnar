# Visualize a store using Embedding Atlas

Visualize a store using Embedding Atlas

## Usage

``` r
ragnar_store_atlas(
  store,
  ...,
  host = "localhost",
  port = 3030,
  launch.browser = interactive()
)
```

## Arguments

- store:

  A `RagnarStore` object to inspect.

- ...:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

- host:

  Host to run the Embedding Atlas server on.

- port:

  Port to run the Embedding Atlas server on.

- launch.browser:

  Whether to launch the browser automatically.

## Note

This function requires the `embedding-atlas` Python package (\>= 0.20.0)
in your reticulate Python environment, the `duckdb` R package (\>=
1.4.0), and `nanoarrow` (\>= 0.8.0) to transfer data from the DuckDB
store to Python.

## Examples

``` r
# \dontrun{
# Start Ollama, then run this in a terminal:
# ollama pull embeddinggemma:300m-qat-q4_0

# Create an in-memory store using local embeddings
store <- ragnar_store_create(
  embed = embed_ollama(model = "embeddinggemma:300m-qat-q4_0")
)
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpSnRJUI/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> Error in req_perform(req): Failed to perform HTTP request.
#> Caused by error in `curl::curl_fetch_memory()`:
#> ! Couldn't connect to server [localhost]:
#> Failed to connect to localhost port 11434 after 0 ms: Couldn't connect to server

# Read and embed a chapter from R for Data Science
chunks <- "https://r4ds.hadley.nz/data-transform.html" |>
  read_as_markdown() |>
  markdown_chunk()
ragnar_store_insert(store, chunks)
#> Error: object 'store' not found

# Launch the Embedding Atlas app
# Interrupt R (Esc or Ctrl+C) to stop the server
ragnar_store_atlas(store)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'dbGetQueryArrow': object 'store' not found
# }

```
