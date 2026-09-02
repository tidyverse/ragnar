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

This function requires the `embedding-atlas` Python package. Make sure
you have it installed in your reticulate Python environment. It also
uses `arrow` to transfer data from the DuckDB store to Python.

## Examples

``` r
# \dontrun{
# Connect or create a store
store <- ragnar_store_connect(':memory:')
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/Rtmp4ELcSY/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> Error in ragnar_store_connect(":memory:"): Store must be created with ragnar_store_create()
# Launch the Embedding Atlas app
ragnar_store_atlas(store)
#> Error in py_module_import(module, convert = convert): ImportError: cannot import name 'compute_vector_projection' from 'embedding_atlas.projection' (/home/runner/.cache/R/reticulate/uv/cache/archive-v0/zEX2pxtUOI7De_To/lib/python3.12/site-packages/embedding_atlas/projection.py)
#> Run `reticulate::py_last_error()` for details.
# }

```
