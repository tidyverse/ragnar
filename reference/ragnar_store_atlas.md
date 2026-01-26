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
#> Error in ragnar_store_connect(":memory:"): Store must be created with ragnar_store_create()
# Launch the Embedding Atlas app
ragnar_store_atlas(store)
#> Error in ragnar_store_atlas(store): error in evaluating the argument 'conn' in selecting a method for function 'dbSendQuery': object 'store' not found
# }

```
