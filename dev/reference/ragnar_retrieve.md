# Retrieve chunks from a `RagnarStore`

Combines both `vss` and `bm25` search and returns the union of chunks
retrieved by both methods.

## Usage

``` r
ragnar_retrieve(store, text, top_k = 3L, ..., deoverlap = TRUE)
```

## Arguments

- store:

  A `RagnarStore` object returned by
  [`ragnar_store_connect()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_create.md)
  or
  [`ragnar_store_create()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_create.md).

- text:

  Character. Query string to match.

- top_k:

  Integer. Number of nearest entries to find per method.

- ...:

  Additional arguments passed to the lower-level retrieval functions.

- deoverlap:

  Logical. If `TRUE` (default) and `store@version == 2`, overlapping
  chunks are merged with
  [`chunks_deoverlap()`](https://ragnar.tidyverse.org/dev/reference/chunks_deoverlap.md).

## Value

A `tibble` of retrieved chunks. Each row represents a chunk and always
contains a `text` column.

## Note

The results are not re-ranked after identifying the unique values.

## See also

Other ragnar_retrieve:
[`ragnar_retrieve_bm25()`](https://ragnar.tidyverse.org/dev/reference/ragnar_retrieve_bm25.md),
[`ragnar_retrieve_vss()`](https://ragnar.tidyverse.org/dev/reference/ragnar_retrieve_vss.md),
[`ragnar_retrieve_vss_and_bm25()`](https://ragnar.tidyverse.org/dev/reference/ragnar_retrieve_vss_and_bm25.md)

## Examples

``` r
if (FALSE) { # (rlang::is_installed("dbplyr") && nzchar(Sys.getenv("OPENAI_API_KEY")) && ragnar:::can_load_duckdb_extensions())
## Build a small store with categories
store <- ragnar_store_create(
  embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small"),
  extra_cols = data.frame(category = character()),
  version = 1 # store text chunks directly
)

ragnar_store_insert(
  store,
  data.frame(
    category = c(rep("pets", 3), rep("dessert", 3)),
    text     = c("playful puppy", "sleepy kitten", "curious hamster",
                 "chocolate cake", "strawberry tart", "vanilla ice cream")
  )
)
ragnar_store_build_index(store)

# Top 3 chunks without filtering
ragnar_retrieve(store, "sweet")

# Combine filter with similarity search
ragnar_retrieve(store, "sweet", filter = category == "dessert")
}
```
