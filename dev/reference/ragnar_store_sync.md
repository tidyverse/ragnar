# Remove stale documents from a Ragnar store

`ragnar_store_sync()` deletes any documents whose `origin` is not in
`keep`, along with their embeddings. Use `dry_run = TRUE` to preview
what would be deleted without modifying the store.

## Usage

``` r
ragnar_store_sync(store, keep, dry_run = FALSE, rebuild_index = TRUE)
```

## Arguments

- store:

  A `DuckDBRagnarStore` (version 2 only).

- keep:

  Character vector of origin URLs or file paths to retain. All other
  origins currently in the store are deleted.

- dry_run:

  Logical. If `TRUE`, return a summary tibble of what would be deleted
  without making any changes. Default `FALSE`.

- rebuild_index:

  Logical. If `TRUE` (default), rebuild search indexes after deletion
  via
  [`ragnar_store_build_index()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_build_index.md).

## Value

When `dry_run = FALSE`: `store` invisibly, with a `sync_summary`
attribute – a tibble with columns `origin` (character) and `action`
(character: `"kept"` or `"deleted"`). When `dry_run = TRUE`: the summary
tibble directly.
