# Build a Ragnar Store index

A search index must be built before calling
[`ragnar_retrieve()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve.md).
If additional entries are added to the store with
[`ragnar_store_insert()`](https://ragnar.tidyverse.org/reference/ragnar_store_insert.md),
`ragnar_store_build_index()` must be called again to rebuild the index.

## Usage

``` r
ragnar_store_build_index(store, type = c("vss", "fts"))
```

## Arguments

- store:

  a `RagnarStore` object

- type:

  The retrieval search type to build an index for.

## Value

`store`, invisibly.
