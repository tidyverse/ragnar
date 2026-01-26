# Inserts or updates chunks in a `RagnarStore`

Inserts or updates chunks in a `RagnarStore`

## Usage

``` r
ragnar_store_insert(store, chunks)

ragnar_store_update(store, chunks)
```

## Arguments

- store:

  a `RagnarStore` object

- chunks:

  Content to insert or update. The precise input structure depends on
  `store@version`. See Details.

## Value

`store`, invisibly.

## Details

**Store Version 2**

`chunks` must be `MarkdownDocumentChunks` object.

**Store Version 1**

`chunks` must be a data frame containing `origin`, `hash`, and `text`
columns. We first filter out chunks for which `origin` and `hash` are
already in the store. If an `origin` is in the store, but with a
different `hash`, we replace all of its chunks with the new chunks.
Otherwise, a regular insert is performed.

This can help avoid needing to compute embeddings for chunks that are
already in the store.
