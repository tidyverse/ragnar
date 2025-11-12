# Merge overlapping chunks in retrieved results

Groups and merges overlapping text chunks from the same origin in the
retrieval results.

## Usage

``` r
chunks_deoverlap(store, chunks)
```

## Arguments

- store:

  A `RagnarStore` object. Must have `@version == 2`.

- chunks:

  A [tibble](https://tibble.tidyverse.org/reference/tibble.html) of
  retrieved chunks, such as the output of
  [`ragnar_retrieve()`](https://ragnar.tidyverse.org/dev/reference/ragnar_retrieve.md).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) of
de-overlapped chunks.

## Details

When multiple retrieved chunks from the same origin have overlapping
character ranges, this function combines them into a single
non-overlapping region.
