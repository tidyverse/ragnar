# Audit a Ragnar store against an expected set of URLs

`ragnar_store_audit()` checks which URLs from an expected set are
present in the store's `documents` table. Use it to verify that all
pages were successfully ingested after a
[`ragnar_store_ingest()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_ingest.md)
run.

## Usage

``` r
ragnar_store_audit(store, expected, group_by = NULL)
```

## Arguments

- store:

  A `DuckDBRagnarStore` (version 2 only).

- expected:

  A character vector or
  [`ragnar_links`](https://ragnar.tidyverse.org/dev/reference/ragnar_find_links.md)
  object of URLs or file paths to check, or a data frame with a
  character column named `source` containing those origins.

- group_by:

  Optional name of a column in `expected` to group by. It must differ
  from the summary column names below. The default, `NULL`, reports
  totals for the full expected set.

## Value

A tibble with one row, or one row per group when `group_by` is set, with
columns:

- The grouping column, when requested, in order of first appearance.

- `expected` (integer): number of distinct expected origins.

- `ingested` (integer): number of expected origins present in the store.

- `missing` (integer): number of expected origins absent from the store.

- `chunks` (integer): total stored chunks for the expected origins.

- `missing_urls` (list): a character vector of missing origins for each
  row, or [`character()`](https://rdrr.io/r/base/character.html) when
  all expected origins are present.

Duplicate origins are counted once per group. Origins stored outside the
expected set are excluded. Empty ungrouped input returns one row of zero
counts; empty grouped input returns zero rows. Read-only connections are
supported.
