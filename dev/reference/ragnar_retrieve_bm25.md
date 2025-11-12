# Retrieves chunks using the BM25 score

BM25 refers to Okapi Best Matching 25. See
[doi:10.1561/1500000019](https://doi.org/10.1561/1500000019) for more
information.

## Usage

``` r
ragnar_retrieve_bm25(
  store,
  text,
  top_k = 3L,
  ...,
  k = 1.2,
  b = 0.75,
  conjunctive = FALSE,
  filter
)
```

## Arguments

- store:

  A `RagnarStore` object returned by
  [`ragnar_store_connect()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_create.md)
  or
  [`ragnar_store_create()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_create.md).

- text:

  String, the text to search for.

- top_k:

  Integer. Number of nearest entries to find per method.

- ...:

  Additional arguments passed to the lower-level retrieval functions.

- k, b:

  \\k_1\\ and \\b\\ parameters in the Okapi BM25 retrieval method.

- conjunctive:

  Whether to make the query conjunctive i.e., all terms in the query
  string must be present in order for a chunk to be retrieved.

- filter:

  Optional. A filter expression evaluated with
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).

## Value

A tibble ordered by descending BM25 `metric_value` (higher is more
relevant), with a `metric_name` column set to "bm25".

## See also

Other ragnar_retrieve:
[`ragnar_retrieve()`](https://ragnar.tidyverse.org/dev/reference/ragnar_retrieve.md),
[`ragnar_retrieve_vss()`](https://ragnar.tidyverse.org/dev/reference/ragnar_retrieve_vss.md),
[`ragnar_retrieve_vss_and_bm25()`](https://ragnar.tidyverse.org/dev/reference/ragnar_retrieve_vss_and_bm25.md)
