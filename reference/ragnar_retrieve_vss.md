# Vector Similarity Search Retrieval

Computes a similarity measure between the query and the document
embeddings and uses this similarity to rank and retrieve document
chunks.

## Usage

``` r
ragnar_retrieve_vss(
  store,
  query,
  top_k = 3L,
  ...,
  method = "cosine_distance",
  query_vector = store@embed(query),
  filter
)
```

## Arguments

- store:

  A `RagnarStore` object returned by
  [`ragnar_store_connect()`](https://ragnar.tidyverse.org/reference/ragnar_store_create.md)
  or
  [`ragnar_store_create()`](https://ragnar.tidyverse.org/reference/ragnar_store_create.md).

- query:

  Character. The query string to embed and use for similarity search.

- top_k:

  Integer. Maximum number of document chunks to retrieve. Defaults to 3.

- ...:

  Additional arguments passed to methods.

- method:

  Character. Similarity method to use: `"cosine_distance"`,
  `"euclidean_distance"`, or `"negative_inner_product"`. Defaults to
  `"cosine_distance"`.

- query_vector:

  Numeric vector. The embedding for `query`. Defaults to
  `store@embed(query)`.

- filter:

  Optional. A filter expression evaluated with
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).

## Value

A `tibble` with the top_k retrieved chunks, ordered by `metric_value`.

## Details

Supported methods:

- **cosine_distance** – cosine of the angle between two vectors.

- **euclidean_distance** – L2 distance between vectors.

- **negative_inner_product** – negative sum of element-wise products.

If `filter` is supplied, the function first performs the similarity
search, then applies the filter in an outer SQL query. It uses the HNSW
index when possible and falls back to a sequential scan for large result
sets or filtered queries.

## Note

The results are not re-ranked after identifying the unique values.

## See also

Other ragnar_retrieve:
[`ragnar_retrieve()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve.md),
[`ragnar_retrieve_bm25()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve_bm25.md),
[`ragnar_retrieve_vss_and_bm25()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve_vss_and_bm25.md)

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
