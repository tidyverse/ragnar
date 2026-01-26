# Embed text using a Databricks model

`embed_databricks()` gets embeddings for text using a model hosted in a
Databricks workspace. It relies on the ellmer package for managing
Databricks credentials. See
[`ellmer::chat_databricks`](https://ellmer.tidyverse.org/reference/chat_databricks.html)
for more on supported modes of authentication.

## Usage

``` r
embed_databricks(
  x,
  workspace = databricks_workspace(),
  model = "databricks-bge-large-en",
  batch_size = 512L
)
```

## Arguments

- x:

  x can be:

  - A character vector, in which case a matrix of embeddings is
    returned.

  - A data frame with a column named `text`, in which case the dataframe
    is returned with an additional column named `embedding`.

  - Missing or `NULL`, in which case a function is returned that can be
    called to get embeddings. This is a convenient way to partial in
    additional arguments like `model`, and is the most convenient way to
    produce a function that can be passed to the `embed` argument of
    [`ragnar_store_create()`](https://ragnar.tidyverse.org/reference/ragnar_store_create.md).

- workspace:

  The URL of a Databricks workspace, e.g.
  `"https://example.cloud.databricks.com"`. Will use the value of the
  environment variable `DATABRICKS_HOST`, if set.

- model:

  The name of a text embedding model.

- batch_size:

  split `x` into batches when embedding. Integer, limit of strings to
  include in a single request.
