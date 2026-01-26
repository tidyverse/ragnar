# Uses Azure AI Foundry to create embeddings

Uses Azure AI Foundry to create embeddings

## Usage

``` r
embed_azure_openai(
  x,
  endpoint = get_envvar("AZURE_OPENAI_ENDPOINT"),
  api_key = get_envvar("AZURE_OPENAI_API_KEY"),
  api_version = "2023-05-15",
  model,
  batch_size = 20L,
  api_args = list()
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

- endpoint:

  The Azure AI Foundry endpoint URL. A URI in the form of
  `https://<project>.cognitiveservices.azure.com/`. Defaults to the
  value of the `AZURE_OPENAI_ENDPOINT` environment variable. This URL is
  appended with `/openai/deployments/{model}/embeddings`. Where `model`
  is the deployment name of the model.

- api_key:

  resolved using env var `OPENAI_API_KEY`

- api_version:

  The API version to use. Defaults to `2023-05-15`.

- model:

  The deployment name of the model to use for generating embeddings.

- batch_size:

  split `x` into batches when embedding. Integer, limit of strings to
  include in a single request.

- api_args:

  A list of additional arguments to pass to the API request body.

## Value

If `x` is a character vector, then a numeric matrix is returned, where
`nrow = length(x)` and `ncol = <model-embedding-size>`. If `x` is a
data.frame, then a new `embedding` matrix "column" is added, containing
the matrix described in the previous sentence.

A matrix of embeddings with 1 row per input string, or a dataframe with
an 'embedding' column.
