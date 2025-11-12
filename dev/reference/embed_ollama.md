# Embed Text

Embed Text

## Usage

``` r
embed_ollama(
  x,
  base_url = "http://localhost:11434",
  model = "embeddinggemma:300m",
  batch_size = 10L
)

embed_openai(
  x,
  model = "text-embedding-3-small",
  base_url = "https://api.openai.com/v1",
  api_key = get_envvar("OPENAI_API_KEY"),
  dims = NULL,
  user = get_user(),
  batch_size = 20L
)

embed_lm_studio(
  x,
  model,
  base_url = "http://localhost:1234/v1",
  api_key = "lm-studio",
  dims = NULL,
  user = get_user(),
  batch_size = 20L
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
    [`ragnar_store_create()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_create.md).

- base_url:

  string, url where the service is available.

- model:

  string; model name

- batch_size:

  split `x` into batches when embedding. Integer, limit of strings to
  include in a single request.

- api_key:

  resolved using env var `OPENAI_API_KEY`

- dims:

  An integer, can be used to truncate the embedding to a specific size.

- user:

  User name passed via the API.

## Value

If `x` is a character vector, then a numeric matrix is returned, where
`nrow = length(x)` and `ncol = <model-embedding-size>`. If `x` is a
data.frame, then a new `embedding` matrix "column" is added, containing
the matrix described in the previous sentence.

A matrix of embeddings with 1 row per input string, or a dataframe with
an 'embedding' column.

## Functions

- `embed_lm_studio()`: Embed Text using LMStudio. Indentical to
  `embed_openai()` but with suitable defaults for LMStudio.

## Examples

``` r
text <- c("a chunk of text", "another chunk of text", "one more chunk of text")
# \dontrun{
text |>
  embed_ollama() |>
  str()
#> Error in req_perform(req): Failed to perform HTTP request.
#> Caused by error in `curl::curl_fetch_memory()`:
#> ! Couldn't connect to server [localhost]:
#> Failed to connect to localhost port 11434 after 0 ms: Couldn't connect to server

text |>
  embed_openai() |>
  str()
#> Error in embed_openai(text): Can't find env var `OPENAI_API_KEY`.
# }
```
