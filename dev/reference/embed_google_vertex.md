# Embed using Google Vertex API platform

Embed using Google Vertex API platform

## Usage

``` r
embed_google_gemini(
  x,
  model = "gemini-embedding-001",
  base_url = "https://generativelanguage.googleapis.com/v1beta",
  api_key = get_envvar("GEMINI_API_KEY"),
  dims = NULL,
  task_type = "RETRIEVAL_QUERY",
  batch_size = 20L
)

embed_google_vertex(
  x,
  model,
  location,
  project_id,
  task_type = "RETRIEVAL_QUERY"
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

- model:

  Character specifying the embedding model. See supported models in
  [Text embeddings
  API](https://cloud.google.com/vertex-ai/generative-ai/docs/model-reference/text-embeddings-api)

- base_url:

  string, url where the service is available.

- api_key:

  resolved using env var `GEMINI_API_KEY`

- dims:

  An integer, can be used to truncate the embedding to a specific size.

- task_type:

  Used to convey intended downstream application to help the model
  produce better embeddings. If left blank, the default used is
  `"RETRIEVAL_QUERY"`.

  - `"RETRIEVAL_QUERY"`

  - `"RETRIEVAL_DOCUMENT"`

  - `"SEMANTIC_SIMILARITY"`

  - `"CLASSIFICATION"`

  - `"CLUSTERING"`

  - `"QUESTION_ANSWERING"`

  - `"FACT_VERIFICATION"`

  - `"CODE_RETRIEVAL_QUERY"` For more information about task types, see
    [Choose an embeddings task
    type](https://cloud.google.com/vertex-ai/generative-ai/docs/embeddings/task-types).

- batch_size:

  split `x` into batches when embedding. Integer, limit of strings to
  include in a single request.

- location:

  Location, e.g. `us-east1`, `me-central1`, `africa-south1` or `global`.

- project_id:

  Project ID.

## Functions

- `embed_google_gemini()`: Use the Gemini API to create embeddings.

## Examples

``` r
if (FALSE) { # Sys.getenv("GEMINI_API_KEY") != ""
embed_google_gemini("hello world")
}
# \dontrun{
embed_google_vertex(
 "hello world",
 model="gemini-embedding-001",
 project = "<your-project-id>",
 location = "us-central1"
)
#> Error in embed_google_vertex("hello world", model = "gemini-embedding-001",     project = "<your-project-id>", location = "us-central1"): No Google credentials are available.
#> ℹ Try suppling an API key or configuring Google's application default
#>   credentials.
# }
```
