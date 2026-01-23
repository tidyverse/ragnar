# Embed text using a Bedrock model

Embed text using a Bedrock model

## Usage

``` r
embed_bedrock(x, model, profile = "", api_args = list())
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

  Currently only Cohere.ai and Amazon Titan models are supported. There
  are no guardarails for the kind of model that is used, but the model
  must be available in the AWS region specified by the profile. You may
  look for available models in the Bedrock Model Catalog

- profile:

  AWS profile to use. It's passed to
  [paws.common::locate_credentials](https://paws-r.r-universe.dev/paws.common/reference/locate_credentials.html)
  to locate AWS credentials.

- api_args:

  Additional arguments to pass to the Bedrock API. Depending on the
  `model`, you might be able to provide different parameters. Check the
  documentation for the model you are using in the [Bedrock user
  guide](https://docs.aws.amazon.com/bedrock/latest/userguide/model-parameters.html).

## Value

If `x` is missing returns a function that can be called to get
embeddings. If `x` is not missing, a matrix of embeddings with 1 row per
input string, or a dataframe with an 'embedding' column.

## See also

[`embed_ollama()`](https://ragnar.tidyverse.org/dev/reference/embed_ollama.md)
