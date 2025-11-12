# Register a 'retrieve' tool with ellmer

Register a 'retrieve' tool with ellmer

## Usage

``` r
ragnar_register_tool_retrieve(
  chat,
  store,
  store_description = "the knowledge store",
  ...,
  name = NULL,
  title = NULL
)
```

## Arguments

- chat:

  a `ellmer:::Chat` object.

- store:

  a string of a store location, or a `RagnarStore` object.

- store_description:

  Optional string, used for composing the tool description.

- ...:

  arguments passed on to
  [`ragnar_retrieve()`](https://ragnar.tidyverse.org/dev/reference/ragnar_retrieve.md).

- name, title:

  Optional tool function name and title. By default, `store@name` and
  `store@title` will be used if present. The tool `name` must be a valid
  R function name and should be unique with the tools registered with
  the [ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html)
  object. `title` is used for user-friendly display.

## Value

`chat`, invisibly.

## Examples

``` r
if (FALSE) { # (file.exists("r4ds.ragnar.duckdb") && Sys.getenv("OPENAI_API_KEY") != "")
system_prompt <- stringr::str_squish("
  You are an expert assistant in R programming.
  When responding, you first quote relevant material from books or documentation,
  provide links to the sources, and then add your own context and interpretation.
")
chat <- ellmer::chat_openai(system_prompt, model = "gpt-4.1")

store <- ragnar_store_connect("r4ds.ragnar.duckdb")
ragnar_register_tool_retrieve(chat, store)
chat$chat("How can I subset a dataframe?")
}
```
