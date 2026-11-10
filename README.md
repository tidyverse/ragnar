
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ragnar <a href="https://ragnar.tidyverse.org"><img src="man/figures/logo.png" align="right" height="138" alt="ragnar website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidyverse/ragnar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidyverse/ragnar/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

ragnar is an R package that helps implement Retrieval-Augmented
Generation (RAG) workflows. It focuses on providing a complete solution
with sensible defaults, while still giving the knowledgeable user
precise control over each step. We don’t believe that you can fully
automate the creation of a good RAG system, so it’s important that
ragnar is not a black box. ragnar is designed to be transparent. You can
easily inspect outputs at intermediate steps to understand what’s
happening.

## Installation

You can install ragnar from CRAN with:

``` r
install.packages("ragnar")
```

You can install the development version from GitHub with:

``` r
# install.packages("pak")
pak::pak("tidyverse/ragnar")
```

On native Windows ARM64, ragnar disables MarkItDown document conversion
and DuckDB stores. Chunking remains available; Python features currently
also encounter [reticulate’s architecture
check](https://github.com/rstudio/reticulate/issues/1934). Use x86_64 R
on Windows for the complete workflow.

## Key Steps

### 1. Document Processing

ragnar works with a wide variety of document types, using
[MarkItDown](https://github.com/microsoft/markitdown) to convert content
to Markdown.

Key functions:

- `read_as_markdown()`: Convert a file or URL to markdown
- `ragnar_find_links()`: Find all links in a webpage

### 2. Text Chunking

Next we divide each document into chunks. ragnar defaults to a strategy
that preserves some of the semantics of the document, but provides
plenty of opportunities to tweak the approach.

Key functions:

- `markdown_chunk()`: Full-featured chunker that identifies semantic
  boundaries and intelligently chunks text.

### 3. Context Augmentation (Optional)

RAG applications benefit from augmenting text chunks with additional
context, such as document headings and subheadings. ragnar makes it easy
to keep track of headings and subheadings as part of chunking.

`markdown_chunk()` automatically associates each chunk with the headings
that are in scope for that chunk.

### 4. Embedding

ragnar can help compute embeddings for each chunk. The goal is for
ragnar to provide access to embeddings from popular LLM providers.

Key functions:

- `embed_ollama()`
- `embed_openai()`
- `embed_bedrock()`
- `embed_databricks()`
- `embed_google_vertex()`

Note that calling the embedding function directly is typically not
necessary. Instead, the embedding function is specified when a store is
first created, and then automatically called when needed by
`ragnar_retrieve()` and `ragnar_store_insert()`.

### 5. Storage

Processed data is stored in a format optimized for efficient searching,
using [DuckDB](https://duckdb.org). allowing additional packages to
implement support for different storage providers.

Key functions:

- `ragnar_store_create()`
- `ragnar_store_connect()`
- `ragnar_store_insert()`

### 6. Retrieval

Given a prompt, retrieve related chunks based on embedding distance or
bm25 text search.

Key functions:

- `ragnar_retrieve()`: high-level function that performs both `vss` and
  `bm25` search and de-overlaps retrieved results.
- `ragnar_retrieve_vss()`: Retrieve using [`vss` DuckDB
  extension](https://duckdb.org/docs/stable/core_extensions/vss)
- `ragnar_retrieve_bm25()`: Retrieve using
  [`full-text search DuckDB extension`](https://duckdb.org/docs/stable/core_extensions/full_text_search)
- `chunks_deoverlap()`: Consolidates retrieved chunks that overlap.

### 7. Chat Augmentation

ragnar can equip an `ellmer::Chat` object with a retrieve tool that
enables an LLM to retrieve content from a store on-demand.

- `ragnar_register_tool_retrieve(chat, store)`.

## Usage

Here’s an example of using ragnar to create a knowledge store from the
*R for Data Science (2e)* book:

``` r
library(ragnar)

base_url <- "https://r4ds.hadley.nz"
pages <- ragnar_find_links(base_url, url_filter = \(url) {
  url[startsWith(url, "https://r4ds.hadley.nz")]
})

store_location <- "r4ds.ragnar.duckdb"

store <- ragnar_store_create(
  store_location,
  embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small")
)

for (page in pages) {
  message("ingesting: ", page)
  chunks <- page |> read_as_markdown() |> markdown_chunk()
  ragnar_store_insert(store, chunks)
}
#> ingesting: https://r4ds.hadley.nz/arrow.html
#> ingesting: https://r4ds.hadley.nz/base-R.html
#> ingesting: https://r4ds.hadley.nz/communicate.html
#> ingesting: https://r4ds.hadley.nz/communication.html
#> ingesting: https://r4ds.hadley.nz/data-import.html
#> ingesting: https://r4ds.hadley.nz/data-tidy.html
#> ingesting: https://r4ds.hadley.nz/data-transform.html
#> ingesting: https://r4ds.hadley.nz/data-visualize.html
#> ingesting: https://r4ds.hadley.nz/databases.html
#> ingesting: https://r4ds.hadley.nz/datetimes.html
#> ingesting: https://r4ds.hadley.nz/EDA.html
#> ingesting: https://r4ds.hadley.nz/factors.html
#> ingesting: https://r4ds.hadley.nz/functions.html
#> ingesting: https://r4ds.hadley.nz/import.html
#> ingesting: https://r4ds.hadley.nz/intro.html
#> ingesting: https://r4ds.hadley.nz/iteration.html
#> ingesting: https://r4ds.hadley.nz/joins.html
#> ingesting: https://r4ds.hadley.nz/layers.html
#> ingesting: https://r4ds.hadley.nz/logicals.html
#> ingesting: https://r4ds.hadley.nz/missing-values.html
#> ingesting: https://r4ds.hadley.nz/numbers.html
#> ingesting: https://r4ds.hadley.nz/preface-2e.html
#> ingesting: https://r4ds.hadley.nz/program.html
#> ingesting: https://r4ds.hadley.nz/quarto-formats.html
#> ingesting: https://r4ds.hadley.nz/quarto.html
#> ingesting: https://r4ds.hadley.nz/rectangling.html
#> ingesting: https://r4ds.hadley.nz/regexps.html
#> ingesting: https://r4ds.hadley.nz/spreadsheets.html
#> ingesting: https://r4ds.hadley.nz/strings.html
#> ingesting: https://r4ds.hadley.nz/transform.html
#> ingesting: https://r4ds.hadley.nz/visualize.html
#> ingesting: https://r4ds.hadley.nz/webscraping.html
#> ingesting: https://r4ds.hadley.nz/whole-game.html
#> ingesting: https://r4ds.hadley.nz/workflow-basics.html
#> ingesting: https://r4ds.hadley.nz/workflow-help.html
#> ingesting: https://r4ds.hadley.nz/workflow-scripts.html
#> ingesting: https://r4ds.hadley.nz/workflow-style.html

ragnar_store_build_index(store)
```

Once the store is set up, you can then retrieve the most relevant text
chunks.

``` r
#' ## Retrieving Chunks

library(ragnar)
store_location <- "r4ds.ragnar.duckdb"
store <- ragnar_store_connect(store_location, read_only = TRUE)

text <- "How can I subset a dataframe with a logical vector?"


#' # Retrieving Chunks
#' Once the store is set up, retrieve the most relevant text chunks like this:
(relevant_chunks <- ragnar_retrieve(store, text))
```

    #> # A tibble: 3 × 9
    #>   origin         doc_id chunk_id start   end cosine_distance bm25  context text 
    #>   <chr>           <int> <list>   <int> <int> <list>          <lis> <chr>   <chr>
    #> 1 https://r4ds.…      2 <int>     2545  4779 <dbl [2]>       <dbl> "# 27 … "###…
    #> 2 https://r4ds.…     19 <int>     1634  4217 <dbl [2]>       <dbl> "# 12 … "```…
    #> 3 https://r4ds.…     19 <int>    19400 20813 <dbl [1]>       <dbl> "# 12 … "Tha…

``` r


#' # Register ellmer tool
#' You can register an ellmer tool to let the LLM retrieve chunks.
system_prompt <- stringr::str_squish(
  "
  You are an expert R programmer and mentor. You are concise.

  Before responding, retrieve relevant material from the knowledge store. Quote or
  paraphrase passages, clearly marking your own words versus the source. Provide a
  working link for every source cited, as well as any additional relevant links.
  Do not answer unless you have retrieved and cited a source.
  "
)
chat <- ellmer::chat_openai(
  system_prompt,
  api_args = list(
    reasoning = list(effort = "low"),
    text = list(verbosity = "low")
  )
)
#> Using model = "gpt-6.1-terra".

ragnar_register_tool_retrieve(chat, store, top_k = 10)

chat$chat("How can I subset a dataframe?")
#> ◯ [tool call] search_store_001(text = c("How to subset rows and columns of a
#> dataframe in R using brackets and subset", ...)
#> ● #> [
#>   #> {
#>   #> "origin": "https://r4ds.hadley.nz/base-R.html",
#>   #> "doc_id": 2,
#>   #> "chunk_id": [27, 28, 29, 30, 31, 32, 33, 34],
#>   #> …
```

    #> In base R, use `df[rows, columns]`. As [R for Data 
    #> Science](https://r4ds.hadley.nz/base-R.html#sec-subset-many) explains, leaving 
    #> either side empty keeps all rows or columns.
    #> 
    #> ```r
    #> df[1:3, ]                          # First three rows
    #> df[, c("name", "age")]             # Selected columns
    #> df[!is.na(df$age) & df$age > 18, ] # Rows matching a condition
    #> df[1:3, "name", drop = FALSE]      # Keep result as a data frame
    #> ```
    #> 
    #> You can also combine row and column selection:
    #> 
    #> ```r
    #> subset(df, age > 18, select = c(name, age))
    #> ```
