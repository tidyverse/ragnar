
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ragnar <img src="man/figures/logo.png" align="right" height="138"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidyverse/ragnar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidyverse/ragnar/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

`ragnar` is an R package that helps implement Retrieval-Augmented
Generation (RAG) workflows. It focuses on providing a complete solution
with sensible defaults, while still giving the knowledgeable user
precise control over each step. We don’t believe that you can fully
automate the creation of a good RAG system, so it’s important that
`ragnar` is not a black box. `ragnar` is designed to be transparent. You
can easily inspect outputs at intermediate steps to understand what’s
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

## Key Steps

### 1. Document Processing

`ragnar` works with a wide variety of document types, using
[MarkItDown](https://github.com/microsoft/markitdown) to convert content
to Markdown.

Key functions:

- `read_as_markdown()`: Convert a file or URL to markdown
- `ragnar_find_links()`: Find all links in a webpage

### 2. Text Chunking

Next we divide each document into chunks. Ragnar defaults to a strategy
that preserves some of the semantics of the document, but provides
plenty of opportunities to tweak the approach.

Key functions:

- `markdown_chunk()`: Full-featured chunker that identifies semantic
  boundaries and intelligently chunks text.

### 3. Context Augmentation (Optional)

RAG applications benefit from augmenting text chunks with additional
context, such as document headings and subheadings. `ragnar` makes it
easy to keep track of headings and subheadings as part of chunking.

`markdown_chunk()` automatically associates each chunk with the headings
that are in scope for that chunk.

### 4. Embedding

`ragnar` can help compute embeddings for each chunk. The goal is for
`ragnar` to provide access to embeddings from popular LLM providers.

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
using `duckdb` by default. The API is designed to be extensible,
allowing additional packages to implement support for different storage
providers.

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

`ragnar` can equip an `ellmer::Chat` object with a retrieve tool that
enables an LLM to retrieve content from a store on-demand.

- `ragnar_register_tool_retrieve(chat, store)`.

## Usage

Here’s an example of using `ragnar` to create a knowledge store from the
*R for Data Science (2e)* book:

``` r
library(ragnar)

base_url <- "https://r4ds.hadley.nz"
pages <- ragnar_find_links(base_url)

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
#> ingesting: https://r4ds.hadley.nz/
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
#' Once the store is set up, retrieve the most relevant text chunks like this

(relevant_chunks <- ragnar_retrieve(store, text))
```

    #> # A tibble: 4 × 9
    #>   origin         doc_id chunk_id start   end cosine_distance bm25  context text 
    #>   <chr>          <list> <list>   <int> <int> <list>          <lis> <chr>   <chr>
    #> 1 https://r4ds.… <int>  <int>     2192  4007 <dbl [1]>       <dbl> "# 25 … "```…
    #> 2 https://r4ds.… <int>  <int>     1622  4205 <dbl [2]>       <dbl> "# 12 … "```…
    #> 3 https://r4ds.… <int>  <int>    19379 20792 <dbl [1]>       <dbl> "# 12 … "Tha…
    #> 4 https://r4ds.… <int>  <int>    12795 15259 <dbl [2]>       <dbl> "# 24 … "The…

``` r


#'  Register ellmer tool
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
  model = "gpt-4.1"
)

ragnar_register_tool_retrieve(chat, store, top_k = 10)

chat$chat("How can I subset a dataframe?")
#> ◯ [tool call] rag_retrieve_from_store_001(text = "How to subset a dataframe in
#> R")
#> ● #> [{"origin":"https://r4ds.hadley.nz/arrow.html","doc_id":2,"chunk_id":13,"…
```

    #> You can subset a dataframe in R in several ways:
    #> 
    #> 1. Using the [ (bracket) operator:
    #> - Select rows and/or columns: df[rows, cols]
    #> - Example:
    #> ```r
    #> df[1, 2]          # first row, second column
    #> df[, c("x","y")]  # all rows, columns x and y
    #> df[df$x > 1, ]    # rows where x > 1, all columns
    #> ```
    #> If df is a tibble, the result of df[, "x"] is always a tibble; for a 
    #> data.frame, it returns a vector unless you use drop=FALSE: df[, "x", 
    #> drop=FALSE] 
    #> ([source](https://r4ds.hadley.nz/base-R.html#subsetting-data-frames)).
    #> 
    #> 2. With dplyr for more readable code:
    #> - Use filter() for subsetting rows and select() for columns:
    #> ```r
    #> library(dplyr)
    #> df %>% filter(x > 1)            # rows where x > 1
    #> df %>% select(x, y)             # columns x and y
    #> df %>% filter(x > 1) %>% select(x, y)  # both
    #> ```
    #> - Many dplyr verbs are wrappers for subsetting, e.g., filter(), arrange(), and 
    #> select() ([source](https://r4ds.hadley.nz/base-R.html#dplyr-equivalents)).
    #> 
    #> 3. subset() function (base R):
    #> ```r
    #> subset(df, x > 1, select = c(x, y))
    #> ```
    #> This combines row/column subsetting in one call.
    #> 
    #> Summary: Use df[rows, cols], dplyr's filter() and select(), or subset() for 
    #> subsetting dataframes.
    #> - Reference: https://r4ds.hadley.nz/base-R.html#subsetting-data-frames
    #> - Reference: https://dplyr.tidyverse.org/reference/filter.html
    #> - Reference: https://rdrr.io/r/base/subset.html
    #> 
    #> Let me know if you want an example with your own dataset or more details!
