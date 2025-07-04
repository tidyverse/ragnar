
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ragnar <img src="man/figures/logo.png" align="right" height="138"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidyverse/ragnar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidyverse/ragnar/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

`ragnar` is an R package that helps implement Retrieval-Augmented
Generation (RAG) workflows. It focuses on providing a complete solution
with sensible defaults, while still giving the knowledgeable user
precise control over each steps. We don’t believe that you can fully
automate the creation of a good RAG system, so it’s important that
`ragnar` is not a black box. `ragnar` is designed to be transparent. You
can inspect easily outputs at intermediate steps to understand what’s
happening.

## Installation

You can install ragnar from CRAN with:

``` r
install.packages("ragnar")
```

## Key Steps

### 1. Document Processing

`ragnar` works with a wide variety of document types, using
[MarkItDown](https://github.com/microsoft/markitdown) to convert content
to Markdown.

Key functions:

- `read_as_markdown`: Convert a file or URL to markdown
- `ragnar_find_links()`: Find all links in a webpage

### 2. Text Chunking

Next we divide each document into chunks. Ragnar defaults to a strategy
that preserves some of the semantics of the document, but provide plenty
of opportunities to tweak the approach.

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
`ragnar_retreive()` and `ragnar_store_insert()`.

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

- `ragnar_retrieve()`
- `ragnar_retrieve_vss()`: Retrieve using [`vss` DuckDB
  extension](https://duckdb.org/docs/stable/core_extensions/vss)
- `ragnar_retrieve_bm25()`: Retrieve using
  [`full-text search DuckDB extension`](https://duckdb.org/docs/stable/core_extensions/full_text_search)
- `chunks_deoverlap()`: Consolidates retrieved chunks that overlap.

### 7. Chat Augmentation

`ragnar` can equip an `ellmer::Chat` object with a retrieve tool that
enables an LLM to retreive content from a store on-demand.

- `ragnar_register_tool_retrieve(chat, store)`.

## Usage

Here’s an example of using `ragnar` to create a knowledge store from the
*R for Data Science (2e)* book:

``` r
library(ragnar)

base_url <- "https://r4ds.hadley.nz"
pages <- ragnar_find_links(base_url)
#> ⠙ Finding links: 38 | On queue: 0 | Current depth: 0 | [0s]

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

    #> # A tibble: 5 × 6
    #>   origin                                  id        start   end headings   text 
    #>   <chr>                                   <list>    <int> <int> <chr>      <chr>
    #> 1 https://r4ds.hadley.nz/functions.html   <int [1]>  2203  4021 "# 25  Fu… "```…
    #> 2 https://r4ds.hadley.nz/logicals.html    <int [2]>  1623  4210 "# 12  Lo… "```…
    #> 3 https://r4ds.hadley.nz/logicals.html    <int [1]> 19413 20827 "# 12  Lo… "Tha…
    #> 4 https://r4ds.hadley.nz/regexps.html     <int [1]> 24602 26543 "# 15  Re… "```…
    #> 5 https://r4ds.hadley.nz/webscraping.html <int [1]> 13433 15289 "# 24  We… "###…

``` r


#'  Register ellmer tool
#' You can register an ellmer tool to let the LLM retrieve chunks.
system_prompt <- stringr::str_squish(
  r"--(
  You are an expert R programmer and mentor. You are concise.
  You always respond by first direct quoting material from book or documentation,
  then adding your own additional context and interpertation.
  Always include links to the source materials used.
  )--"
)
chat <- ellmer::chat_openai(
  system_prompt,
  model = "gpt-4.1",
  params = ellmer::params(temperature = .5)
)

ragnar_register_tool_retrieve(chat, store, top_k = 10)

chat$chat("How can I subset a dataframe?")
#> ◯ [tool call] rag_retrieve_from_store_001(text = "How can I subset a dataframe
#> in R?")
#> ● #> [{"origin":"https://r4ds.hadley.nz/arrow.html","id":15,"start":9344,"end"…
```

    #> From R for Data Science (2e):
    #> 
    #> > "Here are a couple of examples:
    #> >
    #> > ```r
    #> > df <- tibble(
    #> >   x = 1:3,
    #> >   y = c("a", "e", "f"),
    #> >   z = runif(3)
    #> > )
    #> >
    #> > # Select first row and second column
    #> > df[1, 2]
    #> >
    #> > # Select all rows and columns x and y
    #> > df[, c("x" , "y")]
    #> >
    #> > # Select rows where `x` is greater than 1 and all columns
    #> > df[df$x > 1, ]
    #> > ```
    #> >
    #> > Several dplyr verbs are special cases of `[`:  
    #> > - `filter()` is equivalent to subsetting the rows with a logical vector...  
    #> > - `select()` is equivalent to subsetting columns by name or position."  
    #> > ([source](https://r4ds.hadley.nz/base-R.html#subsetting-data-frames))
    #> 
    #> **My interpretation:**  
    #> You can subset data frames in R using base R or the tidyverse:
    #> 
    #> - **Base R:**  
    #>   - `df[rows, columns]` where `rows` and `columns` can be numbers, names, or 
    #> logical vectors.
    #>   - Example: `df[1:5, c("x", "y")]` selects the first 5 rows and columns x and 
    #> y.
    #>   - Logical subsetting: `df[df$x > 1, ]` selects rows where x > 1.
    #> 
    #> - **dplyr (tidyverse):**  
    #>   - Use `filter()` for rows: `df |> filter(x > 1)`
    #>   - Use `select()` for columns: `df |> select(x, y)`
    #> 
    #> See more:  
    #> - [R4DS: Subsetting Data 
    #> Frames](https://r4ds.hadley.nz/base-R.html#subsetting-data-frames)
    #> - [dplyr::filter()](https://dplyr.tidyverse.org/reference/filter.html)
    #> - [dplyr::select()](https://dplyr.tidyverse.org/reference/select.html)
