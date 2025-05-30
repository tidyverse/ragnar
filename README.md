
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

- `ragnar_read()`: Convert a file or URL to a dataframe
- `read_as_markdown`: Convert a file or URL to markdown
- `ragnar_find_links()`: Find all links in a webpage

### 2. Text Chunking

Next we divide each document into chunks. Ragnar defaults to a strategy
that preserves some of the semantics of the document, but provide plenty
of opportunities to tweak the approach.

Key functions:

- `ragnar_chunk()`: Higher-level function that both identifies semantic
  boundaries and chunks text.
- `ragnar_segment()`: Lower-level function that identifies semantic
  boundaries.
- `ragnar_chunk_segments()`: Lower-level function that chunks
  pre-segmented text.

### 3. Context Augmentation (Optional)

RAG applications benefit from augmenting text chunks with additional
context, such as document headings and subheadings. `ragnar` makes it
easy to keep track of headings and subheadings as part of chunking,
which can then be used to support template-based augmentation. (See
examples below)

Key functions:

- `ragnar_read()`: Use `frame_by_tags` and/or `split_by_tags` arguments
  to associate text chunks with their document position.
- `markdown_segment()`: Segment markdown text into a character vector
  using semantic tags (e.g., headings, paragraphs, or code chunks).
- `markdown_frame()`: Convert markdown text into a dataframe.

### 4. Embedding

`ragnar` can help compute embeddings for each chunk. The goal is for
`ragnar` to provide access to embeddings from popular LLM providers.
Currently `ollama` and `openai` providers are supported.

Key functions:

- `embed_ollama()`
- `embed_openai()`

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
  chunks <- page |>
    ragnar_read(frame_by_tags = c("h1", "h2", "h3")) |>
    ragnar_chunk(boundaries = c("paragraph", "sentence")) |>
    # add context to chunks
    dplyr::mutate(
      text = glue::glue(
        r"---(
        # Excerpt from the book "R for Data Science (2e)"
        link: {origin}
        chapter: {h1}
        section: {h2}
        subsection: {h3}
        content: {text}

        )---"
      )
    )

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

embedding_near_chunks <- ragnar_retrieve_vss(store, text, top_k = 3)
embedding_near_chunks
#> # A tibble: 3 × 6
#>      id metric_name     metric_value origin                          hash  text 
#>   <int> <chr>                  <dbl> <chr>                           <chr> <chr>
#> 1   630 cosine_distance        0.394 https://r4ds.hadley.nz/logical… 943f… "# E…
#> 2   653 cosine_distance        0.399 https://r4ds.hadley.nz/logical… 943f… "# E…
#> 3   632 cosine_distance        0.401 https://r4ds.hadley.nz/logical… 943f… "# E…
embedding_near_chunks$text[1] |> cat(sep = "\n~~~~~~~~\n")
#> # Excerpt from the book "R for Data Science (2e)"
#> link: https://r4ds.hadley.nz/logicals.html
#> chapter: # 12  Logical vectors
#> section: ## 12.1 Introduction
#> subsection: NA
#> content: In this chapter, you’ll learn tools for working with logical vectors. Logical vectors are the simplest type of vector because each element can only be one of three possible values: `TRUE`, `FALSE`, and `NA`. It’s relatively rare to find logical vectors in your raw data, but you’ll create and manipulate them in the course of almost every analysis.
#> 
#> We’ll begin by discussing the most common way of creating logical vectors: with numeric comparisons. Then you’ll learn about how you can use Boolean algebra to combine different logical vectors, as well as some useful summaries. We’ll finish off with `[if_else()](https://dplyr.tidyverse.org/reference/if_else.html)` and `[case_when()](https://dplyr.tidyverse.org/reference/case_when.html)`, two useful functions for making conditional changes powered by logical vectors.

bm25_near_chunks <- ragnar_retrieve_bm25(store, text, top_k = 3)
bm25_near_chunks
#> # A tibble: 3 × 6
#>      id metric_name metric_value origin                              hash  text 
#>   <int> <chr>              <dbl> <chr>                               <chr> <chr>
#> 1   988 bm25               0.661 https://r4ds.hadley.nz/webscraping… 8629… "# E…
#> 2   842 bm25               0.665 https://r4ds.hadley.nz/regexps.html 15ee… "# E…
#> 3   337 bm25               0.667 https://r4ds.hadley.nz/datetimes.h… fd3e… "# E…
bm25_near_chunks$text[1] |> cat(sep = "\n~~~~~~~~\n")
```

    #> # Excerpt from the book "R for Data Science (2e)"
    #> link: https://r4ds.hadley.nz/webscraping.html
    #> chapter: # 24  Web scraping
    #> section: ## 24.4 Extracting data
    #> subsection: ### 24.4.2 Nesting selections
    #> content: In most cases, you’ll use `[html_elements()](https://rvest.tidyverse.org/reference/html_element.html)` and `[html_element()](https://rvest.tidyverse.org/reference/html_element.html)` together, typically using `[html_elements()](https://rvest.tidyverse.org/reference/html_element.html)` to identify elements that will become observations then using `[html_element()](https://rvest.tidyverse.org/reference/html_element.html)` to find elements that will become variables. Let’s see this in action using a simple example. Here we have an unordered list (`<ul>)` where each list item (`<li>`) contains some information about four characters from StarWars:
    #> 
    #> ```
    #> html <- minimal_html("
    #>   <ul>
    #>     <li><b>C-3PO</b> is a <i>droid</i> that weighs <span class='weight'>167 kg</span></li>
    #>     <li><b>R4-P17</b> is a <i>droid</i></li>
    #>     <li><b>R2-D2</b> is a <i>droid</i> that weighs <span class='weight'>96 kg</span></li>
    #>     <li><b>Yoda</b> weighs <span class='weight'>66 kg</span></li>
    #>   </ul>
    #>   ")
    #> ```
    #> 
    #> We can use `[html_elements()](https://rvest.tidyverse.org/reference/html_element.html)` to make a vector where each element corresponds to a different character:
    #> 
    #> ```
    #> characters <- html |> html_elements("li")
    #> characters
    #> #> {xml_nodeset (4)}
    #> #> [1] <li>\n<b>C-3PO</b> is a <i>droid</i> that weighs <span class="weight"> ...
    #> #> [2] <li>\n<b>R4-P17</b> is a <i>droid</i>\n</li>
    #> #> [3] <li>\n<b>R2-D2</b> is a <i>droid</i> that weighs <span class="weight"> ...
    #> #> [4] <li>\n<b>Yoda</b> weighs <span class="weight">66 kg</span>\n</li>
    #> ```

``` r

# get both vss and bm26
relevant_chunks <- ragnar_retrieve(
  store,
  text,
  top_k = 3
)
relevant_chunks
#> # A tibble: 6 × 6
#>      id origin                                hash  text  cosine_distance   bm25
#>   <int> <chr>                                 <chr> <chr>           <dbl>  <dbl>
#> 1   630 https://r4ds.hadley.nz/logicals.html  943f… "# E…           0.394 NA    
#> 2   653 https://r4ds.hadley.nz/logicals.html  943f… "# E…           0.399 NA    
#> 3   632 https://r4ds.hadley.nz/logicals.html  943f… "# E…           0.401 NA    
#> 4   988 https://r4ds.hadley.nz/webscraping.h… 8629… "# E…          NA      0.661
#> 5   842 https://r4ds.hadley.nz/regexps.html   15ee… "# E…          NA      0.665
#> 6   337 https://r4ds.hadley.nz/datetimes.html fd3e… "# E…          NA      0.667

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
#> ◯ [tool call] rag_retrieve_from_store_001(text = "subset a dataframe")
#> ● #> # Excerpt from the book "R for Data Science (2e)"
#>   #> link: https://r4ds.hadley.nz/functions.html
#>   #> chapter: # 25  Functions
#>   #> section: ## 25.3 Data frame functions
#>   #> subsection: ### 25.3.3 Common use cases
#>   #> …
```

    #> From "R for Data Science (2e)":
    #> 
    #> > Several dplyr verbs are special cases of `[`:  
    #> > 
    #> > * `[filter()](https://dplyr.tidyverse.org/reference/filter.html)` is 
    #> equivalent to subsetting the rows with a logical vector, taking care to exclude
    #> missing values:
    #> > 
    #> >   ```
    #> >   df |> filter(x > 1)
    #> >   # same as
    #> >   df[!is.na(df$x) & df$x > 1, ]
    #> >   ```
    #> > * Both `[select()](https://dplyr.tidyverse.org/reference/select.html)` and 
    #> `[relocate()](https://dplyr.tidyverse.org/reference/relocate.html)` are similar
    #> to subsetting the columns with a character vector:
    #> > 
    #> >   ```
    #> >   df |> select(x, z)
    #> >   # same as
    #> >   df[, c("x", "z")]
    #> >   ```
    #> > 
    #> > Base R also provides a function that combines the features of `filter()` and 
    #> `select()` called [`subset()`](https://rdrr.io/r/base/subset.html):
    #> 
    #> > ```
    #> > df |> filter(x > 1) |> select(y, z)
    #> > # same as
    #> > df |> subset(x > 1, c(y, z))
    #> > ```
    #> 
    #> ([source](https://r4ds.hadley.nz/base-R.html#selecting-multiple-elements-with))
    #> 
    #> Additional context:
    #> 
    #> - **Base R**:  
    #>   - Subset rows: `df[rows, ]` (where `rows` is a logical or integer vector)
    #>   - Subset columns: `df[, cols]` (where `cols` is a name, number, or character 
    #> vector)
    #>   - Both: `df[rows, cols]`
    #>   - Use `drop = FALSE` to always return a data frame: `df[, "x", drop = FALSE]`
    #> 
    #> - **dplyr** (tidyverse):  
    #>   - Subset rows: `df |> filter(condition)`
    #>   - Subset columns: `df |> select(col1, col2)`
    #>   - Both: `df |> filter(condition) |> select(col1, col2)`
    #> 
    #> - **subset()**:  
    #>   - `subset(df, condition, select = c(cols))`
    #> 
    #> Choose the style that matches your workflow (base R or tidyverse).  
    #> - For more: [dplyr filter](https://dplyr.tidyverse.org/reference/filter.html), 
    #> [dplyr select](https://dplyr.tidyverse.org/reference/select.html), 
    #> [base::subset](https://rdrr.io/r/base/subset.html).
