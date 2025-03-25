
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
`ragnar` is not a black box. `ragnar` is designed to be transparent—you
can inspect easily outputs at intermediate steps to understand what’s
happening.

## Installation

``` r
pak::pak("tidyverse/ragnar")
```

## Key Steps

### 1. Document Processing

`ragnar` works with a wide variety of document types, using
[MarkItDown](https://github.com/microsoft/markitdown) to convert content
to Markdown.

Key functions:

- `ragnar_find_links()`: Find all links in a webpage
- `ragnar_read()`: Convert a file or URL to markdown

### 2. Text Chunking

Next we divide each document into multiple chunks. Ragnar defaults to a
strategy that preserves some of the semantics of the document, but
provide plenty of options to tweak the approach.

Key functions:

- `ragnar_chunk()`: Higher-level function that both identifies semantic
  boundaries and chunks text.
- `ragnar_segment()`: Lower-level function that identifies semantic
  boundaries.
- `ragnar_chunk_segments()`: Lower-level function that chunks
  pre-segmented text.

### 3. Context Augmentation (Optional)

RAG applications benefit from augmenting text chunks with additional
context, such as document headings and subheadings. While `ragnar`
doesn’t directly export functions for this, it supports template-based
augmentation through `ragnar_read(frame_by_tags, split_by_tags)`. Future
versions will support generating context summaries via LLM calls.

Key functions:

- `ragnar_read()`: Use `frame_by_tags` and/or `split_by_tags` arguments
  to associate text chunks with their document position.
- `markdown_segment()`: Segment markdown text into a character vector
  using semantic tags (e.g., headings, paragraphs, or code chunks).
- `markdown_frame()`: Convert markdown text into a dataframe.

### 4. Embedding

`ragnar` can help compute embeddings for each chunk. The goal is for
`ragnar` to provide access to embeddings from popular LLM providers.
Currently only `ollama` and `openai` providers.

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
  extension](https://duckdb.org/docs/extensions/vss.html)
- `ragnar_retrieve_bm25()`: Retrieve using
  [`full-text search DuckDB extension`](https://duckdb.org/docs/extensions/full_text_search.html)

### 7. Re-ranking (Optional)

Re-ranking of retrieved chunks is planned for future releases.

### 8. Prompt Generation

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

store_location <- "r4ds.ragnar.duckdb"
unlink(store_location)

store <- ragnar_store_create(
  store_location,
  embed = \(x) ragnar::embed_ollama(x, model = "all-minilm")
)


for (page in pages) {
  message("ingesting: ", page)
  chunks <- page |>
    ragnar_read(frame_by_tags = c("h1", "h2", "h3")) |>
    dplyr::mutate(link = page) |>
    ragnar_chunk(boundaries = c("paragraph", "sentence")) |>
    # add context to chunks
    dplyr::mutate(text = glue::glue(r"---(
    # Excerpt from the book "R for Data Science (2e)"
    link: {link}
    chapter: {h1}
    section: {h2}
    subsection: {h3}
    content: {text}

    )---"))

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


## Retrieving Chunks
# Once the store is set up, retrieve the most relevant text chunks like this

embedding_near_chunks <- ragnar_retrieve_vss(store, text, top_k = 3)
embedding_near_chunks
#> # A tibble: 3 × 3
#>      id l2sq_distance text                                                      
#>   <int>         <dbl> <chr>                                                     
#> 1    31         0.920 "# Excerpt from the book \"R for Data Science (2e)\"\nlin…
#> 2   625         0.941 "# Excerpt from the book \"R for Data Science (2e)\"\nlin…
#> 3   639         0.943 "# Excerpt from the book \"R for Data Science (2e)\"\nlin…
embedding_near_chunks$text[1] |> cat(sep = "\n~~~~~~~~\n")
```

    #> # Excerpt from the book "R for Data Science (2e)"
    #> link: https://r4ds.hadley.nz/base-R.html
    #> chapter: # 27  A field guide to base R
    #> section: ## 27.2 Selecting multiple elements with `[`
    #> subsection: ### 27.2.2 Subsetting data frames
    #> content: There are quite a few different ways[1](#fn1) that you can use `[` with a data frame, but the most important way is to select rows and columns independently with `df[rows, cols]`. Here `rows` and `cols` are vectors as described above. For example, `df[rows, ]` and `df[, cols]` select just rows or just columns, using the empty subset to preserve the other dimension.
    #> 
    #> Here are a couple of examples:
    #> 
    #> ```
    #> df <- tibble(
    #>   x = 1:3,
    #>   y = c("a", "e", "f"),
    #>   z = runif(3)
    #> )
    #> 
    #> # Select first row and second column
    #> df[1, 2]
    #> #> # A tibble: 1 × 1
    #> #>   y
    #> #>   <chr>
    #> #> 1 a
    #> 
    #> # Select all rows and columns x and y
    #> df[, c("x" , "y")]
    #> #> # A tibble: 3 × 2
    #> #>       x y
    #> #>   <int> <chr>
    #> #> 1     1 a
    #> #> 2     2 e
    #> #> 3     3 f
    #> 
    #> # Select rows where `x` is greater than 1 and all columns
    #> df[df$x > 1, ]
    #> #> # A tibble: 2 × 3
    #> #>       x y         z
    #> #>   <int> <chr> <dbl>
    #> #> 1     2 e     0.834
    #> #> 2     3 f     0.601
    #> ```
    #> 
    #> We’ll come back to `$` shortly, but you should be able to guess what `df$x` does from the context: it extracts the `x` variable from `df`. We need to use it here because `[` doesn’t use tidy evaluation, so you need to be explicit about the source of the `x` variable.

``` r

bm25_near_chunks <- ragnar_retrieve_bm25(store, text, top_k = 3)
bm25_near_chunks
#> # A tibble: 3 × 3
#>      id bm25_score text                                                         
#>   <int>      <dbl> <chr>                                                        
#> 1    29       5.62 "# Excerpt from the book \"R for Data Science (2e)\"\nlink: …
#> 2   645       5.54 "# Excerpt from the book \"R for Data Science (2e)\"\nlink: …
#> 3   624       5.07 "# Excerpt from the book \"R for Data Science (2e)\"\nlink: …
bm25_near_chunks$text[1] |> cat(sep = "\n~~~~~~~~\n")
```

    #> # Excerpt from the book "R for Data Science (2e)"
    #> link: https://r4ds.hadley.nz/base-R.html
    #> chapter: # 27  A field guide to base R
    #> section: ## 27.2 Selecting multiple elements with `[`
    #> subsection: ### 27.2.1 Subsetting vectors
    #> content: There are five main types of things that you can subset a vector with, i.e., that can be the `i` in `x[i]`:
    #> 
    #> 1. **A vector of positive integers**. Subsetting with positive integers keeps the elements at those positions:
    #> 
    #>    ```
    #>    x <- c("one", "two", "three", "four", "five")
    #>    x[c(3, 2, 5)]
    #>    #> [1] "three" "two"   "five"
    #>    ```
    #> 
    #>    By repeating a position, you can actually make a longer output than input, making the term “subsetting” a bit of a misnomer.
    #> 
    #>    ```
    #>    x[c(1, 1, 5, 5, 5, 2)]
    #>    #> [1] "one"  "one"  "five" "five" "five" "two"
    #>    ```
    #> 2. **A vector of negative integers**. Negative values drop the elements at the specified positions:
    #> 
    #>    ```
    #>    x[c(-1, -3, -5)]
    #>    #> [1] "two"  "four"
    #>    ```
    #> 3. **A logical vector**. Subsetting with a logical vector keeps all values corresponding to a `TRUE` value. This is most often useful in conjunction with the comparison functions.
    #> 
    #>    ```
    #>    x <- c(10, 3, NA, 5, 8, 1, NA)
    #> 
    #>    # All non-missing values of x
    #>    x[!is.na(x)]
    #>    #> [1] 10  3  5  8  1
    #> 
    #>    # All even (or missing!) values of x
    #>    x[x %% 2 == 0]
    #>    #> [1] 10 NA  8 NA
    #>    ```
    #> 
    #>    Unlike `[filter()](https://dplyr.tidyverse.org/reference/filter.html)`, `NA` indices will be included in the output as `NA`s.
    #> 4. **A character vector**. If you have a named vector, you can subset it with a character vector:
    #> 
    #>    ```
    #>    x <- c(abc = 1, def = 2, xyz = 5)
    #>    x[c("xyz", "def")]
    #>    #> xyz def
    #>    #>   5   2
    #>    ```

``` r

# get both vss and bm26
relevant_chunks <- ragnar_retrieve(
  store, text, top_k = 3,
  methods = c("vss", "bm25")
)
relevant_chunks
#> # A tibble: 6 × 4
#>      id l2sq_distance bm25_score text                                           
#>   <int>         <dbl>      <dbl> <chr>                                          
#> 1    31         0.920       2.91 "# Excerpt from the book \"R for Data Science …
#> 2   625         0.941       3.58 "# Excerpt from the book \"R for Data Science …
#> 3   639         0.943       2.58 "# Excerpt from the book \"R for Data Science …
#> 4    29         0.980       5.62 "# Excerpt from the book \"R for Data Science …
#> 5   645         0.971       5.54 "# Excerpt from the book \"R for Data Science …
#> 6   624         1.22        5.07 "# Excerpt from the book \"R for Data Science …

# Register ellmer tool
## You can register an ellmer tool to retrieve chunks as well.
## This enables the LLM model to make tool calls to retreive chunks.
system_prompt <- stringr::str_squish(r"--(
    You are an expert R programmer and mentor.
    You often respond by first direct quoting material from book or documentation,
    then adding your own additional context and interpertation.
)--")
chat <- ellmer::chat_openai(system_prompt, model = "gpt-4o")
# chat <- ellmer::chat_ollama(system_prompt, model = "llama3.2:1b")

ragnar_register_tool_retrieve(chat, store)

chat$chat("How can I subset a dataframe?")
```

    #> To subset a dataframe in R, you can use multiple methods depending on your 
    #> needs. A common way to do this is with the square bracket `[` operator, which 
    #> allows you to select rows and columns separately using a matrix-style indexing.
    #> Here are some basic ways to subset a dataframe:
    #> 
    #> 1. **Select Specific Rows and Columns:**
    #>    You can select specific rows and columns by specifying their indices inside 
    #> the square brackets `df[rows, cols]`. Here's an example:
    #> 
    #>    ```r
    #>    df <- data.frame(x = 1:3, y = c("a", "e", "f"), z = runif(3))
    #> 
    #>    df[1, 2]  # Selects the first row and second column.
    #>    df[, c("x", "y")]  # Select all rows for columns x and y.
    #>    df[df$x > 1, ]  # Select rows where x is greater than 1 with all columns.
    #>    ```
    #> 
    #> 2. **Handling Tibbles vs. Data Frames:**
    #>    It's important to understand the difference in behavior between tibbles and 
    #> base data frames:
    #> 
    #>    ```r
    #>    df1 <- data.frame(x = 1:3)
    #>    df1[, "x"]  # Returns a vector.
    #> 
    #>    df2 <- tibble(x = 1:3)
    #>    df2[, "x"]  # Always returns a tibble.
    #> 
    #>    # To avoid ambiguity in data.frames and ensure a dataframe output:
    #>    df1[, "x", drop = FALSE]
    #>    ```
    #> 
    #> 3. **Logical Vectors:**
    #>    Logical vectors are often used for conditional subsetting:
    #> 
    #>    ```r
    #>    x <- c(10, 3, NA, 5, 8, 1, NA)
    #>    x[!is.na(x)]  # All non-missing values.
    #>    x[x %% 2 == 0]  # All even values, including NA.
    #>    ```
    #> 
    #> Each method has its specific use case and can be chosen based on the context of
    #> what you're trying to achieve with your dataset. Understanding these subsetting
    #> techniques provides flexibility in how you manipulate and analyze data frames.
