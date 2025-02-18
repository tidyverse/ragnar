
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ragnar

🚧 Under active development 🚧

<!-- badges: start -->

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
pak::pak("t-kalinowski/ragnar")
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
- `markdown_frame()`: Convert markdown text into a dataframe.
- `markdown_segment()`: Segment markdown text into a character vector using
  semantic tags (e.g., headings, paragraphs, or code chunks).

### 4. Embedding

`ragnar` can help compute embeddings for each chunk. The goal is for
`ragnar` to provide access to embeddings from popular LLM providers.
Currently only `ollama` and `openai` providers.

Key functions:

- `embed_ollama()`
- `embed_openai()`

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

`ragnar` will provide tools for incorporating retrieved chunks into LLM
prompts.

## Usage

Here’s an example of using `ragnar` to store and retrieve chunks from
the *R for Data Science (2e)* book:

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
    ragnar_chunk(boundaries = c("paragraph", "sentence")) |>
    # add context to chunks
    dplyr::mutate(text = glue::glue(r"---(
    # Excerpt from the book "R for Data Science (2e)"
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
#> ingesting: https://r4ds.hadley.nz/index.html
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

### Retrieving Chunks

# Once the store is set up, retrieve the most relevant text chunks:

# store_location <- "r4ds.ragnar.duckdb"
store <- ragnar_store_connect(store_location, read_only = TRUE)

text <- "How can I subset a dataframe with a logical vector?"

embedding_near_chunks <- ragnar_retrieve_vss(store, text, top_k = 3)
embedding_near_chunks
#> # A tibble: 3 × 3
#>      id l2sq_distance text                                                      
#>   <int>         <dbl> <chr>                                                     
#> 1    31         0.929 "# Excerpt from the book \"R for Data Science (2e)\"\ncha…
#> 2   631         0.933 "# Excerpt from the book \"R for Data Science (2e)\"\ncha…
#> 3    32         0.960 "# Excerpt from the book \"R for Data Science (2e)\"\ncha…
embedding_near_chunks$text |> cat(sep = "\n~~~~~~~~\n")
```

    #> # Excerpt from the book "R for Data Science (2e)"
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
    #> 
    #> ~~~~~~~~
    #> # Excerpt from the book "R for Data Science (2e)"
    #> chapter: # 12  Logical vectors
    #> section: ## 12.1 Introduction
    #> subsection: NA
    #> content: In this chapter, you’ll learn tools for working with logical vectors. Logical vectors are the simplest type of vector because each element can only be one of three possible values: `TRUE`, `FALSE`, and `NA`. It’s relatively rare to find logical vectors in your raw data, but you’ll create and manipulate them in the course of almost every analysis.
    #> 
    #> We’ll begin by discussing the most common way of creating logical vectors: with numeric comparisons. Then you’ll learn about how you can use Boolean algebra to combine different logical vectors, as well as some useful summaries. We’ll finish off with `[if_else()](https://dplyr.tidyverse.org/reference/if_else.html)` and `[case_when()](https://dplyr.tidyverse.org/reference/case_when.html)`, two useful functions for making conditional changes powered by logical vectors.
    #> 
    #> ~~~~~~~~
    #> # Excerpt from the book "R for Data Science (2e)"
    #> chapter: # 27  A field guide to base R
    #> section: ## 27.2 Selecting multiple elements with `[`
    #> subsection: ### 27.2.2 Subsetting data frames
    #> content: There’s an important difference between tibbles and data frames when it comes to `[`. In this book, we’ve mainly used tibbles, which *are* data frames, but they tweak some behaviors to make your life a little easier. In most places, you can use “tibble” and “data frame” interchangeably, so when we want to draw particular attention to R’s built-in data frame, we’ll write `data.frame`. If `df` is a `data.frame`, then `df[, cols]` will return a vector if `col` selects a single column and a data frame if it selects more than one column. If `df` is a tibble, then `[` will always return a tibble.
    #> 
    #> ```
    #> df1 <- data.frame(x = 1:3)
    #> df1[, "x"]
    #> #> [1] 1 2 3
    #> 
    #> df2 <- tibble(x = 1:3)
    #> df2[, "x"]
    #> #> # A tibble: 3 × 1
    #> #>       x
    #> #>   <int>
    #> #> 1     1
    #> #> 2     2
    #> #> 3     3
    #> ```
    #> 
    #> One way to avoid this ambiguity with `data.frame`s is to explicitly specify `drop = FALSE`:
    #> 
    #> ```
    #> df1[, "x" , drop = FALSE]
    #> #>   x
    #> #> 1 1
    #> #> 2 2
    #> #> 3 3
    #> ```

``` r

bm25_near_chunks <- ragnar_retrieve_bm25(store, text, top_k = 3)
bm25_near_chunks
#> # A tibble: 3 × 3
#>      id bm25_score text                                                         
#>   <int>      <dbl> <chr>                                                        
#> 1    29       5.62 "# Excerpt from the book \"R for Data Science (2e)\"\nchapte…
#> 2   651       5.45 "# Excerpt from the book \"R for Data Science (2e)\"\nchapte…
#> 3   630       5.17 "# Excerpt from the book \"R for Data Science (2e)\"\nchapte…
bm25_near_chunks$text |> cat(sep = "\n~~~~~~~~\n")
```

    #> # Excerpt from the book "R for Data Science (2e)"
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
    #> 
    #> ~~~~~~~~
    #> # Excerpt from the book "R for Data Science (2e)"
    #> chapter: # 12  Logical vectors
    #> section: ## 12.4 Summaries
    #> subsection: ### 12.4.3 Logical subsetting
    #> content: There’s one final use for logical vectors in summaries: you can use a logical vector to filter a single variable to a subset of interest. This makes use of the base `[` (pronounced subset) operator, which you’ll learn more about in [Section 27.2](base-R.html#sec-subset-many).
    #> 
    #> Imagine we wanted to look at the average delay just for flights that were actually delayed. One way to do so would be to first filter the flights and then calculate the average delay:
    #> 
    #> ```
    #> flights |>
    #>   filter(arr_delay > 0) |>
    #>   group_by(year, month, day) |>
    #>   summarize(
    #>     behind = mean(arr_delay),
    #>     n = n(),
    #>     .groups = "drop"
    #>   )
    #> #> # A tibble: 365 × 5
    #> #>    year month   day behind     n
    #> #>   <int> <int> <int>  <dbl> <int>
    #> #> 1  2013     1     1   32.5   461
    #> #> 2  2013     1     2   32.0   535
    #> #> 3  2013     1     3   27.7   460
    #> #> 4  2013     1     4   28.3   297
    #> #> 5  2013     1     5   22.6   238
    #> #> 6  2013     1     6   24.4   381
    #> #> # ℹ 359 more rows
    #> ```
    #> 
    #> This works, but what if we wanted to also compute the average delay for flights that arrived early? We’d need to perform a separate filter step, and then figure out how to combine the two data frames together[3](#fn3). Instead you could use `[` to perform an inline filtering: `arr_delay[arr_delay > 0]` will yield only the positive arrival delays.
    #> 
    #> This leads to:
    #> 
    #> ~~~~~~~~
    #> # Excerpt from the book "R for Data Science (2e)"
    #> chapter: # 12  Logical vectors – R for Data Science (2e)
    #> section: ## Table of contents
    #> subsection: NA
    #> content: * [12.1 Introduction](#introduction)
    #>   + [12.1.1 Prerequisites](#prerequisites)
    #> * [12.2 Comparisons](#comparisons)
    #>   + [12.2.1 Floating point comparison](#sec-fp-comparison)
    #>   + [12.2.2 Missing values](#sec-na-comparison)
    #>   + [12.2.3 `is.na()`](#is.na)
    #>   + [12.2.4 Exercises](#exercises)
    #> * [12.3 Boolean algebra](#boolean-algebra)
    #>   + [12.3.1 Missing values](#sec-na-boolean)
    #>   + [12.3.2 Order of operations](#sec-order-operations-boolean)
    #>   + [12.3.3 `%in%`](#in)
    #>   + [12.3.4 Exercises](#exercises-1)
    #> * [12.4 Summaries](#sec-logical-summaries)
    #>   + [12.4.1 Logical summaries](#logical-summaries)
    #>   + [12.4.2 Numeric summaries of logical vectors](#sec-numeric-summaries-of-logicals)
    #>   + [12.4.3 Logical subsetting](#logical-subsetting)
    #>   + [12.4.4 Exercises](#exercises-2)
    #> * [12.5 Conditional transformations](#conditional-transformations)
    #>   + [12.5.1 `if_else()`](#if_else)
    #>   + [12.5.2 `case_when()`](#case_when)
    #>   + [12.5.3 Compatible types](#compatible-types)
    #>   + [12.5.4 Exercises](#exercises-3)
    #> * [12.6 Summary](#summary)
    #> 
    #> * [Edit this page](https://github.com/hadley/r4ds/edit/main/logicals.qmd)
    #> * [Report an issue](https://github.com/hadley/r4ds/issues/new)
    #> 
    #> 1. [Transform](./transform.html)
    #> 2. [12  Logical vectors](./logicals.html)

``` r

# get both vss and bm26
(relevant_chunks <- ragnar_retrieve(
  store, text, top_k = 3,
  methods = c("vss", "bm25")
))
#> # A tibble: 6 × 4
#>      id l2sq_distance bm25_score text                                           
#>   <int>         <dbl>      <dbl> <chr>                                          
#> 1    31         0.929       2.92 "# Excerpt from the book \"R for Data Science …
#> 2   631         0.933       3.55 "# Excerpt from the book \"R for Data Science …
#> 3    32         0.960       2.40 "# Excerpt from the book \"R for Data Science …
#> 4    29         0.965       5.62 "# Excerpt from the book \"R for Data Science …
#> 5   651         1.01        5.45 "# Excerpt from the book \"R for Data Science …
#> 6   630         1.15        5.17 "# Excerpt from the book \"R for Data Science …
```
