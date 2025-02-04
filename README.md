
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ragnar

🚧 Under active development 🚧

<!-- badges: start -->
<!-- badges: end -->

`ragnar` is an R package that helps implement Retrieval-Augmented
Generation (RAG) workflows. It focuses on providing a complete solution
with sensible defaults, while still giving the knowledgeable user
precise control over all the steps. We don’t believe that you can fully
automate the creation of a good RAG, so it’s important that ragnar is
not a black box; `ragnar` is designed to be transparent—you can inspect
outputs at intermediate steps to understand what’s happening.

## Installation

``` r
pak::pak("t-kalinowski/ragnar")
```

## Key Steps

### 1. Document Processing

Ragnar starts with a directory of markdown or HTML files. In the long
term, we plan to offer tools for building this directory, such as [web
scraping](https://github.com/r-lib/httr2/pull/584) or markdown
processing. We also intend to add tools for simplifying HTML using
readability techniques or a pandoc AST walker to remove extraneous
attributes.

### 2. Text Chunking

Next we divide each document into multiple chunks. We default to a
strategy that preserves some of the semantics of the document, but
provide plenty of options to tweak the approach.

### 3. (Optionally) Context augmentation

Add needed context to the chunk. The goal it to support a variety of
workflows for attaching context to chunked text, ranging from templating
strings `glue()` to sophisticated workflows that involve calling out to
LLMs to generate relevant contextualized summaries.

### 4. Embedding

Support computing an embedding for each chunk. The goal is for `ragnar`
to provide access to embeddings from popular LLM providers. Currently,
only support for `ollama` is implemented.

### 5. Storage

Store all processed data in a format optimized for efficient searching,
using `duckdb` as a default. The goal is for the API to be extensible,
so additional packages to implement support for different store
providers.

### 6. Retrieval

Retrieve related chunks based on cosine similarity of embeddings. In the
near future we intend also support retrieval using BM25 ranking and
regular text search.

- **Vector similarity search:** [`vss` DuckDB
  extension](https://duckdb.org/docs/extensions/vss.html)
- **Text search:**
  [`full-text search extension`](https://duckdb.org/docs/extensions/full_text_search.html)

### 7. (Optional) Re-ranking

Rerank retreived chunks. (Not yet implemented)

### 8. Prompt generation

Ragnar will provide a set of tools for helping to incorporate the
retrieved chunks into an LLM prompt.

## Usage

Below is an example of using `ragnar` to store and retrieve chunks from
the *R for Data Science (2e)* book.

``` r
library(ragnar)

# local clone copy of https://r4ds.hadley.nz/base-r
# system2("git", c("clone", shQuote("https://github.com/hadley/r4ds/"), shQuote(normalizePath("~/github/hadley/r4ds"))))
# remotes::install_local("~/github/hadley/r4ds", dependencies = TRUE)
# system("quarto render ~/github/hadley/r4ds")

store <- ragnar_store_create(":memory:")

files <- Sys.glob("~/github/hadley/r4ds/_book/*.html")
# file <- files[1]
for (file in files) {
  message("ingesting: ", file)
  chunks <- file |>
    ragnar_read_document(frame_by_tags = c("h1", "h2")) |>
    ragnar_chunk(boundaries = c("paragraph", "sentence")) |>
    # add context to chunks
    dplyr::mutate(text = glue::glue(r"---(
      # Excerpt from the book "R for Data Science (2e)"
      chapter: {h1}
      section: {h2}
      content: {text}

      )---"))

  # chunks <- embed_ollama(chunks)
  ragnar_store_insert(store, chunks)
}
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/EDA.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/arrow.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/base-R.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/communicate.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/communication.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/data-import.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/data-tidy.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/data-transform.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/data-visualize.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/databases.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/datetimes.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/factors.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/functions.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/import.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/index.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/intro.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/iteration.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/joins.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/layers.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/logicals.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/missing-values.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/numbers.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/preface-2e.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/program.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/quarto-formats.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/quarto.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/rectangling.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/regexps.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/spreadsheets.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/strings.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/transform.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/visualize.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/webscraping.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/whole-game.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/workflow-basics.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/workflow-help.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/workflow-scripts.html
#> ingesting: /Users/tomasz/github/hadley/r4ds/_book/workflow-style.html

ragnar_store_build_index(store)


### Retrieving Chunks

# Once the store is set up, retrieve the most relevant text chunks:

prompt <- "How can I subset a dataframe with a logical vector?"
relevent_chunks <- ragnar_retrieve_vss(store, prompt, top_k = 3)

relevent_chunks$text |> cat(sep = "\n-------\n")
#> # Excerpt from the book "R for Data Science (2e)"
#> chapter: 27 A field guide to base R
#> section: 27.2 Selecting multiple elements with [
#> content: x <- c(abc = 1, def = 2, xyz = 5)
#> x[c("xyz", "def")]
#> #> xyz def 
#> #>   5   2
#> 
#> As with subsetting with positive integers, you can use a character vector to duplicate individual entries.
#> 
#> Nothing. The final type of subsetting is nothing, x[], which returns the complete x. This is not useful for subsetting vectors, but as we’ll see shortly, it is useful when subsetting 2d structures like tibbles.
#> 
#> 27.2.2 Subsetting data frames
#> 
#> There are quite a few different ways1 that you can use [ with a data frame, but the most important way is to select rows and columns independently with df[rows, cols]. Here rows and cols are vectors as described above. For example, df[rows, ] and df[, cols] select just rows or just columns, using the empty subset to preserve the other dimension.
#> 
#> Here are a couple of examples:
#> 
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
#> 
#> We’ll come back to $ shortly, but you should be able to guess what df$x does from the context: it extracts the x variable from df. We need to use it here because [ doesn’t use tidy evaluation, so you need to be explicit about the source of the x variable.
#> 
#> -------
#> # Excerpt from the book "R for Data Science (2e)"
#> chapter: 12 Logical vectors
#> section: 12.1 Introduction
#> content: In this chapter, you’ll learn tools for working with logical vectors. Logical vectors are the simplest type of vector because each element can only be one of three possible values: TRUE, FALSE, and NA. It’s relatively rare to find logical vectors in your raw data, but you’ll create and manipulate them in the course of almost every analysis.
#> 
#> We’ll begin by discussing the most common way of creating logical vectors: with numeric comparisons. Then you’ll learn about how you can use Boolean algebra to combine different logical vectors, as well as some useful summaries. We’ll finish off with if_else() and case_when(), two useful functions for making conditional changes powered by logical vectors.
#> 
#> 12.1.1 Prerequisites
#> 
#> Most of the functions you’ll learn about in this chapter are provided by base R, so we don’t need the tidyverse, but we’ll still load it so we can use mutate(), filter(), and friends to work with data frames. We’ll also continue to draw examples from the nycflights13::flights dataset.
#> 
#> library(tidyverse)
#> library(nycflights13)
#> 
#> However, as we start to cover more tools, there won’t always be a perfect real example. So we’ll start making up some dummy data with c():
#> 
#> x <- c(1, 2, 3, 5, 7, 11, 13)
#> x * 2
#> #> [1]  2  4  6 10 14 22 26
#> 
#> This makes it easier to explain individual functions at the cost of making it harder to see how it might apply to your data problems. Just remember that any manipulation we do to a free-floating vector, you can do to a variable inside a data frame with mutate() and friends.
#> 
#> -------
#> # Excerpt from the book "R for Data Science (2e)"
#> chapter: 12 Logical vectors
#> section: 12.3 Boolean algebra
#> content: flights |> 
#>   mutate(
#>     nov = month == 11,
#>     final = nov | 12,
#>     .keep = "used"
#>   )
#> #> # A tibble: 336,776 × 3
#> #>   month nov   final
#> #>   <int> <lgl> <lgl>
#> #> 1     1 FALSE TRUE 
#> #> 2     1 FALSE TRUE 
#> #> 3     1 FALSE TRUE 
#> #> 4     1 FALSE TRUE 
#> #> 5     1 FALSE TRUE 
#> #> 6     1 FALSE TRUE 
#> #> # ℹ 336,770 more rows
#> 12.3.3 %in%
#> 
#> An easy way to avoid the problem of getting your ==s and |s in the right order is to use %in%. x %in% y returns a logical vector the same length as x that is TRUE whenever a value in x is anywhere in y .
#> 
#> 1:12 %in% c(1, 5, 11)
#> #>  [1]  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
#> letters[1:10] %in% c("a", "e", "i", "o", "u")
#> #>  [1]  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE
#> 
#> So to find all flights in November and December we could write:
#> 
#> flights |> 
#>   filter(month %in% c(11, 12))
#> 
#> Note that %in% obeys different rules for NA to ==, as NA %in% NA is TRUE.
#> 
#> c(1, 2, NA) == NA
#> #> [1] NA NA NA
#> c(1, 2, NA) %in% NA
#> #> [1] FALSE FALSE  TRUE
#> 
#> This can make for a useful shortcut:
```

<!-- ```{r, eval = FALSE} -->
<!-- # Create a local copy of the rendered book -->
<!-- if (!dir.exists("~/github/hadley/r4ds")) { -->
<!--   # Clone https://r4ds.hadley.nz/base-r locally -->
<!--   system2("git", c("clone", -->
<!--     shQuote("https://github.com/hadley/r4ds/"), -->
<!--     shQuote(normalizePath("~/github/hadley/r4ds")) -->
<!--   )) -->
<!--   pak::local_install_dev_deps("~/github/hadley/r4ds") -->
<!--   system("quarto render ~/github/hadley/r4ds") -->
<!-- } -->
<!-- ``` -->
