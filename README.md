
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
# Create a local copy of the rendered book
if (!dir.exists("~/github/hadley/r4ds")) {
  # Clone https://r4ds.hadley.nz/base-r locally
  system2("git", c("clone",
    shQuote("https://github.com/hadley/r4ds/"),
    shQuote(normalizePath("~/github/hadley/r4ds"))
  ))
  pak::local_install_dev_deps("~/github/hadley/r4ds")
  system("quarto render ~/github/hadley/r4ds")
}
```

``` r
library(ragnar)

store <- ragnar_store_connect(":memory:")

files <- Sys.glob("~/github/hadley/r4ds/_book/*.html")
for (file in files) {
  message("Ingesting: ", file)
  chunks <- file |>
    ragnar_read_document(frame_by_tags = c("h1", "h2")) |>
    ragnar_chunk(max_size = 500, boundaries = c("paragraph", "sentence")) |>
    # augment chunks with context
    dplyr::mutate(text = glue::glue(r"---(
      # Excerpt from *R for Data Science (2e)*
      Chapter: {h1}
      Section: {h2}
      Content: {text}
      )---"))

  chunks <- ragnar_embed_ollama(chunks)
  ragnar_store_insert(store, chunks)
}
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/EDA.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/arrow.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/base-R.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/communicate.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/communication.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/data-import.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/data-tidy.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/data-transform.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/data-visualize.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/databases.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/datetimes.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/factors.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/functions.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/import.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/index.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/intro.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/iteration.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/joins.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/layers.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/logicals.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/missing-values.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/numbers.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/preface-2e.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/program.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/quarto-formats.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/quarto.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/rectangling.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/regexps.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/spreadsheets.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/strings.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/transform.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/visualize.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/webscraping.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/whole-game.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/workflow-basics.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/workflow-help.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/workflow-scripts.html
#> Ingesting: /Users/tomasz/github/hadley/r4ds/_book/workflow-style.html

ragnar_store_build_index(store)
```

### Retrieving Chunks

Once the store is set up, retrieve the most relevant text chunks:

``` r
prompt <- "How can I subset a dataframe with a vector of booleans?"
relevant_chunks <- ragnar_retrieve_vss(store, prompt, top_k = 3)

relevant_chunks$text |> cat(sep = "\n-------\n")
#> # Excerpt from *R for Data Science (2e)*
#> Chapter: 12 Logical vectors
#> Section: 12.3 Boolean algebra
#> Content: Once you have multiple logical vectors, you can combine them together using Boolean algebra. In R, & is “and”, | is “or”, ! is “not”, and xor() is exclusive or2. For example, df |> filter(!is.na(x)) finds all rows where x is not missing and df |> filter(x < -10 | x > 0) finds all rows where x is smaller than -10 or bigger than 0. Figure 12.1 shows the complete set of Boolean operations and how they work.
#> -------
#> # Excerpt from *R for Data Science (2e)*
#> Chapter: 27 A field guide to base R
#> Section: 27.2 Selecting multiple elements with [
#> Content: 27.2.2 Subsetting data frames
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
#> -------
#> # Excerpt from *R for Data Science (2e)*
#> Chapter: 12 Logical vectors
#> Section: 12.3 Boolean algebra
#> Content: df |> 
#>   mutate(
#>     and = x & NA,
#>     or = x | NA
#>   )
#> #> # A tibble: 3 × 3
#> #>   x     and   or   
#> #>   <lgl> <lgl> <lgl>
#> #> 1 TRUE  NA    TRUE 
#> #> 2 FALSE FALSE NA   
#> #> 3 NA    NA    NA
```
