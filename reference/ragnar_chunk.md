# Chunk text

**\[deprecated\]**

These functions are deprecated in favor of
[`markdown_chunk()`](https://ragnar.tidyverse.org/reference/markdown_chunk.md),
which is more flexible, supports overlapping chunks, enables
deoverlapping or rechunking downstream by
[`ragnar_retrieve()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve.md),
and automatically builds a `context` string of in-scope markdown
headings for each chunk instead of requiring manual string interpolation
from extracted headings.

## Usage

``` r
ragnar_chunk(
  x,
  max_size = 1600L,
  boundaries = c("paragraph", "sentence", "line_break", "word", "character"),
  ...,
  trim = TRUE,
  simplify = TRUE
)

ragnar_segment(x, boundaries = "sentence", ..., trim = FALSE, simplify = TRUE)

ragnar_chunk_segments(x, max_size = 1600L, ..., simplify = TRUE, trim = TRUE)
```

## Arguments

- x:

  A character vector, list of character vectors, or data frame
  containing a `text` column.

- max_size:

  Integer. The maximum number of characters in each chunk. Defaults to
  `1600`, which typically is approximately 400 tokens, or 1 page of
  text.

- boundaries:

  A sequence of boundary types to use in order until `max_size` is
  satisfied. Valid values are `"sentence"`, `"word"`, `"line_break"`,
  `"character"`, `"paragraph"`, or a `stringr_pattern` object like
  [`stringr::fixed()`](https://stringr.tidyverse.org/reference/modifiers.html).

- ...:

  Additional arguments passed to internal functions. tokenizer to use
  `tokens` instead of characters as the count (not fully implemented
  yet)

- trim:

  logical, whether to trim leading and trailing whitespace from strings.
  Default `TRUE`.

- simplify:

  Logical. If `TRUE`, the output is simplified. If `FALSE`, returns a
  vector that has the same length as `x`. If `TRUE`, character strings
  are [`unlist()`](https://rdrr.io/r/base/unlist.html)ed, and dataframes
  are
  [`tidyr::unchop()`](https://tidyr.tidyverse.org/reference/chop.html)ed.

## Value

- For character input with `simplify = FALSE`: A list of character
  vectors

- For character input with `simplify = TRUE`: A character vector of
  chunks

- For data frame input with `simplify = FALSE`: A data frame with the
  same number of rows as the input, where the `text` column transformed
  into a list of chararacter vectors.

- For data frame input with `simplify = TRUE`: Same as a data frame
  input with `simplify=FALSE`, with the `text` column expanded by
  [`tidyr::unchop()`](https://tidyr.tidyverse.org/reference/chop.html)

## Details

Functions for chunking text into smaller pieces while preserving
meaningful semantics. These functions provide flexible ways to split
text based on various boundaries (sentences, words, etc.) while
controlling chunk sizes and overlap.

Chunking is the combination of two fundamental operations:

- identifying boundaries: finding character positions where it makes
  sense to split a string.

- extracting slices: extracting substrings using the candidate
  boundaries to produce chunks that match the requested `chunk_size` and
  `chunk_overlap`

`ragnar_chunk()` is a higher-level function that does both, identifies
boundaries and extracts slices.

If you need lower-level control, you can alternatively use the
lower-level functions `ragnar_segment()` in combination with
`ragnar_chunk_segments()`.

`ragnar_segment()`: Splits text at semantic boundaries.

`ragnar_chunk_segments()`: Combines text segments into chunks.

For most usecases, these two are equivalent:

    x |> ragnar_chunk()
    x |> ragnar_segment() |> ragnar_chunk_segments()

When working with data frames, these functions preserve all columns and
use [`tidyr::unchop()`](https://tidyr.tidyverse.org/reference/chop.html)
to handle the resulting list-columns when `simplify = TRUE`.

## Examples

``` r
# Basic chunking with max size
text <- "This is a long piece of text. It has multiple sentences.
         We want to split it into chunks. Here's another sentence."
ragnar_chunk(text, max_size = 40) # splits at sentences
#> [1] "This is a long piece of text."   
#> [2] "It has multiple sentences."      
#> [3] "We want to split it into chunks."
#> [4] "Here's another sentence."        

# smaller chunk size: first splits at sentence boundaries, then word boundaries
ragnar_chunk(text, max_size = 20)
#> [1] "This is a long piece" "of text."            
#> [3] "It has multiple"      "sentences."          
#> [5] "We want to split it"  "into chunks."        
#> [7] "Here's another"       "sentence."           

# only split at sentence boundaries. Note, some chunks are oversized
ragnar_chunk(text, max_size = 20, boundaries = c("sentence"))
#> [1] "This is a long piece of text."   
#> [2] "It has multiple sentences."      
#> [3] "We want to split it into chunks."
#> [4] "Here's another sentence."        

# only consider word boundaries when splitting:
ragnar_chunk(text, max_size = 20, boundaries = c("word"))
#> [1] "This is a long piece" "of text. It has"     
#> [3] "multiple sentences."  "We want to"          
#> [5] "split it into chunks" "s. Here's another"   
#> [7] "sentence."           

# first split at sentence boundaries, then word boundaries,
# as needed to satisfy `max_chunk`
ragnar_chunk(text, max_size = 20, boundaries = c("sentence", "word"))
#> [1] "This is a long piece" "of text."            
#> [3] "It has multiple"      "sentences."          
#> [5] "We want to split it"  "into chunks."        
#> [7] "Here's another"       "sentence."           

# Use a stringr pattern to find semantic boundaries
ragnar_chunk(text, max_size = 10, boundaries = stringr::fixed(". "))
#> [1] "This is a long piece of text."                                        
#> [2] "It has multiple sentences.\n         We want to split it into chunks."
#> [3] "Here's another sentence."                                             
ragnar_chunk(text, max_size = 10, boundaries = list(stringr::fixed(". "), "word"))
#>  [1] "This is a"  "long piece" "e of text." "It has"     "multiple"  
#>  [6] "sentences." "."          "We want to" "o split it" "into"      
#> [11] "chunks."    "Here's"     "another"    "sentence." 


# Working with data frames
df <- data.frame(
  id = 1:2,
  text = c("First sentence. Second sentence.", "Another sentence here.")
)
ragnar_chunk(df, max_size = 20, boundaries = "sentence")
#>   id                   text
#> 1  1        First sentence.
#> 2  1       Second sentence.
#> 3  2 Another sentence here.
ragnar_chunk(df$text, max_size = 20, boundaries = "sentence")
#> [1] "First sentence."        "Second sentence."      
#> [3] "Another sentence here."

# Chunking pre-segmented text
segments <- c("First segment. ", "Second segment. ", "Third segment. ", "Fourth segment. ")
ragnar_chunk_segments(segments, max_size = 20)
#> [1] "First segment."  "Second segment." "Third segment." 
#> [4] "Fourth segment."
ragnar_chunk_segments(segments, max_size = 40)
#> [1] "First segment. Second segment." "Third segment. Fourth segment."
ragnar_chunk_segments(segments, max_size = 60)
#> [1] "First segment. Second segment. Third segment."
#> [2] "Fourth segment."                              
```
