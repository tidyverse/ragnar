# Markdown documents chunks

`MarkdownDocumentChunks` stores information about candidate chunks in a
Markdown document. It is a tibble with three required columns:

- `start`, `end` — integers. These are character positions (1-based,
  inclusive) in the source `MarkdownDocument`, so that
  `substring(md, start, end)` yields the chunk text. Ranges can overlap.

- `context` — character. A general-purpose field for adding context to a
  chunk. This column is combined with `text` to augment chunk content
  when generating embeddings with
  [`ragnar_store_insert()`](https://ragnar.tidyverse.org/reference/ragnar_store_insert.md),
  and is also returned by
  [`ragnar_retrieve()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve.md).
  Keep in mind that when chunks are deoverlapped (in
  [`ragnar_retrieve()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve.md)
  or
  [`chunks_deoverlap()`](https://ragnar.tidyverse.org/reference/chunks_deoverlap.md)),
  only the context value from the first chunk is kept.
  [`markdown_chunk()`](https://ragnar.tidyverse.org/reference/markdown_chunk.md)
  by default populates this column with all the markdown headings that
  are in-scope at the chunk start position.

Additional columns can be included.

The original document is available via the `@document` property.

For normal use, chunk a Markdown document with
[`markdown_chunk()`](https://ragnar.tidyverse.org/reference/markdown_chunk.md);
the class constructor itself is exported only so advanced users can
generate or tweak chunks by other means.

## Arguments

- chunks:

  A data frame containing `start`, `end`, and `context` columns, and
  optionally other columns.

- document:

  A `MarkdownDocument`.

## Value

An S7 object that inherits from `MarkdownDocumentChunks`, which is also
a `tibble`.

## See also

[`MarkdownDocument()`](https://ragnar.tidyverse.org/reference/MarkdownDocument.md)

## Examples

``` r
doc_text <- "# A\n\nB\n\n## C\n\nD" # can be readLines() output, etc.
doc <- MarkdownDocument(doc_text, origin = "some/where")
chunk_positions <- tibble::tibble(
  start = c(1L, 9L),
  end = c(8L, 15L),
  context = c("", "# A"),
  text = substring(doc, start, end)
)
chunks <- MarkdownDocumentChunks(chunk_positions, doc)
identical(chunks@document, doc)
#> [1] TRUE
```
