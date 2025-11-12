# Markdown documents

`MarkdownDocument` represents a complete Markdown document stored as a
single character string. The constructor normalizes `text` by collapsing
lines and ensuring UTF-8 encoding, so downstream code can rely on a
consistent format.

[`read_as_markdown()`](https://ragnar.tidyverse.org/dev/reference/read_as_markdown.md)
is the recommended way to create a `MarkdownDocument`. The constructor
itself is exported only so advanced users can construct one by other
means when needed.

## Arguments

- text:

  \[string\] Markdown text.

- origin:

  \[string\] Optional source path or URL. Defaults to the `"origin"`
  attribute of `text`, if present, otherwise `NULL`.

## Value

An S7 object that inherits from `MarkdownDocument`, which is a length 1
string of markdown text with an `@origin` property.

## Examples

``` r
md <- MarkdownDocument(
  "# Title\n\nSome text.",
  origin = "example.md"
)
md
#> <ragnar::MarkdownDocument> chr "# Title\n\nSome text."
#>  @ origin: chr "example.md"
```
