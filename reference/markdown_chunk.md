# Chunk a Markdown document

`markdown_chunk()` splits a single Markdown string into shorter
optionally overlapping chunks while nudging cut points to the nearest
sensible boundary (heading, paragraph, sentence, line, word, or
character). It returns a tibble recording the character ranges, headings
context, and text for each chunk.

## Usage

``` r
markdown_chunk(
  md,
  target_size = 1600L,
  target_overlap = 0.5,
  ...,
  max_snap_dist = target_size * (1 - target_overlap)/3,
  segment_by_heading_levels = integer(),
  context = TRUE,
  text = TRUE
)
```

## Arguments

- md:

  A `MarkdownDocument`, or a length-one character vector containing
  Markdown.

- target_size:

  Integer. Target chunk size in characters. Default: 1600 (\\\approx\\
  400 tokens, or 1 page of text). Actual chunk size may differ from the
  target by up to `2 * max_snap_dist`. When set to `NULL`, `NA` or `Inf`
  and used with `segment_by_heading_levels`, chunk size is unbounded and
  each chunk corresponds to a segment.

- target_overlap:

  Numeric in `[0, 1)`. Fraction of desired overlap between successive
  chunks. Default: `0.5`. Even when `0`, some overlap can occur because
  the last chunk is anchored to the document end.

- ...:

  These dots are for future extensions and must be empty.

- max_snap_dist:

  Integer. Furthest distance (in characters) a cut point may move to
  reach a semantic boundary. Defaults to one third of the stride size
  between target chunk starts. Chunks that end up on identical
  boundaries are merged.

- segment_by_heading_levels:

  Integer vector with possible values `1:6`. Headings at these levels
  are treated as segment boundaries; chunking is performed independently
  for each segment. No chunk will overlap a segment boundary, and any
  future deoverlapping will not combine segments. Each segment will have
  a chunk that starts at the segment start and a chunk that ends at the
  segment end (these may be the same chunk or overlap substantially if
  the segment is short). Default: disabled.

- context:

  Logical. Add a `context` column containing the Markdown headings in
  scope at each chunk start. Default: `TRUE`.

- text:

  Logical. If `TRUE`, include a `text` column with the chunk contents.
  Default: `TRUE`.

## Value

A
[`MarkdownDocumentChunks`](https://ragnar.tidyverse.org/reference/MarkdownDocumentChunks.md)
object, which is a tibble (data.frame) with with columns `start` `end`,
and optionally `context` and `text`. It also has a `@document` property,
which is the input `md` document (potentially normalized and converted
to a
[`MarkdownDocument`](https://ragnar.tidyverse.org/reference/MarkdownDocument.md)).

## See also

[`ragnar_chunks_view()`](https://ragnar.tidyverse.org/reference/ragnar_chunks_view.md)
to interactively inspect the output of `markdown_chunk()`. See also
[`MarkdownDocumentChunks()`](https://ragnar.tidyverse.org/reference/MarkdownDocumentChunks.md)
and
[`MarkdownDocument()`](https://ragnar.tidyverse.org/reference/MarkdownDocument.md),
where the input and return value of `markdown_chunk()` are described
more fully.

## Examples

``` r
md <- "
# Title

## Section 1

Some text that is long enough to be chunked.

A second paragraph to make the text even longer.

## Section 2

More text here.

### Section 2.1

Some text under a level three heading.

#### Section 2.1.1

Some text under a level four heading.

## Section 3

Even more text here.
"

markdown_chunk(md, target_size = 40)
#> # A tibble: 15 × 4
#>    start   end context                                            text 
#>  * <int> <int> <chr>                                              <chr>
#>  1     1    39 ""                                                 "\n#…
#>  2    25    60 "# Title\n## Section 1"                            "Som…
#>  3    40    79 "# Title\n## Section 1"                            "is …
#>  4    61   101 "# Title\n## Section 1"                            "chu…
#>  5    80   120 "# Title\n## Section 1"                            "par…
#>  6   102   134 "# Title\n## Section 1"                            "tex…
#>  7   121   166 "# Title"                                          "## …
#>  8   135   178 "# Title\n## Section 2"                            "Mor…
#>  9   167   206 "# Title\n## Section 2\n### Section 2.1"           "\n\…
#> 10   179   226 "# Title\n## Section 2\n### Section 2.1"           "und…
#> 11   207   238 "# Title\n## Section 2\n### Section 2.1"           "\n\…
#> 12   227   266 "# Title\n## Section 2\n### Section 2.1\n#### Sec… "\n\…
#> 13   239   281 "# Title\n## Section 2\n### Section 2.1\n#### Sec… "und…
#> 14   267   301 "# Title\n## Section 2\n### Section 2.1\n#### Sec… "\n#…
#> 15   268   302 "# Title"                                          "## …
markdown_chunk(md, target_size = 40, target_overlap = 0)
#> # A tibble: 8 × 4
#>   start   end context                                             text 
#> * <int> <int> <chr>                                               <chr>
#> 1     1    39 ""                                                  "\n#…
#> 2    40    70 "# Title\n## Section 1"                             "is …
#> 3    71   120 "# Title\n## Section 1"                             "A s…
#> 4   121   151 "# Title"                                           "## …
#> 5   152   208 "# Title\n## Section 2"                             "###…
#> 6   209   228 "# Title\n## Section 2\n### Section 2.1"            "###…
#> 7   229   267 "# Title\n## Section 2\n### Section 2.1\n#### Sect… "Som…
#> 8   268   302 "# Title"                                           "## …
markdown_chunk(md, target_size = NA, segment_by_heading_levels = c(1, 2))
#> # A tibble: 5 × 4
#>   start   end context   text                                           
#> * <int> <int> <chr>     <chr>                                          
#> 1     1     1 ""        "\n"                                           
#> 2     2    10 ""        "# Title\n\n"                                  
#> 3    11   120 "# Title" "## Section 1\n\nSome text that is long enough…
#> 4   121   267 "# Title" "## Section 2\n\nMore text here.\n\n### Sectio…
#> 5   268   302 "# Title" "## Section 3\n\nEven more text here.\n"       
markdown_chunk(md, target_size = 40, max_snap_dist = 100)
#> # A tibble: 6 × 4
#>   start   end context                                  text            
#> * <int> <int> <chr>                                    <chr>           
#> 1     1    10 ""                                       "\n# Title\n\n" 
#> 2    11   120 "# Title"                                "## Section 1\n…
#> 3   121   151 "# Title"                                "## Section 2\n…
#> 4   152   208 "# Title\n## Section 2"                  "### Section 2.…
#> 5   209   267 "# Title\n## Section 2\n### Section 2.1" "#### Section 2…
#> 6   268   302 "# Title"                                "## Section 3\n…
```
