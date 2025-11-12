# Segment markdown text

Segment markdown text

## Usage

``` r
markdown_segment(
  text,
  tags = c("h1", "h2", "h3", "h4"),
  trim = FALSE,
  omit_empty = FALSE
)

markdown_frame(text, frame_by = c("h1", "h2", "h3"), segment_by = NULL)
```

## Arguments

- text:

  Markdown string

- tags, segment_by:

  A character vector of html tag names, e.g.,
  `c("h1", "h2", "h3", "pre")`

- trim:

  logical, trim whitespace on segments

- omit_empty:

  logical, whether to remove empty segments

- frame_by:

  Character vector of tags that will become columns in the returned
  dataframe.

## Value

A named character vector. Names will correspond to `tags`, or `""` for
content in between tags.

## Examples

```` r
md <- r"---(

# Sample Markdown File

## Introduction

This is a sample **Markdown** file for testing.

### Features

- Simple **bold** text
- _Italicized_ text
- `Inline code`
- A [link](https://example.com)
- ‘Curly quotes are 3 bytes chars.’ Non-ascii text is fine.

This is a paragraph with <p> tag.

This next segment with code has a <pre> tag

```r
hello_world <- function() {
  cat("Hello, World!\n")
}
```

A table <table>:

  | Name  | Age | City      |
  |-------|----:|-----------|
  | Alice |  25 | New York  |
  | Bob   |  30 | London    |


## Conclusion

Common tags:

- h1, h2, h3, h4, h5, h6: section headings
- p: paragraph (prose)
- pre: pre-formatted text, meant to be displayed with monospace font.
  Typically code or code output
- blockquote: A blockquote
- table: A table
- ul: Unordered list
- ol: Ordered list
- li: Individual list item in a <ul> or <ol>


)---"
markdown_segment(md) |> tibble::enframe()
#> # A tibble: 9 × 2
#>   name  value                                                          
#>   <chr> <chr>                                                          
#> 1 ""    "\n\n"                                                         
#> 2 "h1"  "# Sample Markdown File"                                       
#> 3 ""    "\n\n"                                                         
#> 4 "h2"  "## Introduction"                                              
#> 5 ""    "\n\nThis is a sample **Markdown** file for testing.\n\n"      
#> 6 "h3"  "### Features"                                                 
#> 7 ""    "\n\n- Simple **bold** text\n- _Italicized_ text\n- `Inline co…
#> 8 "h2"  "## Conclusion"                                                
#> 9 ""    "\n\nCommon tags:\n\n- h1, h2, h3, h4, h5, h6: section heading…
markdown_segment(md |> trimws()) |> tibble::enframe()
#> # A tibble: 8 × 2
#>   name  value                                                          
#>   <chr> <chr>                                                          
#> 1 "h1"  "# Sample Markdown File"                                       
#> 2 ""    "\n\n"                                                         
#> 3 "h2"  "## Introduction"                                              
#> 4 ""    "\n\nThis is a sample **Markdown** file for testing.\n\n"      
#> 5 "h3"  "### Features"                                                 
#> 6 ""    "\n\n- Simple **bold** text\n- _Italicized_ text\n- `Inline co…
#> 7 "h2"  "## Conclusion"                                                
#> 8 ""    "\n\nCommon tags:\n\n- h1, h2, h3, h4, h5, h6: section heading…
markdown_segment(md, c("li"), trim = TRUE, omit_empty = TRUE) |> tibble::enframe()
#> # A tibble: 15 × 2
#>    name  value                                                         
#>    <chr> <chr>                                                         
#>  1 ""    "# Sample Markdown File\n\n## Introduction\n\nThis is a sampl…
#>  2 "li"  "- Simple **bold** text"                                      
#>  3 "li"  "- _Italicized_ text"                                         
#>  4 "li"  "- `Inline code`"                                             
#>  5 "li"  "- A [link](https://example.com)"                             
#>  6 "li"  "- ‘Curly quotes are 3 bytes chars.’ Non-ascii text is fine." 
#>  7 ""    "This is a paragraph with <p> tag.\n\nThis next segment with …
#>  8 "li"  "- h1, h2, h3, h4, h5, h6: section headings"                  
#>  9 "li"  "- p: paragraph (prose)"                                      
#> 10 "li"  "- pre: pre-formatted text, meant to be displayed with monosp…
#> 11 "li"  "- blockquote: A blockquote"                                  
#> 12 "li"  "- table: A table"                                            
#> 13 "li"  "- ul: Unordered list"                                        
#> 14 "li"  "- ol: Ordered list"                                          
#> 15 "li"  "- li: Individual list item in a <ul> or <ol>"                
markdown_segment(md, c("table"), trim = TRUE, omit_empty = TRUE) |> tibble::enframe()
#> # A tibble: 3 × 2
#>   name    value                                                        
#>   <chr>   <chr>                                                        
#> 1 ""      "# Sample Markdown File\n\n## Introduction\n\nThis is a samp…
#> 2 "table" "| Name  | Age | City      |\n  |-------|----:|-----------|\…
#> 3 ""      "## Conclusion\n\nCommon tags:\n\n- h1, h2, h3, h4, h5, h6: …
markdown_segment(md, c("ul"), trim = TRUE, omit_empty = TRUE) |> tibble::enframe()
#> # A tibble: 4 × 2
#>   name  value                                                          
#>   <chr> <chr>                                                          
#> 1 ""    "# Sample Markdown File\n\n## Introduction\n\nThis is a sample…
#> 2 "ul"  "- Simple **bold** text\n- _Italicized_ text\n- `Inline code`\…
#> 3 ""    "This is a paragraph with <p> tag.\n\nThis next segment with c…
#> 4 "ul"  "- h1, h2, h3, h4, h5, h6: section headings\n- p: paragraph (p…
````
