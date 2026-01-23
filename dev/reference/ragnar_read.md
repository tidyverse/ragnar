# Read a document as Markdown

**\[deprecated\]**

This function is deprecated in favor of
[`read_as_markdown()`](https://ragnar.tidyverse.org/dev/reference/read_as_markdown.md).

## Usage

``` r
ragnar_read(x, ..., split_by_tags = NULL, frame_by_tags = NULL)
```

## Arguments

- x:

  file path or url.

- ...:

  passed on `markitdown.convert`.

- split_by_tags:

  character vector of html tag names used to split the returned text

- frame_by_tags:

  character vector of html tag names used to create a dataframe of the
  returned content

## Value

Always returns a data frame with the columns:

- `origin`: the file path or url

- `hash`: a hash of the text content

- `text`: the markdown content

If `split_by_tags` is not `NULL`, then a `tag` column is also included
containing the corresponding tag for each text chunk. `""` is used for
text chunks that are not associated with a tag.

If `frame_by_tags` is not `NULL`, then additional columns are included
for each tag in `frame_by_tags`. The text chunks are associated with the
tags in the order they appear in the markdown content.

## Details

`ragnar_read()` uses
[markitdown](https://github.com/microsoft/markitdown) to convert a
document to markdown. If `frame_by_tags` or `split_by_tags` is provided,
the converted markdown content is then split and converted to a data
frame, otherwise, the markdown is returned as a string.

## Examples

``` r
file <- tempfile(fileext = ".html")
download.file("https://r4ds.hadley.nz/base-R.html", file, quiet = TRUE)

# with no arguments, returns a single row data frame.
# the markdown content is in the `text` column.
file |> ragnar_read() |> str()
#> tibble [1 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ origin: chr "/tmp/Rtmpw4ETZK/file1d6b509cea2f.html"
#>  $ hash  : chr "3aa854d11247b3caf1a717c2fbd83da9"
#>  $ text  : <ragnar::MarkdownDocument> chr "# 27  A field guide to base R – R for Data Science (2e)\n\n# 27  A field guide to base R\n\n## 27.1 Introductio"| __truncated__
#>   ..@ origin: chr "/tmp/Rtmpw4ETZK/file1d6b509cea2f.html"

# use `split_by_tags` to get a data frame where the text is split by the
# specified tags (e.g., "h1", "h2", "h3")
file |>
  ragnar_read(split_by_tags = c("h1", "h2", "h3"))
#> # A tibble: 34 × 4
#>    origin                                hash               tag   text 
#>    <chr>                                 <chr>              <chr> <chr>
#>  1 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa854d11247b3caf… "h1"  "# 2…
#>  2 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa854d11247b3caf… "h1"  "# 2…
#>  3 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa854d11247b3caf… "h2"  "## …
#>  4 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa854d11247b3caf… ""    "To …
#>  5 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa854d11247b3caf… "h3"  "###…
#>  6 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa854d11247b3caf… ""    "Thi…
#>  7 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa854d11247b3caf… "h2"  "## …
#>  8 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa854d11247b3caf… ""    "`[`…
#>  9 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa854d11247b3caf… "h3"  "###…
#> 10 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa854d11247b3caf… ""    "The…
#> # ℹ 24 more rows

# use `frame_by_tags` to get a dataframe where the
# headings associated with each text chunk are easily accessible
file |>
  ragnar_read(frame_by_tags = c("h1", "h2", "h3"))
#> # A tibble: 17 × 6
#>    origin                                hash   h1    h2    h3    text 
#>    <chr>                                 <chr>  <chr> <chr> <chr> <chr>
#>  1 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… NA    NA     NA  
#>  2 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… ## 2… NA    "To …
#>  3 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… ## 2… ### … "Thi…
#>  4 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… ## 2… NA    "`[`…
#>  5 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… ## 2… ### … "The…
#>  6 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… ## 2… ### … "The…
#>  7 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… ## 2… ### … "Sev…
#>  8 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… ## 2… ### … "1. …
#>  9 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… ## 2… NA    "`[`…
#> 10 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… ## 2… ### … "`[[…
#> 11 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… ## 2… ### … "The…
#> 12 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… ## 2… ### … "`[[…
#> 13 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… ## 2… ### … "1. …
#> 14 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… ## 2… NA    "In …
#> 15 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… ## 2… NA    "`fo…
#> 16 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… ## 2… NA    "Man…
#> 17 /tmp/Rtmpw4ETZK/file1d6b509cea2f.html 3aa85… # 27… ## 2… NA    "In …

# use `split_by_tags` and `frame_by_tags` together to further break up `text`.
file |>
  ragnar_read(
    split_by_tags = c("p"),
    frame_by_tags = c("h1", "h2", "h3")
  )
#> # A tibble: 120 × 7
#>    origin                           hash  h1    h2    h3    tag   text 
#>    <chr>                            <chr> <chr> <chr> <chr> <chr> <chr>
#>  1 /tmp/Rtmpw4ETZK/file1d6b509cea2… 3aa8… # 27… NA    NA     NA    NA  
#>  2 /tmp/Rtmpw4ETZK/file1d6b509cea2… 3aa8… # 27… ## 2… NA    "p"   "To …
#>  3 /tmp/Rtmpw4ETZK/file1d6b509cea2… 3aa8… # 27… ## 2… NA    "p"   "Thi…
#>  4 /tmp/Rtmpw4ETZK/file1d6b509cea2… 3aa8… # 27… ## 2… NA    "p"   "Aft…
#>  5 /tmp/Rtmpw4ETZK/file1d6b509cea2… 3aa8… # 27… ## 2… NA    "p"   "In …
#>  6 /tmp/Rtmpw4ETZK/file1d6b509cea2… 3aa8… # 27… ## 2… ### … "p"   "Thi…
#>  7 /tmp/Rtmpw4ETZK/file1d6b509cea2… 3aa8… # 27… ## 2… ### … ""    "```…
#>  8 /tmp/Rtmpw4ETZK/file1d6b509cea2… 3aa8… # 27… ## 2… NA    "p"   "`[`…
#>  9 /tmp/Rtmpw4ETZK/file1d6b509cea2… 3aa8… # 27… ## 2… ### … "p"   "The…
#> 10 /tmp/Rtmpw4ETZK/file1d6b509cea2… 3aa8… # 27… ## 2… ### … ""    "1." 
#> # ℹ 110 more rows

# Example workflow adding context to each chunk
file |>
  ragnar_read(frame_by_tags = c("h1", "h2", "h3")) |>
  glue::glue_data(r"--(
    ## Excerpt from the book "R for Data Science (2e)"
    chapter: {h1}
    section: {h2}
    content: {text}

    )--") |>
  # inspect
  _[6:7] |> cat(sep = "\n~~~~~~~~~~~\n")
#> ## Excerpt from the book "R for Data Science (2e)"
#> chapter: # 27  A field guide to base R
#> section: ## 27.2 Selecting multiple elements with `[`
#> content: There are quite a few different ways[1](#fn1) that you can use `[` with a data frame, but the most important way is to select rows and columns independently with `df[rows, cols]`. Here `rows` and `cols` are vectors as described above. For example, `df[rows, ]` and `df[, cols]` select just rows or just columns, using the empty subset to preserve the other dimension.
#> 
#> Here are a couple of examples:
#> 
#> ```r
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
#> There’s an important difference between tibbles and data frames when it comes to `[`. In this book, we’ve mainly used tibbles, which *are* data frames, but they tweak some behaviors to make your life a little easier. In most places, you can use “tibble” and “data frame” interchangeably, so when we want to draw particular attention to R’s built-in data frame, we’ll write `data.frame`. If `df` is a `data.frame`, then `df[, cols]` will return a vector if `col` selects a single column and a data frame if it selects more than one column. If `df` is a tibble, then `[` will always return a tibble.
#> 
#> ```r
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
#> ```r
#> df1[, "x" , drop = FALSE]
#> #>   x
#> #> 1 1
#> #> 2 2
#> #> 3 3
#> ```
#> 
#> ~~~~~~~~~~~
#> ## Excerpt from the book "R for Data Science (2e)"
#> chapter: # 27  A field guide to base R
#> section: ## 27.2 Selecting multiple elements with `[`
#> content: Several dplyr verbs are special cases of `[`:
#> 
#> * `[filter()](https://dplyr.tidyverse.org/reference/filter.html)` is equivalent to subsetting the rows with a logical vector, taking care to exclude missing values:
#> 
#>   ```r
#>   df <- tibble(
#>     x = c(2, 3, 1, 1, NA),
#>     y = letters[1:5],
#>     z = runif(5)
#>   )
#>   df |> filter(x > 1)
#> 
#>   # same as
#>   df[!is.na(df$x) & df$x > 1, ]
#>   ```
#> 
#>   Another common technique in the wild is to use `[which()](https://rdrr.io/r/base/which.html)` for its side-effect of dropping missing values: `df[which(df$x > 1), ]`.
#> * `[arrange()](https://dplyr.tidyverse.org/reference/arrange.html)` is equivalent to subsetting the rows with an integer vector, usually created with `[order()](https://rdrr.io/r/base/order.html)`:
#> 
#>   ```r
#>   df |> arrange(x, y)
#> 
#>   # same as
#>   df[order(df$x, df$y), ]
#>   ```
#> 
#>   You can use `order(decreasing = TRUE)` to sort all columns in descending order or `-rank(col)` to sort columns in decreasing order individually.
#> * Both `[select()](https://dplyr.tidyverse.org/reference/select.html)` and `[relocate()](https://dplyr.tidyverse.org/reference/relocate.html)` are similar to subsetting the columns with a character vector:
#> 
#>   ```r
#>   df |> select(x, z)
#> 
#>   # same as
#>   df[, c("x", "z")]
#>   ```
#> 
#> Base R also provides a function that combines the features of `[filter()](https://dplyr.tidyverse.org/reference/filter.html)` and `[select()](https://dplyr.tidyverse.org/reference/select.html)`[2](#fn2) called `[subset()](https://rdrr.io/r/base/subset.html)`:
#> 
#> ```r
#> df |>
#>   filter(x > 1) |>
#>   select(y, z)
#> #> # A tibble: 2 × 2
#> #>   y           z
#> #>   <chr>   <dbl>
#> #> 1 a     0.157
#> #> 2 b     0.00740
#> ```
#> 
#> ```r
#> # same as
#> df |> subset(x > 1, c(y, z))
#> ```
#> 
#> This function was the inspiration for much of dplyr’s syntax.
#> 

# Advanced example of postprocessing the output of ragnar_read()
# to add language to code blocks, markdown style
library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(rvest)
library(xml2)
file |>
  ragnar_read(frame_by_tags = c("h1", "h2", "h3"),
              split_by_tags = c("p", "pre")) |>
  mutate(
    is_code = tag == "pre",
    text = ifelse(is_code, str_replace(text, "```", "```r"), text)
  ) |>
  group_by(h1, h2, h3) |>
  summarise(text = str_flatten(text, "\n\n"), .groups = "drop") |>
  glue::glue_data(r"--(
    # Excerpt from the book "R for Data Science (2e)"
    chapter: {h1}
    section: {h2}
    content: {text}

    )--") |>
  # inspect
  _[9:10] |> cat(sep = "\n~~~~~~~~~~~\n")
#> # Excerpt from the book "R for Data Science (2e)"
#> chapter: # 27  A field guide to base R
#> section: ## 27.3 Selecting a single element with `$` and `[[`
#> content: There are a couple of important differences between tibbles and base `data.frame`s when it comes to `$`. Data frames match the prefix of any variable names (so-called **partial matching**) and don’t complain if a column doesn’t exist:
#> 
#> ```rr
#> df <- data.frame(x1 = 1)
#> df$x
#> #> [1] 1
#> df$z
#> #> NULL
#> ```
#> 
#> Tibbles are more strict: they only ever match variable names exactly and they will generate a warning if the column you are trying to access doesn’t exist:
#> 
#> ```rr
#> tb <- tibble(x1 = 1)
#> 
#> tb$x
#> #> Warning: Unknown or uninitialised column: `x`.
#> #> NULL
#> tb$z
#> #> Warning: Unknown or uninitialised column: `z`.
#> #> NULL
#> ```
#> 
#> For this reason we sometimes joke that tibbles are lazy and surly: they do less and complain more.
#> 
#> ~~~~~~~~~~~
#> # Excerpt from the book "R for Data Science (2e)"
#> chapter: # 27  A field guide to base R
#> section: ## 27.3 Selecting a single element with `$` and `[[`
#> content: `[[` and `$` are also really important for working with lists, and it’s important to understand how they differ from `[`. Let’s illustrate the differences with a list named `l`:
#> 
#> ```rr
#> l <- list(
#>   a = 1:3,
#>   b = "a string",
#>   c = pi,
#>   d = list(-1, -5)
#> )
#> ```
#> 
#> *
#> 
#> `[` extracts a sub-list. It doesn’t matter how many elements you extract, the result will always be a list.
#> 
#> ```rr
#>   str(l[1:2])
#>   #> List of 2
#>   #>  $ a: int [1:3] 1 2 3
#>   #>  $ b: chr "a string"
#> 
#>   str(l[1])
#>   #> List of 1
#>   #>  $ a: int [1:3] 1 2 3
#> 
#>   str(l[4])
#>   #> List of 1
#>   #>  $ d:List of 2
#>   #>   ..$ : num -1
#>   #>   ..$ : num -5
#>   ```
#> 
#> Like with vectors, you can subset with a logical, integer, or character vector.
#> 
#> *
#> 
#> `[[` and `$` extract a single component from a list. They remove a level of hierarchy from the list.
#> 
#> ```rr
#>   str(l[[1]])
#>   #>  int [1:3] 1 2 3
#> 
#>   str(l[[4]])
#>   #> List of 2
#>   #>  $ : num -1
#>   #>  $ : num -5
#> 
#>   str(l$a)
#>   #>  int [1:3] 1 2 3
#>   ```
#> 
#> The difference between `[` and `[[` is particularly important for lists because `[[` drills down into the list while `[` returns a new, smaller list. To help you remember the difference, take a look at the unusual pepper shaker shown in [Figure 27.1](#fig-pepper). If this pepper shaker is your list `pepper`, then, `pepper[1]` is a pepper shaker containing a single pepper packet. `pepper[2]` would look the same, but would contain the second packet. `pepper[1:2]` would be a pepper shaker containing two pepper packets. `pepper[[1]]` would extract the pepper packet itself.
#> 
#> ![Three photos. On the left is a photo of a glass pepper shaker. Instead of  the pepper shaker containing pepper, it contains a single packet of pepper. In the middle is a photo of a single packet of pepper. On the right is a  photo of the contents of a packet of pepper.](diagrams/pepper.png)
#> 
#> Figure 27.1: (Left) A pepper shaker that Hadley once found in his hotel room. (Middle) `pepper[1]`. (Right) `pepper[[1]]`
#> 
#> This same principle applies when you use 1d `[` with a data frame: `df["x"]` returns a one-column data frame and `df[["x"]]` returns a vector.
#> 
```
