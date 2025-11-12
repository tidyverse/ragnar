# Read an HTML document

Read an HTML document

## Usage

``` r
ragnar_read_document(
  x,
  ...,
  split_by_tags = frame_by_tags,
  frame_by_tags = NULL
)
```

## Arguments

- x:

  file path or url, passed on to
  [`rvest::read_html()`](http://xml2.r-lib.org/reference/read_xml.md),
  or an `xml_node`.

- ...:

  passed on to
  [`rvest::read_html()`](http://xml2.r-lib.org/reference/read_xml.md)

- split_by_tags:

  character vector of html tag names used to split the returned text

- frame_by_tags:

  character vector of html tag names used to create a dataframe of the
  returned content

## Value

If `frame_by_tags` is not `NULL`, then a data frame is returned, with
column names `c("frame_by_tags", "text")`.

If `frame_by_tags` is `NULL` but `split_by_tags` is not `NULL`, then a
named character vector is returned.

If both `frame_by_tags` and `split_by_tags` are `NULL`, then a string
(length-1 character vector) is returned.

## Examples

``` r
file <- tempfile(fileext = ".html")
download.file("https://r4ds.hadley.nz/base-R.html", file, quiet = TRUE)

# with no arguments, returns a single string of the text.
file |> ragnar_read_document() |> str()
#>  chr "Program\n27 A field guide to base R\nR for Data Science (2e)\nWelcome\nPreface to the second edition\nIntroduct"| __truncated__

# use `split_by_tags` to get a named character vector of length > 1
file |>
  ragnar_read_document(split_by_tags = c("h1", "h2", "h3")) |>
  tibble::enframe("tag", "text")
#> # A tibble: 36 × 2
#>    tag   text                                                          
#>    <chr> <chr>                                                         
#>  1 ""    "Program\n27 A field guide to base R\nR for Data Science (2e)…
#>  2 "h2"  "Table of contents"                                           
#>  3 ""    "27.1 Introduction\n27.1.1 Prerequisites\n27.2 Selecting mult…
#>  4 "h1"  "27 A field guide to base R"                                  
#>  5 "h2"  "27.1 Introduction"                                           
#>  6 ""    "To finish off the programming section, we’re going to give y…
#>  7 "h3"  "27.1.1 Prerequisites"                                        
#>  8 ""    "This package focuses on base R so doesn’t have any real prer…
#>  9 "h2"  "27.2 Selecting multiple elements with ["                     
#> 10 ""    "[ is used to extract sub-components from vectors and data fr…
#> # ℹ 26 more rows

# use `frame_by_tags` to get a dataframe where the
# headings associated with each text chunk are easily accessible
file |>
  ragnar_read_document(frame_by_tags = c("h1", "h2", "h3"))
#> # A tibble: 18 × 4
#>    h1                         h2                            h3    text 
#>    <chr>                      <chr>                         <chr> <chr>
#>  1 NA                         NA                            NA    "Pro…
#>  2 NA                         Table of contents             NA    "27.…
#>  3 27 A field guide to base R 27.1 Introduction             NA    "To …
#>  4 27 A field guide to base R 27.1 Introduction             27.1… "Thi…
#>  5 27 A field guide to base R 27.2 Selecting multiple elem… NA    "[ i…
#>  6 27 A field guide to base R 27.2 Selecting multiple elem… 27.2… "The…
#>  7 27 A field guide to base R 27.2 Selecting multiple elem… 27.2… "The…
#>  8 27 A field guide to base R 27.2 Selecting multiple elem… 27.2… "Sev…
#>  9 27 A field guide to base R 27.2 Selecting multiple elem… 27.2… "Cre…
#> 10 27 A field guide to base R 27.3 Selecting a single elem… NA    "[, …
#> 11 27 A field guide to base R 27.3 Selecting a single elem… 27.3… "[[ …
#> 12 27 A field guide to base R 27.3 Selecting a single elem… 27.3… "The…
#> 13 27 A field guide to base R 27.3 Selecting a single elem… 27.3… "[[ …
#> 14 27 A field guide to base R 27.3 Selecting a single elem… 27.3… "Wha…
#> 15 27 A field guide to base R 27.4 Apply family             NA    "In …
#> 16 27 A field guide to base R 27.5 for loops                NA    "for…
#> 17 27 A field guide to base R 27.6 Plots                    NA    "Man…
#> 18 27 A field guide to base R 27.7 Summary                  NA    "In …

# use `split_by_tags` and `frame_by_tags` together to further break up `text`.
file |>
  ragnar_read_document(
    split_by_tags = c("p"),
    frame_by_tags = c("h1", "h2", "h3")
  )
#> # A tibble: 117 × 5
#>    h1                         h2                      h3    tag   text 
#>    <chr>                      <chr>                   <chr> <chr> <chr>
#>  1 NA                         NA                      NA    ""    "Pro…
#>  2 NA                         Table of contents       NA    ""    "27.…
#>  3 27 A field guide to base R 27.1 Introduction       NA    "p"   "To …
#>  4 27 A field guide to base R 27.1 Introduction       NA    "p"   "Thi…
#>  5 27 A field guide to base R 27.1 Introduction       NA    "p"   "Aft…
#>  6 27 A field guide to base R 27.1 Introduction       NA    "p"   "In …
#>  7 27 A field guide to base R 27.1 Introduction       27.1… "p"   "Thi…
#>  8 27 A field guide to base R 27.1 Introduction       27.1… ""    "lib…
#>  9 27 A field guide to base R 27.2 Selecting multipl… NA    "p"   "[ i…
#> 10 27 A field guide to base R 27.2 Selecting multipl… 27.2… "p"   "The…
#> # ℹ 107 more rows

# Example workflow adding context to each chunk
file |>
  ragnar_read_document(frame_by_tags = c("h1", "h2", "h3")) |>
  glue::glue_data(r"--(
    ## Excerpt from the book "R for Data Science (2e)"
    chapter: {h1}
    section: {h2}
    content: {text}

    )--") |>
    # inspect
    _[6:7] |> cat(sep = "\n~~~~~~~~~~~\n")
#> ## Excerpt from the book "R for Data Science (2e)"
#> chapter: 27 A field guide to base R
#> section: 27.2 Selecting multiple elements with [
#> content: There are five main types of things that you can subset a vector with, i.e., that can be the i in x[i]:
#> 
#> A vector of positive integers. Subsetting with positive integers keeps the elements at those positions:
#> 
#> x <- c("one", "two", "three", "four", "five")
#> x[c(3, 2, 5)]
#> #> [1] "three" "two"   "five"
#> 
#> By repeating a position, you can actually make a longer output than input, making the term “subsetting” a bit of a misnomer.
#> 
#> x[c(1, 1, 5, 5, 5, 2)]
#> #> [1] "one"  "one"  "five" "five" "five" "two"
#> 
#> A vector of negative integers. Negative values drop the elements at the specified positions:
#> 
#> x[c(-1, -3, -5)]
#> #> [1] "two"  "four"
#> 
#> A logical vector. Subsetting with a logical vector keeps all values corresponding to a TRUE value. This is most often useful in conjunction with the comparison functions.
#> 
#> x <- c(10, 3, NA, 5, 8, 1, NA)
#> 
#> # All non-missing values of x
#> x[!is.na(x)]
#> #> [1] 10  3  5  8  1
#> 
#> # All even (or missing!) values of x
#> x[x %% 2 == 0]
#> #> [1] 10 NA  8 NA
#> 
#> Unlike filter(), NA indices will be included in the output as NAs.
#> 
#> A character vector. If you have a named vector, you can subset it with a character vector:
#> 
#> x <- c(abc = 1, def = 2, xyz = 5)
#> x[c("xyz", "def")]
#> #> xyz def 
#> #>   5   2
#> 
#> As with subsetting with positive integers, you can use a character vector to duplicate individual entries.
#> 
#> Nothing. The final type of subsetting is nothing, x[], which returns the complete x. This is not useful for subsetting vectors, but as we’ll see shortly, it is useful when subsetting 2d structures like tibbles.
#> 
#> ~~~~~~~~~~~
#> ## Excerpt from the book "R for Data Science (2e)"
#> chapter: 27 A field guide to base R
#> section: 27.2 Selecting multiple elements with [
#> content: There are quite a few different ways1 that you can use [ with a data frame, but the most important way is to select rows and columns independently with df[rows, cols]. Here rows and cols are vectors as described above. For example, df[rows, ] and df[, cols] select just rows or just columns, using the empty subset to preserve the other dimension.
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
#> There’s an important difference between tibbles and data frames when it comes to [. In this book, we’ve mainly used tibbles, which are data frames, but they tweak some behaviors to make your life a little easier. In most places, you can use “tibble” and “data frame” interchangeably, so when we want to draw particular attention to R’s built-in data frame, we’ll write data.frame. If df is a data.frame, then df[, cols] will return a vector if col selects a single column and a data frame if it selects more than one column. If df is a tibble, then [ will always return a tibble.
#> 
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
#> 
#> One way to avoid this ambiguity with data.frames is to explicitly specify drop = FALSE:
#> 
#> df1[, "x" , drop = FALSE]
#> #>   x
#> #> 1 1
#> #> 2 2
#> #> 3 3
#> 

# Advanced example of postprocessing the output of ragnar_read_document()
# to wrap code blocks in backticks, markdown style
library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(rvest)
library(xml2)
file |>
  ragnar_read_document(frame_by_tags = c("h1", "h2", "h3"),
                       split_by_tags = c("p", "pre")) |>
  mutate(
    is_code = tag == "pre",
    text = ifelse(is_code,
                  str_c("```", text, "```", sep = "\n"),
                  text)) |>
  group_by(h1, h2, h3) |>
  summarise(text = str_flatten(text, "\n"), .groups = "drop") |>
  glue::glue_data(r"--(
    # Excerpt from the book "R for Data Science (2e)"
    chapter: {h1}
    section: {h2}
    content: {text}

    )--") |>
    # inspect
    _[9:10] |> cat(sep = "\n~~~~~~~~~~~\n")
#> # Excerpt from the book "R for Data Science (2e)"
#> chapter: 27 A field guide to base R
#> section: 27.3 Selecting a single element with $ and [[
#> content: There are a couple of important differences between tibbles and base data.frames when it comes to $. Data frames match the prefix of any variable names (so-called partial matching) and don’t complain if a column doesn’t exist:
#> ```
#> df <- data.frame(x1 = 1)
#> df$x
#> #> [1] 1
#> df$z
#> #> NULL
#> ```
#> Tibbles are more strict: they only ever match variable names exactly and they will generate a warning if the column you are trying to access doesn’t exist:
#> ```
#> tb <- tibble(x1 = 1)
#> 
#> tb$x
#> #> Warning: Unknown or uninitialised column: `x`.
#> #> NULL
#> tb$z
#> #> Warning: Unknown or uninitialised column: `z`.
#> #> NULL
#> ```
#> For this reason we sometimes joke that tibbles are lazy and surly: they do less and complain more.
#> 
#> ~~~~~~~~~~~
#> # Excerpt from the book "R for Data Science (2e)"
#> chapter: 27 A field guide to base R
#> section: 27.3 Selecting a single element with $ and [[
#> content: [[ and $ are also really important for working with lists, and it’s important to understand how they differ from [. Let’s illustrate the differences with a list named l:
#> ```
#> l <- list(
#>   a = 1:3, 
#>   b = "a string", 
#>   c = pi, 
#>   d = list(-1, -5)
#> )
#> ```
#> [ extracts a sub-list. It doesn’t matter how many elements you extract, the result will always be a list.
#> ```
#> str(l[1:2])
#> #> List of 2
#> #>  $ a: int [1:3] 1 2 3
#> #>  $ b: chr "a string"
#> 
#> str(l[1])
#> #> List of 1
#> #>  $ a: int [1:3] 1 2 3
#> 
#> str(l[4])
#> #> List of 1
#> #>  $ d:List of 2
#> #>   ..$ : num -1
#> #>   ..$ : num -5
#> ```
#> Like with vectors, you can subset with a logical, integer, or character vector.
#> [[ and $ extract a single component from a list. They remove a level of hierarchy from the list.
#> ```
#> str(l[[1]])
#> #>  int [1:3] 1 2 3
#> 
#> str(l[[4]])
#> #> List of 2
#> #>  $ : num -1
#> #>  $ : num -5
#> 
#> str(l$a)
#> #>  int [1:3] 1 2 3
#> ```
#> The difference between [ and [[ is particularly important for lists because [[ drills down into the list while [ returns a new, smaller list. To help you remember the difference, take a look at the unusual pepper shaker shown in Figure 27.1. If this pepper shaker is your list pepper, then, pepper[1] is a pepper shaker containing a single pepper packet. pepper[2] would look the same, but would contain the second packet. pepper[1:2] would be a pepper shaker containing two pepper packets. pepper[[1]] would extract the pepper packet itself.
#> Figure 27.1: (Left) A pepper shaker that Hadley once found in his hotel room. (Middle) pepper[1]. (Right) pepper[[1]]
#> This same principle applies when you use 1d [ with a data frame: df["x"] returns a one-column data frame and df[["x"]] returns a vector.
#> 

# Example of preprocessing the input to ragnar_read_document()
# to wrap code in backticks, markdown style
# same outcome as above, except via pre processing instead of post processing.
file |>
  read_html() |>
  (\(doc) {
    # fence preformatted code with triple backticks
    for (node in html_elements(doc, "pre")) {
      xml_add_child(node, "code", "```\n", .where = 0)
      xml_add_child(node, "code", "\n```")
    }
    # wrap inline code with single backticks
    for (node in html_elements(doc, "code")) {
      if (!"pre" %in% xml_name(xml_parents(node))) {
        xml_text(node) <- str_c("`", xml_text(node), "`")
      }
    }
    doc
  })() |>
  ragnar_read_document(frame_by_tags = c("h1", "h2", "h3")) |>
  glue::glue_data(r"--(
    # Excerpt from the book "R for Data Science (2e)"
    chapter: {h1}
    section: {h2}
    content: {text}

    )--") |> _[6]
#> # Excerpt from the book "R for Data Science (2e)"
#> chapter: 27 A field guide to base R
#> section: 27.2 Selecting multiple elements with `[`
#> content: There are five main types of things that you can subset a vector with, i.e., that can be the `i` in `x[i]`:
#> 
#> A vector of positive integers. Subsetting with positive integers keeps the elements at those positions:
#> 
#> ```
#> x <- c("one", "two", "three", "four", "five")
#> x[c(3, 2, 5)]
#> #> [1] "three" "two"   "five"
#> ```
#> 
#> By repeating a position, you can actually make a longer output than input, making the term “subsetting” a bit of a misnomer.
#> 
#> ```
#> x[c(1, 1, 5, 5, 5, 2)]
#> #> [1] "one"  "one"  "five" "five" "five" "two"
#> ```
#> 
#> A vector of negative integers. Negative values drop the elements at the specified positions:
#> 
#> ```
#> x[c(-1, -3, -5)]
#> #> [1] "two"  "four"
#> ```
#> 
#> A logical vector. Subsetting with a logical vector keeps all values corresponding to a `TRUE` value. This is most often useful in conjunction with the comparison functions.
#> 
#> ```
#> x <- c(10, 3, NA, 5, 8, 1, NA)
#> 
#> # All non-missing values of x
#> x[!is.na(x)]
#> #> [1] 10  3  5  8  1
#> 
#> # All even (or missing!) values of x
#> x[x %% 2 == 0]
#> #> [1] 10 NA  8 NA
#> ```
#> 
#> Unlike `filter()`, `NA` indices will be included in the output as `NA`s.
#> 
#> A character vector. If you have a named vector, you can subset it with a character vector:
#> 
#> ```
#> x <- c(abc = 1, def = 2, xyz = 5)
#> x[c("xyz", "def")]
#> #> xyz def 
#> #>   5   2
#> ```
#> 
#> As with subsetting with positive integers, you can use a character vector to duplicate individual entries.
#> 
#> Nothing. The final type of subsetting is nothing, `x[]`, which returns the complete `x`. This is not useful for subsetting vectors, but as we’ll see shortly, it is useful when subsetting 2d structures like tibbles.
#> 
```
