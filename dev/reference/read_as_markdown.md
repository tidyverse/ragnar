# Convert files to Markdown

Convert files to Markdown

## Usage

``` r
read_as_markdown(
  path,
  ...,
  origin = path,
  html_extract_selectors = c("main"),
  html_zap_selectors = c("nav"),
  youtube_transcript_formatter = NULL
)
```

## Arguments

- path:

  \[string\] A filepath or URL. Accepts a wide variety of file types,
  including plain text (markdown), PDF, PowerPoint, Word, Excel, images
  (EXIF metadata and OCR), audio (EXIF metadata and speech
  transcription), HTML, text-based formats (CSV, JSON, XML), ZIP files
  (iterates over contents), YouTube URLs, and EPUBs.

- ...:

  Passed on to `MarkItDown.convert()`.

- origin:

  The value to use for the `@origin` property of the returned
  `MarkdownDocument`.

- html_extract_selectors:

  Character vector of CSS selectors. If a match for a selector is found
  in the document, only the matched node's contents are converted.
  Unmatched extract selectors have no effect.

- html_zap_selectors:

  Character vector of CSS selectors. Elements matching these selectors
  will be excluded ("zapped") from the HTML document before conversion
  to markdown. This is useful for removing navigation bars, sidebars,
  headers, footers, or other unwanted elements. By default, navigation
  elements (`nav`) are excluded.

- youtube_transcript_formatter:

  A function used to customize how YouTube transcript data is converted
  to markdown. It receives a tibble/data.frame with columns `text`
  (chr), `start` (dbl, seconds), and `duration` (dbl, seconds), along
  with a `"youtube_metadata"` attribute, a named list containing
  elements `language`, `language_code`, `video_id`, and `is_generated`.
  The formatter must return a single string; by default it behaves like
  `\(transcript) paste0(transcript$text, collapse = " ")`. Provide a
  custom formatter to include timestamps or links (see examples).

## Value

A
[`MarkdownDocument`](https://ragnar.tidyverse.org/dev/reference/MarkdownDocument.md)
object, which is a single string of Markdown with an `@origin` property.

## Details

### Converting HTML

When converting HTML, you might want to omit certain elements, like
sidebars, headers, footers, etc. You can pass CSS selector strings to
either extract nodes or exclude nodes during conversion.

The easiest way to make selectors is to use SelectorGadget:
<https://rvest.tidyverse.org/articles/selectorgadget.html>

You can also right-click on a page and select "Inspect Element" in a
browser to better understand an HTML page's structure.

For comprehensive or advanced usage of CSS selectors, consult
<https://www.crummy.com/software/BeautifulSoup/bs4/doc/#css-selectors-through-the-css-property>
and <https://facelessuser.github.io/soupsieve/selectors/>

## Examples

``` r
# \dontrun{
# Convert HTML
md <- read_as_markdown("https://r4ds.hadley.nz/base-R.html")
md
#> <ragnar::MarkdownDocument> chr "# 27  A field guide to base R – R for Data Science (2e)\n\n# 27  A field guide to base R\n\n## 27.1 Introductio"| __truncated__
#>  @ origin: chr "https://r4ds.hadley.nz/base-R.html"

cat_head <- \(md, n = 10) writeLines(head(strsplit(md, "\n")[[1L]], n))
cat_head(md)
#> # 27  A field guide to base R – R for Data Science (2e)
#> 
#> # 27  A field guide to base R
#> 
#> ## 27.1 Introduction
#> 
#> To finish off the programming section, we’re going to give you a quick tour of the most important base R functions that we don’t otherwise discuss in the book. These tools are particularly useful as you do more programming and will help you read code you’ll encounter in the wild.
#> 
#> This is a good place to remind you that the tidyverse is not the only way to solve data science problems. We teach the tidyverse in this book because tidyverse packages share a common design philosophy, increasing the consistency across functions, and making each new function or package a little easier to learn and use. It’s not possible to use the tidyverse without using base R, so we’ve actually already taught you a **lot** of base R functions: from `[library()](https://rdrr.io/r/base/library.html)` to load packages, to `[sum()](https://rdrr.io/r/base/sum.html)` and `[mean()](https://rdrr.io/r/base/mean.html)` for numeric summaries, to the factor, date, and POSIXct data types, and of course all the basic operators like `+`, `-`, `/`, `*`, `|`, `&`, and `!`. What we haven’t focused on so far is base R workflows, so we will highlight a few of those in this chapter.
#> 

## Using selector strings

# By default, this output includes the sidebar and other navigational elements
url <- "https://duckdb.org/code_of_conduct"
read_as_markdown(url) |> cat_head(15)
#> # Code of Conduct – DuckDB
#> 
#> Search Shortcut cmd + k | ctrl + k
#> 
#> # Code of Conduct
#> 
#> **All creatures are welcome**: We aim to create a safe space for all community members, regardless of their age, race, gender, sexual orientation, physical appearance or disability, choice of text editor, or any other qualities by which living beings can be discriminated.
#> 
#> **Be excellent to each other**: We do not tolerate verbal or physical harassment, violence or intimidation.
#> 
#> We do not tolerate life forms who refuse to share this openness and respect towards others: Creatures that are not excellent to others are not welcome.
#> 
#> We continuously strive to make our community a better place for everyone – in the best tradition of hackers we "build, test, improve, reiterate". In this ongoing adventure, we rely on the support, courage, and creativity of all members of the DuckDB community.
#> 
#> If you are made uncomfortable in your role as DuckDB community member, please let us know: You can reach us at [[email protected]](/cdn-cgi/l/email-protection#7f0e0a1e1c143f1b0a1c141b1d51100d18). All complaints will be reviewed and investigated and will result in a response that is deemed necessary and appropriate to the circumstances. The project team is obligated to maintain confidentiality with regard to the reporter of an incident.

# To extract just the main content, use a selector
read_as_markdown(url, html_extract_selectors = "#main_content_wrap") |>
  cat_head()
#> # Code of Conduct – DuckDB
#> 
#> # Code of Conduct
#> 
#> **All creatures are welcome**: We aim to create a safe space for all community members, regardless of their age, race, gender, sexual orientation, physical appearance or disability, choice of text editor, or any other qualities by which living beings can be discriminated.
#> 
#> **Be excellent to each other**: We do not tolerate verbal or physical harassment, violence or intimidation.
#> 
#> We do not tolerate life forms who refuse to share this openness and respect towards others: Creatures that are not excellent to others are not welcome.
#> 

# Alternative approach: zap unwanted nodes
read_as_markdown(
  url,
  html_zap_selectors = c(
    "header",          # name
    ".sidenavigation", # class
    ".searchoverlay",  # class
    "#sidebar"         # ID
  )
) |> cat_head()
#> # Code of Conduct – DuckDB
#> 
#> # Code of Conduct
#> 
#> **All creatures are welcome**: We aim to create a safe space for all community members, regardless of their age, race, gender, sexual orientation, physical appearance or disability, choice of text editor, or any other qualities by which living beings can be discriminated.
#> 
#> **Be excellent to each other**: We do not tolerate verbal or physical harassment, violence or intimidation.
#> 
#> We do not tolerate life forms who refuse to share this openness and respect towards others: Creatures that are not excellent to others are not welcome.
#> 

# Quarto example
read_as_markdown(
  "https://quarto.org/docs/computations/python.html",
  html_extract_selectors = "main",
  html_zap_selectors = c(
    "#quarto-sidebar",
    "#quarto-margin-sidebar",
    "header",
    "footer",
    "nav"
  )
) |> cat_head()
#> # Using Python – Quarto
#> 
#> ## Overview
#> 
#> Quarto supports executable Python code blocks within markdown. This allows you to create fully reproducible documents and reports—the Python code required to produce your output is part of the document itself, and is automatically re-run whenever the document is rendered.
#> 
#> If you have Python and the `jupyter` package installed then you have all you need to render documents that contain embedded Python code (if you don’t, we’ll cover this in the [installation](#installation) section below). Next, we’ll cover the basics of creating and rendering documents with Python code blocks.
#> 
#> ### Code Blocks
#> 

## Convert PDF
pdf <- file.path(R.home("doc"), "NEWS.pdf")
read_as_markdown(pdf) |> cat_head(15)
#> NEWS for R version 4.5.2 (2025-10-31)
#> 
#> NEWS
#> 
#> R News
#> 
#> CHANGES IN R 4.5.2
#> 
#> UTILITIES:
#> 
#> (cid:136) R CMD check now handles archives with extension ‘.tar’ or ‘.tar.zstd’ (where zstd
#> 
#> compression is supported by the R build).
#> 
#> BUG FIXES:
## Alternative:
# pdftools::pdf_text(pdf) |> cat_head()

# Convert images to markdown descriptions using OpenAI
jpg <- file.path(R.home("doc"), "html", "logo.jpg")
if (Sys.getenv("OPENAI_API_KEY") != "") {
  # if (xfun::is_macos()) system("brew install ffmpeg")
  reticulate::py_require("openai")
  llm_client <- reticulate::import("openai")$OpenAI()
  read_as_markdown(jpg, llm_client = llm_client, llm_model = "gpt-4.1-mini") |>
    writeLines()
  # # Description:
  # The image displays the logo of the R programming language. It features a
  # large, stylized capital letter "R" in blue, positioned prominently in the
  # center. Surrounding the "R" is a gray oval shape that is open on the right
  # side, creating a dynamic and modern appearance. The R logo is commonly
  # associated with statistical computing, data analysis, and graphical
  # representation in various scientific and professional fields.
}

# Alternative approach to image conversion:
if (
  Sys.getenv("OPENAI_API_KEY") != "" &&
    rlang::is_installed("ellmer") &&
    rlang::is_installed("magick")
) {
  chat <- ellmer::chat_openai(echo = TRUE)
  chat$chat("Describe this image", ellmer::content_image_file(jpg))
}

# YouTube transcripts
## read_as_markdown() fetches transcripts for YouTube links
cat_head(read_as_markdown("https://youtu.be/GELhdezYmP0"))
#> #  - YouTube
#> 
#> [About](https://www.youtube.com/about/)[Press](https://www.youtube.com/about/press/)[Copyright](https://www.youtube.com/about/copyright/)[Contact us](/t/contact_us/)[Creators](https://www.youtube.com/creators/)[Advertise](https://www.youtube.com/ads/)[Developers](https://developers.google.com/youtube)[Terms](/t/terms)[Privacy](/t/privacy)[Policy & Safety](https://www.youtube.com/about/policies/)[How YouTube works](https://www.youtube.com/howyoutubeworks?utm_campaign=ytgen&utm_source=ythp&utm_medium=LeftNav&utm_content=txt&u=https%3A%2F%2Fwww.youtube.com%2Fhowyoutubeworks%3Futm_source%3Dythp%26utm_medium%3DLeftNav%26utm_campaign%3Dytgen)[Test new features](/new)[NFL Sunday Ticket](https://tv.youtube.com/learn/nflsundayticket)
#> 
#> © 2026 Google LLC

## The default transcript omits timestamps. Supply a custom
## `youtube_transcript_formatter` to control the output. This example formats
## the transcript with timestamped YouTube links.

format_youtube_timestamp <- function(time) {
  h <- time %/% 3600
  time <- time %% 3600
  m <- time %/% 60
  time <- time %% 60
  s <- floor(time)
  out <- paste0(h, "h", m, "m", s, "s")
  out <- sub("^0h", "", out)
  out <- sub("^0m", "", out)
  out
}

format_transcript_with_timestamps <-
  function(data, min_timestamp_stride_seconds = 30, links = FALSE) {
    ts <- format_youtube_timestamp(data$start)
    if (links) {
      video_id <- attr(data, "youtube_metadata")$video_id
      ts <- sprintf("\n<https://youtu.be/%s?t=%s>\n", video_id, ts)
    } else {
      ts <- sprintf("\n[%s] ", ts)
    }

    if (!is.null(min_timestamp_stride_seconds)) {
      show <- c(TRUE, as.logical(diff(x %/% min_timestamp_stride_seconds)))
      ts[!show] <- ""
    }

    paste0(ts, data$text, sep = "", collapse = "\n")
  }


read_as_markdown(
  "https://www.youtube.com/watch?v=GELhdezYmP0",
  youtube_transcript_formatter = \(data) {
    format_transcript_with_timestamps(data, links = TRUE)
  }
) |>
  cat_head(n = 60)
#> #  - YouTube
#> 
#> [About](https://www.youtube.com/about/)[Press](https://www.youtube.com/about/press/)[Copyright](https://www.youtube.com/about/copyright/)[Contact us](/t/contact_us/)[Creators](https://www.youtube.com/creators/)[Advertise](https://www.youtube.com/ads/)[Developers](https://developers.google.com/youtube)[Terms](/t/terms)[Privacy](/t/privacy)[Policy & Safety](https://www.youtube.com/about/policies/)[How YouTube works](https://www.youtube.com/howyoutubeworks?utm_campaign=ytgen&utm_source=ythp&utm_medium=LeftNav&utm_content=txt&u=https%3A%2F%2Fwww.youtube.com%2Fhowyoutubeworks%3Futm_source%3Dythp%26utm_medium%3DLeftNav%26utm_campaign%3Dytgen)[Test new features](/new)[NFL Sunday Ticket](https://tv.youtube.com/learn/nflsundayticket)
#> 
#> © 2026 Google LLC
# }
```
