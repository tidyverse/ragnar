#' Convert files to Markdown
#'
#' @param path \[string\] A filepath or URL. Accepts a wide variety of file types,
#'   including PDF, PowerPoint, Word, Excel, images (EXIF metadata and OCR),
#'   audio (EXIF metadata and speech transcription), HTML, text-based formats
#'   (CSV, JSON, XML), ZIP files (iterates over contents), YouTube URLs, and
#'   EPUBs.
#' @param ... Passed on to `MarkItDown.convert()`.
#' @param html_extract_selectors Character vector of CSS selectors. If a match
#'   for a selector is found in the document, only the matched node's contents
#'   are converted. Unmatched extract selectors have no effect.
#' @param html_zap_selectors Character vector of CSS selectors. Elements
#'   matching these selectors will be excluded ("zapped") from the HTML document
#'   before conversion to markdown. This is useful for removing navigation bars,
#'   sidebars, headers, footers, or other unwanted elements. By default,
#'   navigation elements (`nav`) are excluded.
#'
#' @returns A single string of Markdown.
#' @export
#'
#' @examplesIf reticulate::py_module_available("markitdown")
#' # Convert HTML
#' read_as_markdown("https://r4ds.hadley.nz/base-R.html") |>
#'   substr(1, 500) |>
#'   cat()
#'
#' read_as_markdown("https://r4ds.hadley.nz/base-R.html", canonical = TRUE) |>
#'   substr(1, 500) |>
#'   cat()
#'
#' # When converting HTML, you might want to omit certain elements, like
#' # sidebars, headers, footers, etc. You can pass CSS selector strings
#' # to either extract nodes or exclude nodes during conversion.
#' #
#' # The easiest way to make selectors is to use SelectorGadget:
#' # https://rvest.tidyverse.orgarticles/selectorgadget.html
#' #
#' # You can also right-click on a page and select "Inspect Element" in a
#' # browser to better understand an HTML page's structure.
#' #
#' # For comprehensive or advanced usage of CSS selectors, consult:
#' # https://www.crummy.com/software/BeautifulSoup/bs4/doc/#css-selectors-through-the-css-property
#' # https://facelessuser.github.io/soupsieve/selectors/
#'
#' url <- "https://duckdb.org/code_of_conduct"
#' # Includes the sidebar and other navigational elements
#' read_as_markdown(url) |> substr(1, 500) |> writeLines()
#'
#' # Extract the main content
#' read_as_markdown(url, html_extract_selectors = "#main_content_wrap")
#'
#' # Alternative approach: exclude nodes
#' read_as_markdown(
#'   url,
#'   html_zap_selectors = c(
#'     "header",          # node name
#'     ".sidenavigation", # node class
#'     ".searchoverlay",  # node class
#'     "#sidebar"         # node ID
#'   )
#' ) |> substr(1, 500) |> writeLines()
#'
#' # Quarto example
#' url <- "https://quarto.org/docs/computations/python.html"
#'
#' # Include sidebar, footer, etc.
#' read_as_markdown(
#'   url,
#'   html_extract_selectors = NULL,
#'   html_zap_selectors = NULL
#' ) |> substr(1, 500) |> writeLines()
#'
#' # Exclude content outside <main>
#' read_as_markdown(url, html_extract_selectors = "main") |>
#'   substr(1, 500) |> writeLines()
#'
#' # Exclude specific matching nodes
#' read_as_markdown(
#'   url,
#'   html_extract_selectors = NULL,
#'   html_zap_selectors = c(
#'     "#quarto-sidebar",
#'     "#quarto-margin-sidebar",
#'     "header",
#'     "footer",
#'     "nav"
#'   )
#' ) |> substr(1, 500) |> writeLines()
#'
#' # Convert PDF
#' pdf <- file.path(R.home("doc"), "NEWS.pdf")
#' read_as_markdown(pdf) |> substr(1, 1000) |> cat()
#' ## Alternative:
#' # pdftools::pdf_text(pdf) |> substr(1, 2000) |> cat()
#'
#' # Convert images to markdown descriptions using OpenAI
#' jpg <- file.path(R.home("doc"), "html", "logo.jpg")
#' if (Sys.getenv("OPENAI_API_KEY") != "") {
#'   # if (xfun::is_macos()) system("brew install ffmpeg")
#'   reticulate::py_require("openai")
#'   llm_client <- reticulate::import("openai")$OpenAI()
#'   read_as_markdown(jpg, llm_client = llm_client, llm_model = "gpt-4.1-mini")
#'   # # Description:
#'   # The image displays the logo of the R programming language. It features a
#'   # large, stylized capital letter "R" in blue, positioned prominently in the
#'   # center. Surrounding the "R" is a gray oval shape that is open on the right
#'   # side, creating a dynamic and modern appearance. The R logo is commonly
#'   # associated with statistical computing, data analysis, and graphical
#'   # representation in various scientific and professional fields.
#' }
#'
#' # Alternative approach to image conversion:
#' if (
#'   Sys.getenv("OPENAI_API_KEY") != "" &&
#'     rlang::is_installed("ellmer") &&
#'     rlang::is_installed("magick")
#' ) {
#'   chat <- ellmer::chat_openai(echo = TRUE)
#'   chat$chat("Describe this image", ellmer::content_image_file(jpg))
#' }
read_as_markdown <- function(
  path,
  ...,
  html_extract_selectors = c("main"),
  html_zap_selectors = c("nav")
) {
  check_string(path)
  if (startsWith(path, "~")) {
    x <- path.expand(x)
  }

  if (getOption("ragnar.markitdown.use_reticulate", TRUE)) {
    # use the Python API, faster, more powerful, the default,
    # but we leave an escape hatch just in case there are other python
    # dependencies that conflict
    md <- ragnartools.markitdown$convert_to_markdown(
      path,
      html_extract_selectors = html_extract_selectors,
      html_zap_selectors = html_zap_selectors,
      ...,
    )
  } else {
    md <- read_as_markdown_cli(path, ...)
  }

  # normalize
  md |>
    stri_split_lines() |>
    unlist() |>
    enc2utf8() |>
    stri_trim_right() |>
    stri_flatten("\n") |>
    commonmark::markdown_commonmark(
      normalize = TRUE,
      footnotes = TRUE,
      extensions = TRUE
    ) |>
    as_glue()
}


#' Read a document as Markdown
#'
#' `ragnar_read()` uses [markitdown](https://github.com/microsoft/markitdown) to
#' convert a document to markdown.
#' @param x file path or url.
#' @param ... passed on `markitdown.convert`.
#'
#' @returns
#' Always returns a data frame with the columns:
#'   - `origin`: the file path or url
#'   - `text`: the markdown content
#'
#' @export
ragnar_read <- function(paths, ...) {
  origin <- coalesce_names(paths)
  text <- map_chr(paths, read_as_markdown, ...)
  tibble(origin, text)
}


# ------ utils ----

coalesce_names <- function(x) {
  out <- names2(x)
  empty <- out == ""
  out[empty] <- as.character(x[empty])
  out
}

read_as_markdown_cli <- function(x, ...) {
  # use the markitdown cli API, (much) slower, but can be isolated from
  # reticulated python.
  # TODO: apply markitdown monkeypatches in cli interface too

  check_dots_empty()
  outfile <- withr::local_tempfile(fileext = ".md")
  exit_code <- cli_markitdown(c(shQuote(x), "-o", shQuote(outfile)))
  if (
    !identical(exit_code, 0L) ||
      (no_outfile_produced <- !file.exists(outfile))
  ) {
    # more useful output to stderr() should have been printed
    # already by cli_markitdown() if we are here.
    errmsg <- stri_flatten(
      c(
        paste("markitdown exit code: ", exit_code),
        if (no_outfile_produced) "No output file produced."
      ),
      collapse = "\n"
    )
    stop(errmsg)
  }

  md <- stri_read_lines(outfile)
  md
}


cli_markitdown <- function(args, ...) {
  if (is.na(Sys.getenv("PYTHONIOENCODING", NA))) {
    withr::local_envvar("PYTHONIOENCODING" = "utf-8")
  } # needed on windows

  reticulate::uv_run_tool(
    "markitdown",
    args,

    # pin cli escape hatch to a known working version
    # incase a future markitdown release breaks the default path.
    from = "markitdown[all]==0.1.1",

    # exclude_newer = "2025-02-22",

    python_version = "3.11",
    ...
  )
}


#' View chunks with the store inspector
#'
#' Visualize chunks read by [ragnar_read()] for quick inspection.
#' Helpful for inspecting the results of chunking and reading while iterating
#' on the ingestion pipeline.
#'
#' @param chunks A data frame containing a few chunks.
#'
#' @export
ragnar_chunks_view <- function(chunks) {
  store <- ragnar_store_create(embed = NULL)
  ragnar_store_insert(store, chunks)
  ragnar_store_build_index(store)
  ragnar_store_inspect(store)
}
