#' Read text from an html doc
#'
#' This builds upon `rvest::html_text2()` by adding support for splitting the returned
#' vector at the locations of requested nodes.
#'
#' @param file passed on to `rvest::read_html()`
#' @param split_tags character vector of tag selectors to split the returned
#'   character vector on. Individually passed on to `rvest::htmls_elements()`.
#' @param doc alternative way to supply the html document. WARNING: the document
#'   is destructively edited, and should not be used after calling this function.
#'
#' @returns
#' Named character vector.
#' names correspond to the requested tags, or "" if the text is not enclosed by one of the requested tags.
#' @noRd
#'
#' @examples
#' html_text3("https://r4ds.hadley.nz/base-R.html") |>
#'   tibble::enframe()
html_text3 <- function(
  file,
  split_tags = c("h1", "h2", "h3", "p"),
  doc = read_html(file)
) {
  if ("p" %in% split_tags) split_tags <- unique(c("p", split_tags))

  if (
    any(stri_detect_fixed(
      html_text(doc),
      c("____RAGNAR_SPLIT____", "____RAGNAR_TAG_NAME____")
    ))
  ) {
    # TODO: find a better unique sequence we can safely split on.
    # (or add support for splitting directly into rvest::html_text2())
    warning("splits might be inaccurate")
  }

  for (tag_name in split_tags) {
    for (node in html_elements(doc, tag_name)) {
      xml_add_sibling(
        node,
        "p",
        "____RAGNAR_SPLIT____",
        "____RAGNAR_TAG_NAME____",
        tag_name,
        "__",
        .where = "before"
      )
      xml_add_sibling(node, "p", "____RAGNAR_SPLIT____", .where = "after")
    }
  }
  # TODO: collect added nodes, call xml_remove() to restore doc to original state afterwards?
  # or implement and use xml_clone(): https://github.com/r-lib/xml2/issues/341

  txt <- html_text2(doc)
  txt <- stri_split_fixed(txt, "____RAGNAR_SPLIT____")[[1L]]

  is_named_tag <- stri_startswith_fixed(txt, "____RAGNAR_TAG_NAME____")
  x <- stri_match_first_regex(
    txt[is_named_tag],
    "____RAGNAR_TAG_NAME____(.+)__(?s:(.+))"
  )

  txt[is_named_tag] <- x[, 3L] # remove  ____RAGNAR_TAG_NAME___<name>__ prefix

  txt <- stri_trim_both(txt)

  # now attach tag names. Content in between tags has name `""`
  nms <- character(length(txt))
  nms[is_named_tag] <- x[, 2L]
  names(txt) <- nms

  # drop empty entries
  txt <- txt[nzchar(txt) | nzchar(names(txt))]

  txt
}


#' Convert a vector that's a flattened tree into a dataframe
#'
#' @param vec named character vector, (e.g., returned by `markdown_segment()` or
#'   `html_text3()`) where `names()` correspond to `nodes`
#' @param nodes character vector of names. `names(vec)` that match to `nodes`
#'   will are taken as nodes in the tree. These will become column names in the
#'   returned data frame.
#' @param names string, name of column for any unmatched nodes, taken from
#'   `names(vec)`.
#' @param leaves string, the name of the last column in the returned frame,
#'   corresponding to all the unmatched elements in `vec` that are implicitly
#'   children of the last entry in `nodes`.
#'
#' @returns a data frame
#' @noRd
#'
#' @examples
#' vec <- c(
#'   "h1" = "Level 1 Heading",
#'   "unnamed text",
#'   "h2" = "Level 2 Heading",
#'   "some name" = "some name text",
#'   "some name" = "moresome name text"
#' )
#' vec_frame_flattened_tree(vec, c("h1", "h2"))
#' vec_frame_flattened_tree(vec, c("h1", "h2"), leaves = "text", names = "tag")
vec_frame_flattened_tree <- function(
  vec,
  nodes,
  leaves = ".content",
  names = ".name"
) {
  stopifnot(is.vector(vec), !is.null(names(vec))) # TODO: accept data frames
  check_character(nodes)
  check_string(leaves)
  check_string(names)
  if (anyDuplicated(c(nodes, leaves, names)))
    stop("target names must be unique")

  frame <- vec_frame_flattened_tree_impl(vec, nodes, leaves, names)

  # reorder, drop names, normalize row.names
  for (missing_node in setdiff(c(nodes, leaves, names), names(frame)))
    frame[[missing_node]] <- NA_character_
  frame <- as.list(frame)[c(nodes, names, leaves)]
  frame <- lapply(frame, `names<-`, NULL)
  vctrs::new_data_frame(frame)
}


vec_frame_flattened_tree_impl <-
  function(vec, nodes, leaves = ".content", names = ".name") {
    if (!length(vec)) {
      return(NULL)
    }

    # If we've processed all parent nodes, create leaf node
    if (!length(nodes)) {
      # Remove empty/NA entries
      vec <- vec[nzchar(vec) & !is.na(vec)]
      return(data_frame("{names}" := names(vec), "{leaves}" := vec))
    }

    node_name <- nodes[1L]
    nodes <- nodes[-1L]

    # Find positions where current tag appears
    is_node <- base::names(vec) == node_name

    # If tag not found, skip to next level
    if (!any(is_node)) {
      return(vec_frame_flattened_tree_impl(vec, nodes, leaves, names))
    }

    # Get headers for this level
    if (is_node[1]) {
      parents <- vec[is_node]
      children <- vec_split(vec, cumsum(is_node))$val |> lapply(`[`, -1L)
    } else {
      # first split has no parent node
      parents <- c(NA, vec[is_node])
      children <- vec_split(vec, cumsum(is_node))$val
      children[-1L] <- children[-1L] |> lapply(`[`, -1L)
    }
    base::names(parents) <- NULL

    # for each node,
    # - recurse to frame the children,
    # - attach this node as a column (with recycling)
    frames <- map2(parents, children, function(node, vec) {
      frame <- vec_frame_flattened_tree_impl(vec, nodes, leaves, names)
      vctrs::vec_cbind("{node_name}" := node, frame)
    })

    # Combine all nodes into a single dataframe
    vec_rbind(!!!frames)
  }

#' Read an HTML document
#'
#' @param x file path or url, passed on to `rvest::read_html()`, or an `xml_node`.
#' @param ... passed on to `rvest::read_html()`
#' @param split_by_tags character vector of html tag names used to split the
#'   returned text
#' @param frame_by_tags character vector of html tag names used to create a
#'   dataframe of the returned content
#'
#' @returns If `frame_by_tags` is not `NULL`, then a data frame is returned,
#' with column names `c("frame_by_tags", "text")`.
#'
#' If `frame_by_tags` is `NULL` but `split_by_tags` is not `NULL`, then a named
#' character vector is returned.
#'
#' If both `frame_by_tags` and `split_by_tags` are `NULL`, then a string
#' (length-1 character vector) is returned.
#' @export
#'
#' @examples
#' file <- tempfile(fileext = ".html")
#' download.file("https://r4ds.hadley.nz/base-R.html", file, quiet = TRUE)
#'
#' # with no arguments, returns a single string of the text.
#' file |> ragnar_read_document() |> str()
#'
#' # use `split_by_tags` to get a named character vector of length > 1
#' file |>
#'   ragnar_read_document(split_by_tags = c("h1", "h2", "h3")) |>
#'   tibble::enframe("tag", "text")
#'
#' # use `frame_by_tags` to get a dataframe where the
#' # headings associated with each text chunk are easily accessible
#' file |>
#'   ragnar_read_document(frame_by_tags = c("h1", "h2", "h3"))
#'
#' # use `split_by_tags` and `frame_by_tags` together to further break up `text`.
#' file |>
#'   ragnar_read_document(
#'     split_by_tags = c("p"),
#'     frame_by_tags = c("h1", "h2", "h3")
#'   )
#'
#' # Example workflow adding context to each chunk
#' file |>
#'   ragnar_read_document(frame_by_tags = c("h1", "h2", "h3")) |>
#'   glue::glue_data(r"--(
#'     ## Excerpt from the book "R for Data Science (2e)"
#'     chapter: {h1}
#'     section: {h2}
#'     content: {text}
#'
#'     )--") |>
#'     # inspect
#'     _[6:7] |> cat(sep = "\n~~~~~~~~~~~\n")
#'
#' # Advanced example of postprocessing the output of ragnar_read_document()
#' # to wrap code blocks in backticks, markdown style
#' library(dplyr, warn.conflicts = FALSE)
#' library(stringr)
#' library(rvest)
#' library(xml2)
#' file |>
#'   ragnar_read_document(frame_by_tags = c("h1", "h2", "h3"),
#'                        split_by_tags = c("p", "pre")) |>
#'   mutate(
#'     is_code = tag == "pre",
#'     text = ifelse(is_code,
#'                   str_c("```", text, "```", sep = "\n"),
#'                   text)) |>
#'   group_by(h1, h2, h3) |>
#'   summarise(text = str_flatten(text, "\n"), .groups = "drop") |>
#'   glue::glue_data(r"--(
#'     # Excerpt from the book "R for Data Science (2e)"
#'     chapter: {h1}
#'     section: {h2}
#'     content: {text}
#'
#'     )--") |>
#'     # inspect
#'     _[9:10] |> cat(sep = "\n~~~~~~~~~~~\n")
#'
#' # Example of preprocessing the input to ragnar_read_document()
#' # to wrap code in backticks, markdown style
#' # same outcome as above, except via pre processing instead of post processing.
#' file |>
#'   read_html() |>
#'   (\(doc) {
#'     # fence preformatted code with triple backticks
#'     for (node in html_elements(doc, "pre")) {
#'       xml_add_child(node, "code", "```\n", .where = 0)
#'       xml_add_child(node, "code", "\n```")
#'     }
#'     # wrap inline code with single backticks
#'     for (node in html_elements(doc, "code")) {
#'       if (!"pre" %in% xml_name(xml_parents(node))) {
#'         xml_text(node) <- str_c("`", xml_text(node), "`")
#'       }
#'     }
#'     doc
#'   })() |>
#'   ragnar_read_document(frame_by_tags = c("h1", "h2", "h3")) |>
#'   glue::glue_data(r"--(
#'     # Excerpt from the book "R for Data Science (2e)"
#'     chapter: {h1}
#'     section: {h2}
#'     content: {text}
#'
#'     )--") |> _[6]
ragnar_read_document <- function(
  x,
  ...,
  split_by_tags = frame_by_tags,
  frame_by_tags = NULL
) {
  if (is.null(frame_by_tags) && is.null(split_by_tags)) {
    text <- html_text2(read_html(x, ...))
    return(text)
  }

  if (!inherits(x, "xml_node")) x <- read_html(x)

  text <- html_text3(
    doc = x,
    split_tags = unique(c(split_by_tags, frame_by_tags))
  )
  if (is.null(frame_by_tags)) {
    # TODO: Return a 2 col tibble, instead of a named vector.
    # return(enframe(text, "tag", "text"))
    return(text)
  }

  frame <- vec_frame_flattened_tree(
    text,
    frame_by_tags,
    names = "tag",
    leaves = "text"
  )

  if (base::setequal(split_by_tags, frame_by_tags)) frame[["tag"]] <- NULL

  as_tibble(frame)
}


#' Find links on a page
#'
#' @param x URL, HTML file path, or XML document. For Markdown, convert to HTML
#'   using [`commonmark::markdown_html()`] first.
#'
#' @param depth Integer specifying how many levels deep to crawl for links. When
#'   `depth > 0`, the function will follow child links (links with `x` as a
#'   prefix) and collect links from those pages as well.
#'
#' @param children_only Logical or string. If `TRUE`, returns only child links
#'   (those having `x` as a prefix). If `FALSE`, returns all links found on the
#'   page. Note that regardless of this setting, only child links are followed
#'   when `depth > 0`.
#'
#' @param progress Logical, draw a progress bar if `depth > 0`. A separate
#'   progress bar is drawn per recursion level.
#'
#' @param ... Currently unused. Must be empty.
#'
#' @param url_filter A function that takes a character vector of URL's and may
#'   subset them to return a smaller list. This can be useful for filtering out
#'   URL's by rules different them `children_only` which only checks the prefix.
#'
#' @return A character vector of links on the page.
#' @export
#'
#' @examples
#' \dontrun{
#' ragnar_find_links("https://r4ds.hadley.nz/base-R.html")
#' ragnar_find_links("https://ellmer.tidyverse.org/")
#' ragnar_find_links("https://ellmer.tidyverse.org/", depth = 2)
#' ragnar_find_links("https://ellmer.tidyverse.org/", depth = 2, children_only = FALSE)
#' ragnar_find_links(
#'   paste0("https://github.com/Snowflake-Labs/sfquickstarts/",
#'          "tree/master/site/sfguides/src/build_a_custom_model_for_anomaly_detection"),
#'   children_only = "https://github.com/Snowflake-Labs/sfquickstarts",
#'   depth = 1
#' )
#' }
ragnar_find_links <- function(
  x,
  depth = 0L,
  children_only = TRUE,
  progress = TRUE,
  ...,
  url_filter = identity
) {
  rlang::check_dots_empty()

  if (!inherits(x, "xml_node")) {
    check_string(x)
    x <- read_html2(x)
  }

  depth <- as.integer(depth)

  prefix <- if (isTRUE(children_only)) {
    url_normalize_stem(xml_url2(x))
  } else if (is.character(children_only)) {
    check_string(children_only)
    children_only
  } else {
    NULL
  }

  url_filter_fn <- if (!is.null(prefix)) {
    \(urls) url_filter(stri_subset_startswith_fixed(urls, prefix))
  } else {
    url_filter
  }

  deque <- reticulate::import("collections")$deque()
  visited <- reticulate::import_builtins()$set()
  problems <- list()

  deque$append(list(url = xml_url2(x), depth = 0))

  pb <- cli::cli_progress_bar(
    format = "{cli::pb_spin} Finding links: {length(visited)} | On queue: {length(deque)} | Current depth: {item$depth} | [{round(cli::pb_elapsed_raw)}s]",
    total = NA
  )

  # This is wrapped into a try catch so users interrupts are captured and
  # we are able to return the current set of visited pages.
  tryCatch(
    {
      while (length(deque) > 0) {
        item <- deque$popleft()
        cli::cli_progress_update()

        visited$add(item$url)

        links <- tryCatch(
          html_find_links(item$url),
          error = function(e) {
            # if there's an issue finding child links we log it into the `problems` table
            # which is included in the output as an attribute.
            problems[[length(problems) + 1]] <<- list(
              link = item$url,
              problem = conditionMessage(e)
            )
            character(0)
          }
        )

        links <- url_filter_fn(links)

        # If depth still supports, we add items to the deque if they are not yet
        # visited.
        if (item$depth + 1 <= depth) {
          for (link in links) {
            if (!visited$`__contains__`(link)) {
              deque$append(list(url = link, depth = item$depth + 1))
            }
          }
        }

        visited$update(as.list(links))
      }
    },
    interrupt = function(e) {
      cli::cli_inform(c(i = "User interrupted. Returning the current set!"))
    }
  )
  cli::cli_progress_update(force = TRUE)

  out <- sort(reticulate::import_builtins()$list(visited))

  if (length(problems)) {
    cli::cli_warn(
      "Some links could not be followed. Call {.code attr(.Last.value, 'problems')} to see the issues."
    )
    attr(out, "problems") <- dplyr::bind_rows(problems)
  }

  out
}


# E.g.,
# for same site only: prefix = url_host(xml_url(x))
# for child links only: prefix = url_normalize_stem(xml_url(x))
html_find_links <- function(x, absolute = TRUE) {
  if (!inherits(x, "xml_node")) {
    x <- read_html2(x)
  }

  links <- x |>
    xml_find_all(".//a[@href]") |>
    xml_attr("href", default = "")

  # Canonicalize links
  links <- stri_extract_first_regex(links, "^[^#]*") # strip section links
  links <- links[!links %in% c("", "/", "./", "./index.html")] # remove self links
  links <- stri_replace_last_regex(links, "/$", "") # strip trailing /
  links <- sort(unique(links))

  if (absolute) links <- url_absolute2(links, xml_url2(x))

  links
}

url_host <- function(x, baseurl = NULL) {
  map_chr(x, \(url) {
    # tryCatch to guard against error from, e.g., "mailto:copilot-safety@github.com"
    tryCatch(curl::curl_parse_url(url, baseurl)$host, error = \(e) NULL) %||%
      NA_character_
  })
}

url_absolute2 <- function(urls, baseurl) {
  links <- url_absolute(urls, baseurl)
  # It's possible that `url_absolute()` returns `NA` when some kind of invalid is used
  # as input. In this case, we replace the `NA` with the original URL so we can report
  # the problem later.
  # For an example of failing url see:
  # https://docs.posit.co/drivers/2024.03.0/pdf/Simba Teradata ODBC Connector Install and Configuration Guide.pdf
  na_links <- is.na(links)
  links[na_links] <- urls[na_links]
  links
}

url_normalize_stem <- function(url) {
  check_string(url)
  # if (endsWith(url, "index.html")) {
  #   dirname(dirname(url))
  # } else
  if (endsWith(url, ".html")) {
    dirname(url)
  } else {
    url
  }
}

stri_subset_startswith_fixed <- function(str, pattern, ...) {
  str[stri_startswith_fixed(str, pattern, ...)]
}

# workaround for https://github.com/r-lib/xml2/issues/453
read_html2 <- function(url, ...) {
  # For some reason curl is both erroring and warning when the URL is invalid or
  # returns 404. We don't really want the warnings, so we discard them.
  suppressWarnings({
    handle <- curl::new_handle(followlocation = TRUE)
    # We first try the original URL, if some error occurs we retry with the
    # URL encoded version. (If it's different from the original URL.)
    conn <- tryCatch(
      curl::curl(url, "rb", handle = handle),
      error = function(err) {
        encoded_url <- utils::URLencode(url)
        if (url != encoded_url) {
          handle <<- curl::new_handle(followlocation = TRUE)
          curl::curl(encoded_url, "rb", handle = handle)
        } else {
          stop(err)
        }
      }
    )
  })
  on.exit(tryCatch(close(conn), error = function(e) NULL))
  out <- xml2::read_html(conn, ...)
  attr(out, "resolved_url") <- curl::handle_data(handle)$url
  out
}

xml_url2 <- function(x) {
  attr(x, "resolved_url", TRUE) %||% xml2::xml_url(x)
}
