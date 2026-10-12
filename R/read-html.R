# ' Read text from an html doc
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
  if ("p" %in% split_tags) {
    split_tags <- unique(c("p", split_tags))
  }

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
  if (anyDuplicated(c(nodes, leaves, names))) {
    stop("target names must be unique")
  }

  frame <- vec_frame_flattened_tree_impl(vec, nodes, leaves, names)

  # reorder, drop names, normalize row.names
  for (missing_node in setdiff(c(nodes, leaves, names), names(frame))) {
    frame[[missing_node]] <- NA_character_
  }
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
#' @keywords internal
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

  if (!inherits(x, "xml_node")) {
    x <- read_html(x)
  }

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

  if (base::setequal(split_by_tags, frame_by_tags)) {
    frame[["tag"]] <- NULL
  }

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
#' @param progress Logical, draw a progress bar if `depth > 0`.
#'
#' @param ... Currently unused. Must be empty.
#'
#' @param url_filter A function that takes a character vector of URL's and may
#'   subset them to return a smaller list. This can be useful for filtering out
#'   URL's by rules different than `children_only` which only checks the prefix.
#'
#' @param validate Default is `FALSE`. If `TRUE` sends a `HEAD` request for each
#'   link and removes those that are not accessible. Requests are sent in parallel
#'   using [httr2::req_perform_parallel()].
#'
#' @param get_timestamps Default is `FALSE`. If `TRUE`, sends parallel `HEAD`
#'   requests for any discovered URLs that were not visited during crawling, and
#'   records their `Last-Modified` response header. Pages fetched during crawling
#'   already have their timestamp from the `GET` response at no extra cost.
#'   Useful for incremental ingest: callers can skip re-fetching pages whose
#'   timestamp has not changed since the last run. Note that not all hosting
#'   platforms set `Last-Modified`; S3-backed sites (including many
#'   `docs.posit.co` pages) and GitHub Pages do, while Netlify-hosted sites
#'   (e.g. `quarto.org`) do not. Missing headers are recorded as `NA`.
#'
#' @return A `ragnar_links` object: a character vector of URLs, sorted
#'   alphabetically and deduplicated, with the following attributes:
#'   \describe{
#'     \item{`last_modified`}{A named `POSIXct` vector mapping each crawled URL
#'       to its `Last-Modified` timestamp. Always present. URLs for which the
#'       server did not return a timestamp (either because the header was absent
#'       or `get_timestamps = FALSE` and the page was not visited) are `NA`.}
#'     \item{`problems`}{A data frame with columns `link` and `problem`,
#'       present only when one or more URLs could not be fetched during crawling.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' ragnar_find_links("https://r4ds.hadley.nz/base-R.html")
#' ragnar_find_links("https://ellmer.tidyverse.org/")
#' ragnar_find_links(
#'   paste0("https://github.com/Snowflake-Labs/sfquickstarts/",
#'          "tree/master/site/sfguides/src/build_a_custom_model_for_anomaly_detection"),
#'   children_only = "https://github.com/Snowflake-Labs/sfquickstarts",
#'   depth = 1
#' )
#'
#' # Retrieve Last-Modified timestamps for all discovered URLs
#' links <- ragnar_find_links(
#'   "https://docs.posit.co/",
#'   children_only = TRUE,
#'   depth = 1,
#'   get_timestamps = TRUE
#' )
#' attr(links, "last_modified")
#' }
ragnar_find_links <- function(
  x,
  depth = 0L,
  children_only = FALSE,
  progress = TRUE,
  ...,
  url_filter = identity,
  validate = FALSE,
  get_timestamps = FALSE
) {
  rlang::check_dots_empty()

  if (!inherits(x, "xml_node")) {
    check_string(x)
    x <- read_html2(x)
  }

  depth <- as.integer(depth)

  prefix <- if (isTRUE(children_only)) {
    url <- xml_url2(x)
    # sitemaps are special cased, so we look at the actual root url.
    url <- gsub("sitemap\\.xml$", "", url)
    url_normalize_stem(url)
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
  queued <- reticulate::import_builtins()$set()
  resolved <- reticulate::dict()
  collected <- reticulate::import_builtins()$set()
  problems <- list()
  last_modified <- list()

  start_url <- xml_url2(x)
  deque$append(list(url = start_url, depth = 0))
  queued$add(start_url)
  if (!depth) {
    progress <- FALSE
  }

  if (progress) {
    pb <- cli::cli_progress_bar(
      format = "{cli::pb_spin} Finding links: {length(visited)} | On queue: {length(deque)} | Current depth: {item$depth} | [{round(cli::pb_elapsed_raw)}s]",
      total = NA
    )
  }

  # This is wrapped into a try catch so users interrupts are captured and
  # we are able to return the current set of visited pages.
  tryCatch(
    {
      while (length(deque) > 0) {
        item <- deque$popleft()
        if (progress) {
          cli::cli_progress_update()
        }

        visited$add(item$url)
        page_result <- tryCatch(
          {
            page <- read_html2(item$url)
            resolved_url <- xml_url2(page) # maybe redirected
            resolved[item$url] <- resolved_url
            list(
              links = html_find_links(page),
              resolved_url = resolved_url,
              last_modified = attr(page, "last_modified", exact = TRUE) %||% .POSIXct(NA_real_)
            )
          },

          error = function(e) {
            # if there's an issue finding child links we log it into the `problems` table
            # which is included in the output as an attribute.
            problems[[length(problems) + 1]] <<- list(
              link = item$url,
              problem = conditionMessage(e)
            )
            resolved[item$url] <- ""
            list(links = character(0), resolved_url = NULL, last_modified = NULL)
          }
        )
        if (!is.null(page_result$resolved_url)) {
          last_modified[[page_result$resolved_url]] <- page_result$last_modified
        }
        links <- page_result$links

        links <- url_filter_fn(links)

        # If depth still supports, we add items to the deque if they are not yet
        # visited or already queued.
        if (item$depth + 1 <= depth) {
          for (link in links) {
            if (!visited$`__contains__`(link) && !queued$`__contains__`(link)) {
              deque$append(list(url = link, depth = item$depth + 1))
              queued$add(link)
            }
          }
        }

        collected$update(as.list(links))
      }
    },
    interrupt = function(e) {
      cli::cli_inform(c(i = "User interrupted. Returning the current set!"))
    }
  )
  if (progress) {
    cli::cli_progress_update(force = TRUE)
  }

  out <- collected
  get_resolved <- reticulate::py_to_r(resolved$get)
  out <- reticulate::iterate(out, \(x) get_resolved(x) %||% x)
  out <- as.character(out)
  out <- out[nzchar(out)]
  out <- unique(sort(url_filter_fn(out)))

  # Validate that we can acess all the URL's
  if (validate) {
    resps <- out |>
      lapply(\(url) url |> httr2::request() |> httr2::req_method("HEAD")) |>
      httr2::req_perform_parallel(on_error = "continue")

    is_ok <- resps |>
      map_lgl(\(x) inherits(x, "httr2_response"))

    errors <- map2(out[!is_ok], resps[!is_ok], function(url, err) {
      list(url = url, err = conditionMessage(err))
    })

    problems <- c(problems, errors)

    out <- out[is_ok]
  }

  if (length(problems)) {
    cli::cli_warn(
      "Some links could not be followed. Call {.code attr(.Last.value, 'problems')} to see the issues."
    )
    attr(out, "problems") <- dplyr::bind_rows(problems)
  }

  if (get_timestamps) {
    unvisited <- setdiff(out, names(last_modified))
    if (length(unvisited)) {
      resps <- unvisited |>
        lapply(\(url) {
          httr2::request(url) |>
            httr2::req_method("HEAD") |>
            httr2::req_timeout(10) |>
            httr2::req_error(is_error = \(resp) FALSE)
        }) |>
        httr2::req_perform_parallel(on_error = "continue")
      for (i in seq_along(unvisited)) {
        resp <- resps[[i]]
        ts <- if (inherits(resp, "httr2_response")) {
          raw <- httr2::resp_header(resp, "Last-Modified", default = NA)
          if (!is.na(raw)) {
            tryCatch(
              as.POSIXct(raw, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT"),
              error = \(e) .POSIXct(NA_real_)
            )
          } else {
            .POSIXct(NA_real_)
          }
        } else {
          .POSIXct(NA_real_)
        }
        last_modified[[unvisited[[i]]]] <- ts
      }
    }
  }

  if (length(last_modified)) {
    lm_vec <- do.call(c, last_modified)
    attr(out, "last_modified") <- lm_vec[order(names(lm_vec))]
  } else {
    attr(out, "last_modified") <- structure(
      numeric(0), class = c("POSIXct", "POSIXt"), names = character(0)
    )
  }

  class(out) <- "ragnar_links"
  out
}


#' @export
print.ragnar_links <- function(x, n = 10L, ...) {
  n_total <- length(x)
  cat(sprintf("# A ragnar_links vector: %d URL%s\n", n_total, if (n_total == 1L) "" else "s"))

  if (n_total > 0L) {
    lm <- attr(x, "last_modified", exact = TRUE)
    shown <- min(n, n_total)
    idx <- seq_len(shown)

    shown_urls <- unclass(x)[idx]
    if (!is.null(lm)) {
      df <- data.frame(
        url = shown_urls,
        last_modified = format(lm[shown_urls]),
        stringsAsFactors = FALSE,
        row.names = idx
      )
    } else {
      df <- data.frame(
        url = shown_urls,
        stringsAsFactors = FALSE,
        row.names = idx
      )
    }
    print(df, right = FALSE)

    if (n_total > shown) {
      cat(sprintf("# ... with %d more row%s\n", n_total - shown, if (n_total - shown == 1L) "" else "s"))
    }
  }

  problems <- attr(x, "problems", exact = TRUE)
  if (!is.null(problems) && nrow(problems) > 0L) {
    cat("#\n# Problems:\n")
    print(problems, right = FALSE)
  }

  invisible(x)
}


# E.g.,
# for same site only: prefix = url_host(xml_url(x))
# for child links only: prefix = url_normalize_stem(xml_url(x))
html_find_links <- function(x, absolute = TRUE) {
  if (!inherits(x, "xml_node")) {
    x <- read_html2(x)
  }

  links <- if (is_sitemap(x)) {
    x |>
      xml2::xml_ns_strip() |>
      xml2::xml_find_all("//urlset/url/loc", flatten = TRUE) |>
      xml2::xml_text()
  } else {
    x |>
      xml_find_all(".//a[@href]") |>
      xml_attr("href", default = "")
  }

  # Canonicalize links
  links <- stri_extract_first_regex(links, "^[^#]*") # strip section links
  links <- links[!links %in% c("", "/", "./", "./index.html")] # remove self links
  links <- stri_replace_last_regex(links, "/$", "") # strip trailing /
  # Strip links to source files (e.g. .md source files served alongside HTML)
  links <- links[!stri_detect_regex(links, "\\.md$")]
  links <- sort(unique(links))

  if (absolute) {
    links <- url_absolute2(links, xml_url2(x))
    # Remove links with doubled path segments, which can arise when a page's
    # resolved URL already contains a path prefix that a relative href repeats.
    links <- links[!stri_detect_regex(links, "(/[^/]+(?:/[^/]+)*)\\1")]
  }

  links
}

is_sitemap <- function(x) {
  has_sitemap <- x |>
    xml2::xml_ns_strip() |>
    xml2::xml_find_first("//sitemapindex | //urlset")
  length(has_sitemap) > 0
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
  if (endsWith(url, ".html")) {
    sub("\\.html$", "", url)
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
  if (!is_url(url)) {
    return(xml2::read_html(url, ...))
  }
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
  hdata <- curl::handle_data(handle)
  attr(out, "resolved_url") <- hdata$url
  attr(out, "last_modified") <- hdata$modified %||% .POSIXct(NA_real_)

  # Some sites (e.g. docs.posit.co) serve JS-only redirect pages that curl
  # cannot follow. Detect the pattern and re-fetch the redirect target.
  out <- follow_js_redirect(out, ...)

  out
}

# Detect and follow JS redirect pages of the form:
#   var redirects = {"": "../some/path.html"};  window.location.replace(redirect)
# These are static files that redirect browsers but have no <a> links, so
# curl lands on a blank page. We parse the redirect target and re-fetch.
follow_js_redirect <- function(doc, ...) {
  script_text <- doc |>
    xml2::xml_find_first(".//script[contains(., 'redirects')]") |>
    xml2::xml_text()
  if (is.na(script_text)) return(doc)
  if (!grepl("window\\.location\\.replace", script_text)) return(doc)

  # Extract the default redirect: {"": "TARGET"} or {"": 'TARGET'}
  m <- regmatches(
    script_text,
    regexpr('"[^"]*"\\s*:\\s*["\']([^"\']+)["\']', script_text, perl = TRUE)
  )
  if (!length(m)) return(doc)
  target <- sub('^"[^"]*"\\s*:\\s*["\']([^"\']+)["\']$', "\\1", m)

  base_url <- attr(doc, "resolved_url", exact = TRUE) %||% xml2::xml_url(doc)
  redirect_url <- xml2::url_absolute(target, base_url)

  # Guard against redirect loops
  if (identical(redirect_url, base_url)) return(doc)

  read_html2(redirect_url, ...)
}

is_url <- function(path) {
  grepl("^(http|ftp)s?://", path)
}

xml_url2 <- function(x) {
  attr(x, "resolved_url", TRUE) %||% xml2::xml_url(x)
}
