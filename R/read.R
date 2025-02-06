

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
#' html_text3("https://r4ds.hadley.nz/base-r") |>
#'   tibble::enframe()
html_text3 <- function(file, split_tags = c("h1", "h2", "h3", "p"), doc = read_html(file)) {
  if("p" %in% split_tags)
    split_tags <- unique(c("p", split_tags))

  if (any(stri_detect_fixed(html_text(doc), c("____RAGNAR_SPLIT____",
                                              "____RAGNAR_TAG_NAME____")))) {
    # TODO: find a better unique sequence we can safely split on.
    # (or add support for splitting directly into rvest::html_text2())
    warning("splits might be inaccurate")
  }

  for (tag_name in split_tags) {
    for (node in html_elements(doc, tag_name)) {
      xml_add_sibling(node, "p",
                      "____RAGNAR_SPLIT____",
                      "____RAGNAR_TAG_NAME____", tag_name, "__",
                      .where = "before")
      xml_add_sibling(node, "p",
                      "____RAGNAR_SPLIT____",
                      .where = "after")
    }
  }
  # TODO: collect added nodes, call xml_remove() to restore doc to original state afterwards?
  # or implement and use xml_clone(): https://github.com/r-lib/xml2/issues/341

  txt <- html_text2(doc)
  txt <- stri_split_fixed(txt, "____RAGNAR_SPLIT____")[[1L]]

  is_named_tag <- stri_startswith_fixed(txt, "____RAGNAR_TAG_NAME____")
  x <- stri_match_first_regex(txt[is_named_tag], "____RAGNAR_TAG_NAME____(.+)__(?s:(.+))")

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
#' @param vec named character vector, (e.g., returned by `html_text3()`) where `names()` correspond to `nodes`
#' @param nodes character vector of names. `names(vec)` that match to `nodes` will are taken as nodes in the tree. These will become column names in the returned data frame.
#' @param names string, name of column for any unmatched nodes, taken from `names(vec)`.
#' @param leaves string, the name of the last column in the returned frame, corresponding to all the unmatched elements in `vec` that are implicitly children of the last entry in `nodes`.
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
vec_frame_flattened_tree <- function(vec, nodes, leaves = ".content", names = ".name") {
  stopifnot(is.vector(vec), !is.null(names(vec))) # TODO: accept data frames
  check_character(nodes)
  check_string(leaves)
  check_string(names)
  if (anyDuplicated(c(nodes, leaves, names)))
    stop("target names must be unique")

  frame <- data_frame("{leaves}" := vec, "{names}" := base::names(vec))
  for (name in nodes) {
    is_node <- frame[[names]] == name
    frame[[name]] <- ifelse(is_node, frame[[leaves]], NA) |> vec_fill_missing("down")
    frame <- frame[!is_node, ]
  }

  # reorder
  frame <- frame[c(nodes, names, leaves)]

  frame
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
#' download.file("https://r4ds.hadley.nz/base-r", file, quiet = TRUE)
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
ragnar_read_document <- function(x, ...,
                                 split_by_tags = frame_by_tags,
                                 frame_by_tags = NULL) {
  if(is.null(frame_by_tags) && is.null(split_by_tags)) {
    text <- html_text2(read_html(x, ...))
    return(text)
  }

  if(!inherits(x, "xml_node"))
    x <- read_html(x)

  text <- html_text3(doc = x, split_tags = unique(c(split_by_tags, frame_by_tags)))
  if (is.null(frame_by_tags)) {
    # TODO: Return a 2 col tibble, instead of a named vector.
    # return(enframe(text, "tag", "text"))
    return(text)
  }

  frame <- vec_frame_flattened_tree(text, frame_by_tags,
                                    names = "tag", leaves = "text")

  if (base::setequal(split_by_tags, frame_by_tags))
    frame[["tag"]] <- NULL

  as_tibble(frame)
}
