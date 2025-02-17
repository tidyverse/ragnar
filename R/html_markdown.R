
html_markdown <- function(node, split_tags = NULL, trim_splits = TRUE, omit_empty_splits = TRUE, convert_table = TRUE) {

  markdown_contents <- function(node, text = flatten, flatten = TRUE) {
    contents <- xml_contents(node)
    if (!text) contents <- contents[xml_name(contents) != "text"]
    md <- unlist(lapply(contents, markdown))

    if(!isFALSE(flatten)) {
      if(isTRUE(flatten))
        flatten <- ""
      md <- md |> stri_flatten(flatten)
    }
    md
  }

  markdown <- function(node, text = TRUE) {
    text <- switch(
      xml_name(node),
      text = {
        if (text) xml_text(node)
      },
      pre = {
        lang <- xml_find_first(node, ".//code/@class") |>
          xml_text() |>
          stri_replace_first_fixed("language-", "") |>
          stri_replace_na("")

        stri_c("```", lang, "\n", xml_text(node), "```")
      },
      br = { "\n" },
      code = { stri_c("`", markdown_contents(node), "`") },
      strong = { stri_c("**", markdown_contents(node), "**") },
      i = , em = { stri_c("_", markdown_contents(node), "_") },
      h1 = { stri_c("# ", markdown_contents(node), "\n") },
      h2 = { stri_c("## ", markdown_contents(node), "\n") },
      h3 = { stri_c("### ", markdown_contents(node), "\n") },
      h4 = { stri_c("#### ", markdown_contents(node), "\n") },
      h5 = { stri_c("##### ", markdown_contents(node), "\n") },
      h6 = { stri_c("###### ", markdown_contents(node), "\n") },
      p = { stri_c(stri_trim_both(markdown_contents(node)), "\n") },
      a = {
        link <- xml_attr(node, "href", default = "")
        if(nzchar(link))
          link <- stri_c("[", markdown_contents(node), "](", link, ")")
        },
      blockquote = {
        lines <-  markdown_contents(node, text = FALSE, flatten = FALSE) |>
          unlist() |>
          stri_trim_both() |>
          stri_flatten("\n\n") |>
          stri_split_lines1()

        stri_c("> ", lines, "\n") |> c("\n") |> stri_flatten()
      },
      img = {
        stri_c("![", xml_attr(node, "alt", default = ""),
               "](",   xml_attr(node, "src"), ")")
      },
      sup = { stri_c("^", markdown_contents(node), "^") },
      sub = { stri_c("~", markdown_contents(node), "~") },
      del = { stri_c("~~", markdown_contents(node), "~~") },
      ul = {

       items <- node |> markdown_contents(text = FALSE, flatten = FALSE) |> stri_trim_both()
       sep <- if (any(stri_detect_fixed(items, "\n\n"))) "\n\n" else "\n"

       # escape list items that look like numbered items
       # so "1968. A great year!" --> "1968\. A great year!
       items <- stri_replace_first_regex(items, "^([0-9]+)(\\.)",  "$1\\\\$2")

       items <- stri_replace_all_fixed(items, "\n", "\n  ")
       items <- stri_c("- ", items, sep) |> stri_flatten()
       stri_c("  ", stri_split_lines1(items), "\n") |>
         c(if (sep == "\n") "\n") |> stri_flatten()
      },
      ol = {
        items <- node |> markdown_contents(text = FALSE, flatten = FALSE) |> stri_trim_both()
        sep <- if (any(stri_detect_fixed(items, "\n\n"))) "\n\n" else "\n"
        items <- stri_replace_all_fixed(items, "\n", "\n  ")

        start <- as.integer(xml_attr(node, "start", default = "1"))
        nums <- seq.int(from = start, along.with = items)

        items <- stri_c(nums, ". ", items, sep) |> stri_flatten()
        stri_c("  ", stri_split_lines1(items), "\n") |>
          c(if (sep == "\n") "\n") |> stri_flatten()
      },
      li = {
        markdown_contents(node)
      },
      table = {
        if (convert_table) {
          markdown_contents(node, text = FALSE) |> stri_c("\n")
        } else {
          stri_c("\n", as.character(node), "\n")
        }
      },
      thead = {markdown_contents(node, text = FALSE)},
      tbody = {markdown_contents(node, text = FALSE)},
      tfoot = {markdown_contents(node, text = FALSE)},
      tr = {
        # not using markdown_contents() because we
        # needs ref to contents to get alignment for headers
        contents <- node |> xml_contents()
        contents <- contents[xml_name(contents) != "text"]
        cells <- contents |> lapply(markdown) |> unlist()

        row <- stri_c("| ", stri_flatten(cells, " | "), " |\n")

        is_header <- all(xml_name(contents) == "th")
        if(is_header) {
          # is it better to handle the header line in thead?
          # what about tfoot?
          header_line <- contents |> xml_attr("align", default = "center") |>
            lapply(\(col) {
              switch(col, left = ":--", right = "--:", center = "---")
            }) |> unlist() |> stri_flatten(" | ") |> stri_c("| ", x = _, " |\n")
          row <- stri_c(row, header_line)
        }
        row
      },
      th = {
        markdown_contents(node)
      },
      td = {
        markdown_contents(node)
      },
      script = {},
      style = {},
      noscript = {},
      comment = {},
      doctype = {},
      {
        # Default case - process all children
        lapply(xml_contents(node), markdown) |> unlist() |> stri_flatten()
      }
    )
    if(xml_name(node) %in% split_tags) {
      text <- stri_c(
        "____RAGNAR_SPLIT____",
        "____RAGNAR_TAG_NAME____", xml_name(node), "__",
        text,
        "____RAGNAR_SPLIT____"
      )
    }
    text
  }

  text <- markdown(node) |> stri_replace_all_regex("[ \t\r]*\n", "\n") # trim trailing ws

  if(length(split_tags)) {
    text <- stri_split_fixed(text, "____RAGNAR_SPLIT____")[[1L]]

    is_named_tag <- stri_startswith_fixed(text, "____RAGNAR_TAG_NAME____")
    x <- stri_match_first_regex(text[is_named_tag], "____RAGNAR_TAG_NAME____(.+)__(?s:(.+))")

    text[is_named_tag] <- x[, 3L] # remove  ____RAGNAR_TAG_NAME___<name>__ prefix

    if (trim_splits)
      text <- stri_trim_both(text)

    # now attach tag names. Content in between tags has name `""`
    nms <- character(length(text))
    nms[is_named_tag] <- x[, 2L]
    names(text) <- nms

    # drop empty entries
    if (omit_empty_splits)
      text <- text[nzchar(text) | nzchar(names(text))]
  }
  text
}


