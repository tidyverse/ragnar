
set_markitdown_options <- function(...) {
  init_markitdown(...)
}

init_markitdown <- function(...) {
  .globals$markitdown <- reticulate::import("markitdown")$MarkItDown(...)
}

read_markdown <- function(x, ...) {
  # ... kwargs that can be passed down. These can be supplied to
  # MarkItDown.__init__() or passed on to individual convert() kwargs:
  # llm_client
  # llm_model
  # style_map  (used for docx conversion)
  # exiftool_path  (used for wav mp3 image conversion)
  # file_extension
  # url
  convert <- .globals$markitdown$convert %||% init_markitdown()$convert
  res <- convert(x, ...)
  glue::as_glue(res)
}


markdown_text <- function(md, split_by = c("h1", "h2", "h3", "pre", "p")) {
  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html))
  pandoc::pandoc_convert(text = md, to = "html", output = tmp_html)
  html_text3(doc = read_html(tmp_html, encoding = "UTF-8"),
             split_tags = split_by)
}

markdown_split <- function(text, tags = c("h1", "h2", "h3"), trim = TRUE, omit_empty = TRUE) {

  lines <- text |> stri_split_lines() |> unlist() |> stri_c("\n")
  text <- lines |> stri_flatten()

  doc <- text |>
    commonmark::markdown_html(sourcepos = TRUE, extensions = TRUE, normalize = TRUE) |>
    read_html(encoding = "UTF-8")

  elements <- doc |> xml_find_all(xpath = "//*[@data-sourcepos]")

  df <- tibble::tibble(
    tag = elements |> xml_name(),
    source_position = elements |> xml_attr("data-sourcepos"),
    text = elements |> xml_text()
  )

  df <- dplyr::filter(df, tag %in% unique(c(tags)))

  # common mark returns positions as line:byte-line:byte
  # e.g., 52:1-52:20
  position <- df$source_position |>
    stringi::stri_split_charclass("[-:]", n = 4L, simplify = TRUE)
  storage.mode(position) <- "integer"
  colnames(position) <- c("start_line", "start_byte", "end_line", "end_byte")

  line_numbytes <- stri_numbytes(lines)
  line_startbyte <- c(1L, 1L + drop_last(cumsum(line_numbytes)))

  bytes <- charToRaw(text)

  start <- line_startbyte[position[, "start_line"]] + position[, "start_byte"] - 1L # position of start byte, inclusive
  end <- line_startbyte[position[, "end_line"]] + position[, "end_byte"] - 1L # position of end byte, inclusive
  cuts <- sort(unique(c(rbind(start, end + 1L))))

  nms <- c("", df$tag[match(cuts, start)])
  nms[is.na(nms)] <- ""

  # if(FALSE) {
  #   np <- reticulate::import("numpy", convert = F)
  #   splits <- np$split(
  #     as.array(charToRaw(text)),
  #     as.array(cuts - 1L)
  #   ) |> py_to_r() |> vapply(rawToChar, "")
  # }

  sizes <- c(cuts, length(bytes) + 1L) - c(1L, cuts)
  splits <- vctrs::vec_chop(bytes, sizes = sizes) |> vapply(rawToChar, "")

  if(trim)
    splits <- stri_trim_both(splits)

  names(splits) <- nms

  if(omit_empty) {
    splits <- splits[nzchar(splits) | nzchar(names(splits))]
  } else if (cuts[1L] != 1L) {
    splits <- splits[-1L]
  }
  splits

}


markdown_frame <- function(md, frame_by = c("h1", "h2", "h3"), split_by = c("p", "pre")) {
  md <- split_markdown(md, split_by = unique(c(frame_by, split_by)))
  frame <- vec_frame_flattened_tree(md, frame_by, names = "tag", leaves = "text")
  if (base::setequal(split_by, frame_by))
    frame[["tag"]] <- NULL
  as_tibble(frame)
}


markdown_split_text <- function(text, split_by = c("h1", "h2", "h3", "pre", "p")) {
  ## Uses pandoc to convert md to html, then html_text3() to read and split.
  ## Returns a character vector. Note, the returned text does not have
  ## markdown formatting like ``` fences. Currently unused.
  ## TOOD: probably better to use commonmark instead of pandoc here.
  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html))
  pandoc::pandoc_convert(text = md, to = "html", output = tmp_html)
  html_text3(doc = read_html(tmp_html, encoding = "UTF-8"),
             split_tags = split_by)
}


ragnar_read <- function(x, ...,
                        split_by_tags  = NULL,
                        frame_by_tags = NULL) {

  text <- read_as_markdown(x, ...)
  if (is.null(frame_by_tags) && is.null(split_by_tags)) {
    return(text)
  }

  text <- markdown_split(text, tags = unique(c(split_by_tags, frame_by_tags)))
  if (is.null(frame_by_tags)) {
    # TODO?: Return a 2 col tibble, instead of a named vector.
    # return(enframe(text, "tag", "text"))
    return(text)
  }

  frame <- vec_frame_flattened_tree(text, frame_by_tags,
    names = "tag", leaves = "text"
  )

  if (base::setequal(split_by_tags, frame_by_tags)) {
    frame[["tag"]] <- NULL
  }

  as_tibble(frame)
}
