
# example:
#  x <- matrix(double(), 3, 4)
#  is_double2(x, c(3, 4))   # TRUE
#  is_double2(x, c(NA, 4))  # TRUE
#  is_double2(x, c(NA, NA)) # TRUE
#  is_double2(x, c(NA, NA, NA)) # FALSE
#  is_double2(x, 12)            # FALSE
is_double2 <- function(x, dim = NULL) {
  if (is.null(dim))
    return(is_double(x))

  if (!is.double(x))
    return(FALSE)

  actual_size <- base::dim(x)
  expected_size <- as.integer(dim)
  if (length(actual_size) != length(expected_size))
    return(FALSE)

  all(expected_size == actual_size, na.rm = TRUE)
}

map_chr <- function(.x, .f, ...) vapply(X = .x, FUN = .f, FUN.VALUE = "", ...)

map2 <- function (.x, .y, .f, ...) {
  out <- .mapply(.f, list(.x, .y), list(...))
  if (length(.x) == length(out))
    names(out) <- names(.x)
  out
}

map3 <- function (.x, .y, .z, .f, ...) {
  out <- .mapply(.f, list(.x, .y, .z), list(...))
  if (length(.x) == length(out))
    names(out) <- names(.x)
  out
}

`%""%` <- function(x, y) {
  stopifnot(is.character(x), is.character(y))
  if (length(y) != length(x)) {
    stopifnot(length(y) == 1L)
    y <- rep(y, length(x))
  }
  empty <- !nzchar(x)
  x[empty] <- y[empty]
  x
}

`%empty%` <- function(x, y) if (length(x)) x else y
last <- function(x) x[[length(x)]]
`add<-` <- function(x, value) x + value
drop_last <- function(x) x[-length(x)]
drop_first <- function(x) x[-1L]
drop_nulls <- function(x) x[!vapply(x, is.null, FALSE, USE.NAMES = FALSE)]


#' @importFrom rvest html_text html_text2 html_elements read_html
#' @importFrom stringi stri_length stri_locate_all_boundaries stri_split_fixed stri_startswith_fixed stri_sub stri_trim_both stri_flatten stri_match_first_regex stri_locate_all_fixed stri_detect_fixed stri_flatten
#' @importFrom xml2 xml_add_sibling
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom tidyr unchop
#' @importFrom vctrs data_frame vec_split vec_rbind vec_locate_matches vec_fill_missing vec_unique vec_slice vec_c
#' @importFrom httr2 request req_url_path_append req_body_json req_perform resp_body_json req_retry req_auth_bearer_token
#' @importFrom DBI dbExecute dbConnect dbExistsTable dbGetQuery dbQuoteString
#' @importFrom glue glue glue_data
#' @importFrom methods is
# ' @importFrom rlang names2 # stand alone type checks need to import all of rlang?!?! :\
#' @import rlang
#' @useDynLib ragnar, .registration = TRUE
NULL

