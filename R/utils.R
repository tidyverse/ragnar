#' @importFrom rvest html_text html_text2 html_elements read_html html_attr
#' @importFrom stringi stri_length stri_locate_all_boundaries stri_split_fixed
#'   stri_startswith_fixed stri_sub stri_trim_both stri_flatten
#'   stri_match_first_regex stri_locate_all_fixed stri_detect_fixed stri_flatten
#'   stri_c stri_split_lines stri_numbytes stri_extract_first_regex
#'   stri_extract_last_regex stri_startswith_charclass stri_replace_last_regex
#'   stri_replace_all_regex stri_replace_all_fixed stri_split_lines1
#'   stri_replace_first_regex stri_replace_na stri_replace_first_fixed
#'   stri_endswith_fixed stri_trim_both stri_split_charclass stri_read_lines
#' @importFrom xml2 xml_add_sibling xml_find_all xml_name xml_attr xml_text
#'   xml_url url_absolute xml_contents xml_find_first
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_rows select mutate filter
#' @importFrom tidyr unchop
#' @importFrom vctrs data_frame vec_split vec_rbind vec_cbind vec_locate_matches
#'   vec_fill_missing vec_unique vec_slice vec_c list_unchop new_data_frame
#'   vec_chop
#' @importFrom httr2 request req_url_path_append req_body_json req_perform
#'   resp_body_json req_retry req_auth_bearer_token
#' @importFrom DBI dbExecute dbConnect dbExistsTable dbGetQuery dbQuoteString
#'   dbWriteTable dbListTables dbReadTable
#' @importFrom glue glue glue_data as_glue
#' @importFrom methods is
#' @importFrom utils head
#' @useDynLib ragnar, .registration = TRUE
NULL

# ' @importFrom rlang names2 # stand alone type checks need to import all of rlang?!?! :\
#' @import rlang
#' @import S7

NULL

`%error%` <- rlang::zap_srcref(
  function(x, y) tryCatch(x, error = function(e) y)
)

`%empty%` <- function(x, y) if (length(x)) x else y
`add<-` <- function(x, value) x + value
first <- function(x) x[[1L]]
last <- function(x) x[[length(x)]]
drop_last <- function(x) x[-length(x)]
drop_first <- function(x) x[-1L]
drop_nulls <- function(x) x[!vapply(x, is.null, FALSE, USE.NAMES = FALSE)]

map_chr <- function(.x, .f, ...) vapply(X = .x, FUN = .f, FUN.VALUE = "", ...)
map_lgl <- function(.x, .f, ...) vapply(X = .x, FUN = .f, FUN.VALUE = TRUE, ...)

map2 <- function(.x, .y, .f, ...) {
  out <- .mapply(.f, list(.x, .y), list(...))
  if (length(.x) == length(out)) names(out) <- names(.x)
  out
}

map3 <- function(.x, .y, .z, .f, ...) {
  out <- .mapply(.f, list(.x, .y, .z), list(...))
  if (length(.x) == length(out)) names(out) <- names(.x)
  out
}

imap <- function(.x, .f, ...) {
  out <- .mapply(.f, list(.x, names(.x) %||% seq_along(.x)), list(...))
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

# example:
#  x <- matrix(double(), 3, 4)
#  is_double2(x, c(3, 4))   # TRUE
#  is_double2(x, c(NA, 4))  # TRUE
#  is_double2(x, c(NA, NA)) # TRUE
#  is_double2(x, c(NA, NA, NA)) # FALSE
#  is_double2(x, 12)            # FALSE
is_double2 <- function(x, dim = NULL) {
  if (is.null(dim)) return(is_double(x))

  if (!is.double(x)) return(FALSE)

  actual_size <- base::dim(x)
  expected_size <- as.integer(dim)
  if (length(actual_size) != length(expected_size)) return(FALSE)

  all(expected_size == actual_size, na.rm = TRUE)
}

capture_args <- function(omit_default_values = TRUE) {
  ## modified copy of keras3:::capture_args()
  envir <- parent.frame(1L)
  fn <- sys.function(-1L)
  call <- sys.call(-1L)
  call <- match.call(fn, call, expand.dots = TRUE, envir = parent.frame(2L))

  fn_arg_nms <- names(formals(fn))
  known_args <- intersect(names(call), fn_arg_nms)
  names(known_args) <- known_args

  call <- as.call(c(
    list,
    lapply(known_args, as.symbol),
    if ("..." %in% fn_arg_nms) quote(...)
  ))
  args <- eval(call, envir)

  if (omit_default_values) {
    default_args <- as.list(formals(fn))[names(args)]
    default_args <- lapply(
      default_args,
      eval,
      envir = new.env(parent = environment(fn))
    )
    for (nm in names(args)) {
      if (identical(default_args[[nm]], args[[nm]])) args[[nm]] <- NULL
    }
  }

  args
}

partial <- function(.fn, .sig, ...) {
  body <- as.call(c(.fn, lapply(names(.sig), as.symbol), ...))
  as.function.default(c(.sig, body), envir = baseenv())
}


reorder_names <- function(..., last = NULL) {
  x <- unique(c(...))
  if (!is.null(last)) x <- unique(c(x, last), fromLast = TRUE)
  x
}

is_windows <- function() identical(.Platform$OS.type, "windows")

