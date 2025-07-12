#' @importFrom rvest html_text html_text2 html_elements read_html html_attr
#' @importFrom stringi stri_c stri_count_fixed stri_detect_fixed
#'   stri_endswith_fixed stri_extract_first_regex stri_extract_last_regex
#'   stri_flatten stri_length stri_locate_all_boundaries stri_locate_all_fixed
#'   stri_locate_first_regex stri_match_first_regex stri_numbytes
#'   stri_read_lines stri_replace_all_fixed stri_replace_all_regex
#'   stri_replace_first_fixed stri_replace_first_regex stri_replace_last_fixed
#'   stri_replace_last_regex stri_replace_na stri_split_boundaries
#'   stri_split_charclass stri_split_fixed stri_split_lines stri_split_lines1
#'   stri_startswith_charclass stri_startswith_fixed stri_sub stri_trim_both
#'   stri_trim_right
#' @importFrom xml2 xml_add_sibling xml_find_all xml_name xml_attr xml_text
#'   xml_url url_absolute xml_contents xml_find_first
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_rows coalesce distinct filter join_by left_join mutate
#'   na_if rename rename_with select slice_max slice_min starts_with collect
#'   summarize row_number anti_join lag any_of all_of across lag
#' @importFrom tidyr unchop unnest
#' @importFrom tidyr unchop
#' @importFrom vctrs data_frame vec_split vec_rbind vec_cbind vec_locate_matches
#'   vec_fill_missing vec_unique vec_slice vec_c list_unchop new_data_frame
#'   vec_chop vec_ptype vec_proxy vec_restore
#' @importFrom httr2 request req_url_path_append req_body_json req_perform
#'   resp_body_json req_retry req_auth_bearer_token req_error req_user_agent
#' @importFrom DBI dbExecute dbConnect dbExistsTable dbGetQuery dbQuoteString
#'   dbWriteTable dbListTables dbReadTable dbQuoteIdentifier dbWithTransaction
#'   dbAppendTable dbDisconnect dbListFields
#' @importFrom glue glue glue_data as_glue
#' @importFrom methods is
#' @importFrom utils head
# ' @importFrom rlang names2 # stand alone type checks need to import all of rlang?!?! :\
#' @import rlang
#' @import S7
#' @useDynLib ragnar, .registration = TRUE
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
map_int <- function(.x, .f, ...) vapply(X = .x, FUN = .f, FUN.VALUE = 0L, ...)
map_lgl <- function(.x, .f, ...) vapply(X = .x, FUN = .f, FUN.VALUE = TRUE, ...)

map2 <- function(.x, .y, .f, ...) {
  out <- .mapply(.f, list(.x, .y), list(...))
  if (length(.x) == length(out)) {
    names(out) <- names(.x)
  }
  out
}

map3 <- function(.x, .y, .z, .f, ...) {
  out <- .mapply(.f, list(.x, .y, .z), list(...))
  if (length(.x) == length(out)) {
    names(out) <- names(.x)
  }
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
  if (is.null(dim)) {
    return(is_double(x))
  }

  if (!is.double(x)) {
    return(FALSE)
  }

  actual_size <- base::dim(x)
  expected_size <- as.integer(dim)
  if (length(actual_size) != length(expected_size)) {
    return(FALSE)
  }

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

reorder_by_names <- function(object, to_front, to_back = NULL) {
  nms <- names(object)
  nms <- unique(c(to_front, nms))
  nms <- unique(c(nms, to_back), fromLast = TRUE)
  nms <- base::intersect(nms, names(object))
  object[nms]
}

is_windows <- function() identical(.Platform$OS.type, "windows")


prepend <- function(body, head) c(head, body)

is_scalar <- function(x) identical(length(x), 1L)

replace_val <- function(x, old, new) {
  if (!is_scalar(new)) {
    stop(
      "Unexpected length of replacement value in replace_val().\n",
      "`new` must be length 1, not ",
      length(new)
    )
  }
  x[x %in% old] <- new
  x
}

as_bare_list <- function(x, ...) {
  out <- as.list(x, ...)
  attributes(out) <- NULL
  out
}

as_bare_df <- function(x) {
  new_data_frame(
    as_bare_list(x),
    names = names(x)
  )
}

`:=` <- function(name, value) {
  name <- substitute(name)
  if (!is.symbol(name)) {
    stop("left hand side must be a symbol")
  }
  value <- substitute(value)
  if (!is.call(value)) {
    stop("right hand side must be a call")
  }

  value <- as.call(c(as.list(value), list(name = as.character(name))))
  eval(call("<-", name, value), parent.frame())
}

local_duckdb_register <- function(
  con,
  name,
  table,
  overwrite = TRUE,
  ...,
  .local_envir = parent.frame()
) {
  defer(duckdb::duckdb_unregister(con, name), .local_envir)
  duckdb::duckdb_register(con, name, table, overwrite, ...)
}

defer <- function(expr, envir = parent.frame(), priority = c("first", "last")) {
  thunk <- as.call(list(function() expr))
  after <- match.arg(priority) == "last"
  do.call(base::on.exit, list(thunk, TRUE, after), envir = envir)
}


coalesce_names <- function(x) {
  out <- names2(x)
  empty <- out == ""
  out[empty] <- as.character(x[empty])
  out
}

new_scalar_validator <- function(
  allow_null = FALSE,
  allow_na = FALSE,
  additional_checks = NULL,
  env = baseenv()
) {
  checks <- c(
    if (allow_null) quote(if (is.null(value)) return()),
    quote(if (length(value) != 1L) return("must be a scalar")),
    if (!allow_na) quote(if (anyNA(value)) return("must not be NA")),
    additional_checks
  )

  new_function(
    args = alist(value = ),
    body = as.call(c(quote(`{`), checks)),
    env = env
  )
}


prop_bool <- function(
  default,
  allow_null = FALSE,
  allow_na = FALSE
) {
  stopifnot(is_bool(allow_null), is_bool(allow_na))

  new_property(
    class = if (allow_null) NULL | class_logical else class_logical,
    validator = new_scalar_validator(
      allow_null = allow_null,
      allow_na = allow_na
    ),
    default = default
  )
}


prop_string <- function(
  default = NULL,
  allow_null = FALSE,
  allow_na = FALSE
) {
  stopifnot(is_bool(allow_null), is_bool(allow_na))

  new_property(
    class = if (allow_null) NULL | class_character else class_character,
    default = default,
    validator = new_scalar_validator(
      allow_null = allow_null,
      allow_na = allow_na
    )
  )
}
