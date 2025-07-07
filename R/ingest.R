RagnarStoreV2 <- R6::R6Class(
  "RagnarStore",
  public = list(
    embed = NULL,
    initialize = function(
      location = ":memory:",
      read_only = !file.exists(location)
    ) {
      if (!file.exists(location)) {
        ragnar_store_create_v2(location, )
      }
      # if exists... populate, otherwise, get embed.
    }
  ),
  private = list(
    conn_ = NULL,
    finalize = function() {
      DBI::dbDisconnect(self$con)
    }
  ),
  active = list(
    con = function(x) {
      if (!missing(x)) {
        stop(
          "RagnarStore$conn cannot be changed after the store is created",
          call. = FALSE
        )
      }
      private$conn_
    }
  ),
  cloneable = FALSE
)


ragnar_store_ingest <- function(store, paths, ...) {
  # cli::cli_progress_bar("Cleaning data", total = length(paths))
  # for (origin in paths) {
  for (i in seq_along(paths)) {
    origin <- paths[i]
    message(sprintf("[% 3i/%i] ingesting: %s", i, length(paths), origin))
    tryCatch(
      {
        chunks <- origin |> read_as_markdown() |> markdown_chunk()
        ragnar_store_update(store, chunks)
      },
      error = warning
    )
    # cli::cli_progress_update()
  }
  # cli::cli_progress_done()
  invisible(store)
}
