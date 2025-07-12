#' Register a 'retrieve' tool with ellmer
#'
#' @param chat a `ellmer:::Chat` object.
#' @param store a string of a store location, or a `RagnarStore` object.
#' @param store_description Optional string, used for composing the tool description.
#' @param ... arguments passed on to `ragnar_retrieve()`.
#' @param name,title Optional tool function name and title. By default,
#'   `store@name` and `store@title` will be used if present. The tool `name`
#'   must be a valid R function name and should be unique with the tools
#'   registered with the [ellmer::Chat] object. `title` is used for
#'   user-friendly display.
#'
#' @returns `chat`, invisibly.
#' @export
#'
#' @examplesIf (file.exists("r4ds.ragnar.duckdb") && Sys.getenv("OPENAI_API_KEY") != "")
#' system_prompt <- stringr::str_squish("
#'   You are an expert assistant in R programming.
#'   When responding, you first quote relevant material from books or documentation,
#'   provide links to the sources, and then add your own context and interpretation.
#' ")
#' chat <- ellmer::chat_openai(system_prompt, model = "gpt-4o")
#'
#' store <- ragnar_store_connect("r4ds.ragnar.duckdb")
#' ragnar_register_tool_retrieve(chat, store)
#' chat$chat("How can I subset a dataframe?")
ragnar_register_tool_retrieve <- function(
  chat,
  store,
  store_description = "the knowledge store",
  ...,
  name = NULL,
  title = NULL
) {
  rlang::check_installed("ellmer")
  store
  list(...)

  check_string(name, allow_null = TRUE)
  check_string(title, allow_null = TRUE)

  name <- name %||% glue::glue("rag_retrieve_from_{store@name}")
  title <- title %||% store@title

  withr::local_options(lifecycle_verbosity = "quiet")
  tool_def <- suppressWarnings(suppressMessages(
    ## TODO: ellmer::tool() changed in dev version (slated next v0.3.0)
    ## need to update once ellmer v0.3.0 is on CRAN
    ellmer::tool(
      .name = name,
      function(text) {
        chunks <- ragnar_retrieve(store, text, ...)
        chunks
      },
      glue::glue(
        "Given a string, retrieve the most relevant excerpts from {store_description}."
      ),
      text = ellmer::type_string(
        "The text to find the most relevant matches for."
      ),
      .annotations = ellmer::tool_annotations(
        title = title,
        read_only_hint = TRUE,
        open_world_hint = FALSE
      )
    )
  ))
  chat$register_tool(tool_def)
  invisible(chat)
}
