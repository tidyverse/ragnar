#' Register a 'retrieve' tool with ellmer
#'
#' @param chat a `ellmer:::Chat` object.
#' @param store a string of a store location, or a `RagnarStore` object.
#' @param store_description Optional string, used for composing the tool description.
#' @param ... arguments passed on to `ragnar_retrieve()`.
#'
#' @returns `chat`, invisibly.
#' @export
#'
#' @examplesIf (file.exists("r4ds.ragnar.duckdb") && Sys.getenv("OPENAI_API_KEY") != "")
#'
#' system_prompt <- stringr::str_squish("
#'   You are an expert assistant in R programming.
#'   When responding, you first quote relevant material from books or documentation,
#'   provide links to the sources, and then add your own context and interpretation.
#' ")
#' chat <- ellmer::chat_openai(system_prompt, model = "gpt-4o")
#'
#' store <- ragnar_store_connect("r4ds.ragnar.duckdb", read_only = TRUE)
#' ragnar_register_tool_retrieve(chat, store)
#' chat$chat("How can I subset a dataframe?")
ragnar_register_tool_retrieve <-
  function(chat, store, store_description = "the knowledge store", ...) {
    rlang::check_installed("ellmer")
    store
    list(...)

    chat$register_tool(
      ellmer::tool(
        .name = glue::glue("rag_retrieve_from_{store@name}"),
        function(text) {
          ragnar_retrieve(store, text, ...)$text |>
            stringi::stri_flatten("\n\n---\n\n")
        },
        glue::glue(
          "Given a string, retrieve the most relevent excerpts from {store_description}."
        ),
        text = ellmer::type_string(
          "The text to find the most relevent matches for."
        )
      )
    )
    invisible(chat)
  }
