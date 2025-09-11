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
#' chat <- ellmer::chat_openai(system_prompt, model = "gpt-4.1")
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

  tool_def <- ragnar_tool_retrieve(
    store = store,
    store_description = store_description,
    ...,
    name = name,
    title = title
  )

  chat$register_tool(tool_def)
  invisible(chat)
}

# Internal: build an ellmer tool for retrieving from a Ragnar store
ragnar_tool_retrieve <- function(
  store,
  store_description = "the knowledge store",
  ...,
  name = NULL,
  title = NULL
) {
  rlang::check_installed("ellmer")

  check_string(name, allow_null = TRUE)
  check_string(title, allow_null = TRUE)

  name <- name %||% glue::glue("rag_retrieve_from_{store@name}")
  title <- title %||% store@title

  previously_retrieved_chunk_ids <- integer()

  ellmer::tool(
    function(text) {
      chunks <- ragnar_retrieve(
        store,
        text,
        ...,
        filter = !.data$chunk_id %in% previously_retrieved_chunk_ids
      )
      previously_retrieved_chunk_ids <<-
        unique(unlist(c(chunks$chunk_id, previously_retrieved_chunk_ids)))
      jsonlite::toJSON(
        chunks,
        pretty = TRUE,
        auto_unbox = TRUE,
        null = "null",
        na = "null",
        rownames = FALSE
      )
    },
    name = name,
    description = glue::glue(
      "Given a string, retrieve the most relevant excerpts from {store_description}. Previously retrieved chunks are not returned; repeated searches of the same query will return unique results."
    ),
    arguments = list(
      text = ellmer::type_string(
        "The text to find the most relevant matches for."
      )
    ),
    annotations = ellmer::tool_annotations(
      title = title,
      read_only_hint = TRUE,
      open_world_hint = FALSE
    )
  )
}

#' Serve a Ragnar store over MCP
#'
#' Launches an MCP server (via mcptools) that exposes a retrieval tool backed by
#' a Ragnar store. This lets MCP-enabled clients (e.g., Claude Desktop, Claude Code)
#' call into your store to retrieve relevant excerpts.
#'
#' @param store A `RagnarStore` object or a file path to a Ragnar DuckDB store.
#'   If a character path is supplied, it is opened with [ragnar_store_connect()].
#' @param store_description Optional string used in the tool description presented
#'   to clients.
#' @param ... Additional arguments forwarded to [ragnar_retrieve()].
#' @param name,title Optional identifiers for the tool. By default, derives from
#'   `store@name` and `store@title` when available.
#' @param extra_tools Optional additional tools (list of `ellmer::tool()` objects)
#'   to serve alongside the retrieval tool.
#'
#' @return This function blocks the current R process by running an MCP server.
#'   It is intended for non-interactive use. Called primarily for side-effects.
#'
#' @export
mcp_serve_store <- function(
  store,
  store_description = "the knowledge store",
  ...,
  name = NULL,
  title = NULL,
  extra_tools = NULL
) {
  rlang::check_installed("mcptools")
  rlang::check_installed("ellmer")

  if (is.character(store)) {
    store <- ragnar_store_connect(store)
  }

  retrieve_tool <- ragnar_tool_retrieve(
    store = store,
    store_description = store_description,
    ...,
    name = name,
    title = title,
    as_json = TRUE
  )

  tools <- c(list(retrieve_tool), extra_tools)
  if (rlang::is_installed("mcptools", version = "0.1.1.9001")) {
    mcptools::mcp_server(tools, FALSE)
  } else {
    mcptools::mcp_server(tools)
  }
}
