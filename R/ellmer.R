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
  list(...) # force

  name <- name %||% glue::glue("search_{store@name}")
  title <- title %||% store@title
  location <- store@location

  if (identical(store_description, "the knowledge store") && !is.null(title)) {
    store_description <- title |>
      sub("^search ", "", x = _, ignore.case = TRUE, perl = TRUE) |>
      sub("^the ", "", x = _, ignore.case = TRUE, perl = TRUE)
  }

  omit_cols <- c(
    "bm25",
    "cosine_distance",
    "euclidean_distance",
    "negative_inner_product"
  )
  previously_retrieved_chunk_ids <- integer()

  ellmer::tool(
    function(text) {
      chunks <- ragnar_retrieve(
        store,
        text,
        ...,
        filter = !.data$chunk_id %in% previously_retrieved_chunk_ids
      ) |>
        select(-any_of(omit_cols))

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
      "
      Given a string, retrieve the most relevant excerpts from the {store_description}. \\
      Both BM25 (keyword) search and embedding vector similarity (semantic) search are performed. \\
      Previously retrieved chunks are never returned; \\
      repeated searches of the same query will always return unique new results.
      "
    ),
    arguments = list(
      text = ellmer::type_array(ellmer::type_string(glue::trim(
        "
        Input string to match. \\
        Semantic search works best with full sentences; \\
        keyword search works best with a small set of terms (keywords are stemmed). \\
        Both modes run on every query. \\
        For best results, provide at least two queries: \\
        one optimized for semantic search and one optimized for keyword search.
        "
      )))
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
#' Launches an MCP server (via [mcptools::mcp_server()]) that exposes a
#' retrieval tool backed by a Ragnar store. This lets MCP-enabled clients (e.g.,
#' Codex CLI, Claude Code) call into your store to retrieve relevant
#' excerpts.
#'
#' @param store A `RagnarStore` object or a file path to a Ragnar DuckDB store.
#'   If a character path is supplied, it is opened with
#'   [ragnar_store_connect()].
#' @param store_description Optional string used in the tool description
#'   presented to clients.
#' @inheritParams ragnar_register_tool_retrieve
# ' @param ... Additional arguments forwarded to [ragnar_retrieve()].
# ' @param name,title Optional identifiers for the tool. By default, derives from
# '   `store@name` and `store@title` when available.
#' @param extra_tools Optional additional tools (list of `ellmer::tool()`
#'   objects) to serve alongside the retrieval tool.
#'
#' @return This function blocks the current R process by running an MCP server.
#'   It is intended for non-interactive use. Called primarily for side-effects.
#'
#' @details
#'
#' To use this function with
#' [Codex CLI](https://developers.openai.com/codex/cli/),
#' add something like this to `~/.codex/config.toml`
#'
#' ```toml
#' [mcp_servers.quartohelp]
#' command = "Rscript"
#' args = [
#'   "-e",
#'   "ragnar::mcp_serve_store('/path/to/ragnar.store', top_k=10)"
#' ]
#' ```
#'
#' You can confirm the agent can search the ragnar store by inspecting the
#' output from the `/mcp` command, or by asking it "What tools do you have
#' available?".
#'
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
    mcptools::mcp_server(tools, include_session_tools = FALSE)
  } else {
    mcptools::mcp_server(tools)
  }
}
