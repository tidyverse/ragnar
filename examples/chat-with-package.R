library(ragnar)

## Build the RagnarStore
base_url <- "https://ellmer.tidyverse.org"
pages <- ragnar_find_links(base_url, depth = 2)

store_location <- "ellmer.ragnar.duckdb"
unlink(store_location)

store <- ragnar_store_create(
  store_location,
  embed = \(x) ragnar::embed_ollama(x, model = "all-minilm")
)

for (page in pages) {
  message("ingesting: ", page)
  chunks <- page |>
    read_as_markdown() |>
    markdown_chunk(
      pre_segment_heading_levels = 2,
      max_snap_dist = 1600
    )

  ragnar_store_insert(store, chunks)
}

ragnar_store_build_index(store)

## Chat with the store.
ask_ellmer_docs <- function(question) {
  system_prompt <- stringr::str_squish(
    r"--(
    You are an expert R programmer and mentor. When responding, you often begin by
    directly quoting relevant material from guides and documentation, including
    links to your sources. After quoting, you provide additional context and
    interpretation when appropriate.
    )--"
  )
  chat <- ellmer::chat_openai(system_prompt, model = "gpt-4o")

  ragnar_register_tool_retrieve(
    chat,
    store,
    "website for 'ellmer', an R package for interfacing with an LLM"
  )

  chat$chat(question)
}


ask_ellmer_docs(
  "How do I register a tool in ellmer for a function that retrieves content from a database?"
)
