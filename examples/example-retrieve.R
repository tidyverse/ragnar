#' ## Retrieving Chunks

library(ragnar)
store_location <- "r4ds.ragnar.duckdb"
store <- ragnar_store_connect(store_location, read_only = TRUE)

text <- "How can I subset a dataframe with a logical vector?"


#' # Retrieving Chunks
#' Once the store is set up, retrieve the most relevant text chunks like this

(relevant_chunks <- ragnar_retrieve(store, text))


#'  Register ellmer tool
#' You can register an ellmer tool to let the LLM retrieve chunks.
system_prompt <- stringr::str_squish(
  r"--(
  You are an expert R programmer and mentor. You are concise.
  You always respond by first direct quoting material from book or documentation,
  then adding your own additional context and interpretation.
  Always include links to the source materials used.
  )--"
)
chat <- ellmer::chat_openai(
  system_prompt,
  model = "gpt-4.1",
  params = ellmer::params(temperature = .5)
)

ragnar_register_tool_retrieve(chat, store, top_k = 10)

chat$chat("How can I subset a dataframe?")
