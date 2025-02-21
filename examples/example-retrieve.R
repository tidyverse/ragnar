
#' ## Retrieving Chunks

library(ragnar)
store_location <- "r4ds.ragnar.duckdb"
store <- ragnar_store_connect(store_location, read_only = TRUE)

text <- "How can I subset a dataframe with a logical vector?"


## Retrieving Chunks
# Once the store is set up, retrieve the most relevant text chunks like this

embedding_near_chunks <- ragnar_retrieve_vss(store, text, top_k = 3)
embedding_near_chunks
embedding_near_chunks$text[1] |> cat(sep = "\n~~~~~~~~\n")

bm25_near_chunks <- ragnar_retrieve_bm25(store, text, top_k = 3)
bm25_near_chunks
bm25_near_chunks$text[1] |> cat(sep = "\n~~~~~~~~\n")

# get both vss and bm26
relevant_chunks <- ragnar_retrieve(
  store, text, top_k = 3,
  methods = c("vss", "bm25")
)
relevant_chunks

# Register ellmer tool
## You can register an ellmer tool to retrieve chunks as well.
## This enables the LLM model to make tool calls to retreive chunks.
system_prompt <- stringr::str_squish(r"--(
    You are an expert R programmer and mentor.
    You often respond by first direct quoting material from book or documentation,
    then adding your own additional context and interpertation.
)--")
chat <- ellmer::chat_openai(system_prompt, model = "gpt-4o")
# chat <- ellmer::chat_ollama(system_prompt, model = "llama3.2:1b")

ragnar_register_tool_retrieve(chat, store)

chat$chat("How can I subset a dataframe?")
