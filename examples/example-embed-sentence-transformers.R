library(ragnar)

## This is an example showing how to use a local embedding model via
## sentence-transformers using reticulate. The only trick is to
## make the `embed` function initialize Python on first run, otherwise
## it's vanilla reticulate code.

# Lazily initialise the Python model the first time the embedder runs.
# `.encode` starts as a bootstrap R closure that swaps itself for
# `model$encode` after loading the Python model.
embed <- carrier::crate(
  function(x) {
    .encode(as.list(x))
  },
  # model_id = "google/embeddinggemma-300m" # requires a HuggingFace Account
  model_id = "ibm-granite/granite-embedding-small-english-r2",
  .encode = function(text) {
    reticulate::py_require(c("sentence-transformers"))
    Sys.setenv("TOKENIZERS_PARALLELISM" = "true")
    sentence_transformers <- reticulate::import("sentence_transformers")
    model <- sentence_transformers$SentenceTransformer(model_id)
    .encode <<- model$encode
    .encode(text)
  }
)

store_path <- "sentence-transformers.ragnar.duckdb"
unlink(c(store_path, paste0(store_path, ".wal")), force = TRUE)

store <- ragnar_store_create(
  location = store_path,
  embed = embed,
  version = 1
)

chunks <- data.frame(
  text = c(
    "Ragnar makes working with RAG simple.",
    "Sentence Transformers provides many embedding models."
  ),
  stringsAsFactors = FALSE
)

ragnar_store_insert(store, chunks)
ragnar_store_build_index(store)

ragnar_retrieve(store, "How do I use sentence transformers?")


# New R session -----------------------------------------------------------

library(ragnar)

store_path <- "sentence-transformers.ragnar.duckdb"

store <- ragnar_store_connect(store_path)

ragnar_retrieve(store, "How do I use sentence transformers?")
