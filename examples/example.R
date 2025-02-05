library(ragnar)

# local clone copy of https://r4ds.hadley.nz/base-r
# system2("git", c("clone",
#    shQuote("https://github.com/hadley/r4ds/"),
#    shQuote(normalizePath("~/github/hadley/r4ds"))))
# remotes::install_local("~/github/hadley/r4ds", dependencies = TRUE)
# system("quarto render ~/github/hadley/r4ds")

store_location <- "r4ds.ragnar.duckdb"
unlink(store_location)

if (!file.exists(store_location)) {

  store <- ragnar_store_create(
    store_location,
    embed = embed_ollama(model = "all-minilm")
  )

  files <- Sys.glob("~/github/hadley/r4ds/_book/*.html")

  for (file in files) {
    message("ingesting: ", file)
    chunks <- file |>
      ragnar_read_document(frame_by_tags = c("h1", "h2")) |>
      ragnar_chunk(boundaries = c("paragraph", "sentence")) |>
      # add context to chunks
      dplyr::mutate(text = glue::glue(r"---(
      # Excerpt from the book "R for Data Science (2e)"
      chapter: {h1}
      section: {h2}
      content: {text}

      )---"))

    # chunks <- embed_ollama(chunks)
    ragnar_store_insert(store, chunks)
  }

  ragnar_store_build_index(store)

}

### Retrieving Chunks

# Once the store is set up, retrieve the most relevant text chunks:

# store_location <- "r4ds.ragnar.duckdb"
store <- ragnar_store_connect(store_location, read_only = TRUE)
text <- "How can I subset a dataframe with a logical vector?"

embedding_near_chunks <- ragnar_retrieve_vss(store, text, top_k = 3)
embedding_near_chunks
embedding_near_chunks$text |> cat(sep = "\n~~~~~~~~\n")

bm25_near_chunks <- ragnar_retrieve_bm25(store, text, top_k = 3)
bm25_near_chunks
bm25_near_chunks$text |> cat(sep = "\n~~~~~~~~\n")

# get both vss and bm26
(relevant_chunks <- ragnar_retrieve(
  store, text, top_k = 3,
  methods = c("vss", "bm25")
))
