library(ragnar)

store <- ragnar_store_connect(":memory:")

# local clone copy of https://r4ds.hadley.nz/base-r
# system2("git", c("clone", shQuote("https://github.com/hadley/r4ds/"), shQuote(normalizePath("~/github/hadley/r4ds"))))
# remotes::install_local("~/github/hadley/r4ds", dependencies = TRUE)
# system("quarto render ~/github/hadley/r4ds")
files <- Sys.glob("~/github/hadley/r4ds/_book/*.html")

for (file in files) {
  message("ingesting: ", file)
  chunks <- file |>
    ragnar_read_document(frame_by_tags = c("h1", "h2")) |>
    ragnar_chunk(max_size = 500,
                 boundaries = c("paragraph", "sentence")) |>
    # add context to chunks
    dplyr::mutate(text = glue::glue(r"---(
      # Excerpt from the book "R for Data Science (2e)"
      chapter: {h1}
      section: {h2}
      content: {text}

      )---"))

  chunks <- ragnar_embed_ollama(chunks)
  ragnar_store_insert(store, chunks)
}

ragnar_store_build_index(store)

prompt <- "How can I subset a dataframe with a vector of booleans?"
relevent_chunks <- ragnar_retrieve_vss(store, prompt, top_k = 3)

relevent_chunks$text |> cat(sep = "\n-------\n")

