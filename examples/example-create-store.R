library(ragnar)

base_url <- "https://r4ds.hadley.nz"
pages <- ragnar_find_links(base_url)

store_location <- "r4ds.ragnar.duckdb"
unlink(store_location)

store <- ragnar_store_create(
  store_location,
  embed = \(x) ragnar::embed_ollama(x, model = "all-minilm")
)


for (page in pages) {
  message("ingesting: ", page)
  chunks <- page |>
    ragnar_read(frame_by_tags = c("h1", "h2", "h3")) |>
    dplyr::mutate(link = page) |>
    ragnar_chunk(boundaries = c("paragraph", "sentence")) |>
    # add context to chunks
    dplyr::mutate(
      text = glue::glue(
        r"---(
        # Excerpt from the book "R for Data Science (2e)"
        link: {link}
        chapter: {h1}
        section: {h2}
        subsection: {h3}
        content: {text}

        )---"
      )
    )

  ragnar_store_insert(store, chunks)
}


ragnar_store_build_index(store)
