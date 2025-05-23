library(ragnar)

base_url <- "https://r4ds.hadley.nz"
pages <- ragnar_find_links(base_url)

store_location <- "r4ds.ragnar.duckdb"

store <- ragnar_store_create(
  store_location,
  embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small")
)


for (page in pages) {
  message("ingesting: ", page)
  chunks <- page |>
    ragnar_read(frame_by_tags = c("h1", "h2", "h3")) |>
    ragnar_chunk(boundaries = c("paragraph", "sentence")) |>
    # add context to chunks
    dplyr::mutate(
      text = glue::glue(
        r"---(
        # Excerpt from the book "R for Data Science (2e)"
        link: {origin}
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
