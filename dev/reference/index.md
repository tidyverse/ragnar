# Package index

## Document processing

Ingest documents from the web, PDFs, Word, or local Markdown.

- [`read_as_markdown()`](https://ragnar.tidyverse.org/dev/reference/read_as_markdown.md)
  : Convert files to Markdown
- [`MarkdownDocument`](https://ragnar.tidyverse.org/dev/reference/MarkdownDocument.md)
  : Markdown documents
- [`ragnar_find_links()`](https://ragnar.tidyverse.org/dev/reference/ragnar_find_links.md)
  : Find links on a page

## Text chunking

Chunk and augment text context.

- [`markdown_chunk()`](https://ragnar.tidyverse.org/dev/reference/markdown_chunk.md)
  : Chunk a Markdown document
- [`MarkdownDocumentChunks`](https://ragnar.tidyverse.org/dev/reference/MarkdownDocumentChunks.md)
  : Markdown documents chunks
- [`ragnar_chunks_view()`](https://ragnar.tidyverse.org/dev/reference/ragnar_chunks_view.md)
  : View chunks with the store inspector

## Store

Store embeddings in DuckDB for fast, local queries.

- [`ragnar_store_create()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_create.md)
  [`ragnar_store_connect()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_create.md)
  : Create and connect to a vector store

- [`ragnar_store_insert()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_insert.md)
  [`ragnar_store_update()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_insert.md)
  :

  Inserts or updates chunks in a `RagnarStore`

- [`ragnar_store_ingest()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_ingest.md)
  : Concurrently ingest documents into a Ragnar store

- [`ragnar_store_build_index()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_build_index.md)
  : Build a Ragnar Store index

- [`ragnar_store_inspect()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_inspect.md)
  : Launch the Ragnar Store Inspector

- [`ragnar_store_atlas()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_atlas.md)
  : Visualize a store using Embedding Atlas

## Embeddings

Embed chunks with commercial or open-source models.

- [`embed_azure_openai()`](https://ragnar.tidyverse.org/dev/reference/embed_azure_openai.md)
  : Uses Azure AI Foundry to create embeddings
- [`embed_bedrock()`](https://ragnar.tidyverse.org/dev/reference/embed_bedrock.md)
  : Embed text using a Bedrock model
- [`embed_databricks()`](https://ragnar.tidyverse.org/dev/reference/embed_databricks.md)
  : Embed text using a Databricks model
- [`embed_google_gemini()`](https://ragnar.tidyverse.org/dev/reference/embed_google_vertex.md)
  [`embed_google_vertex()`](https://ragnar.tidyverse.org/dev/reference/embed_google_vertex.md)
  : Embed using Google Vertex API platform
- [`embed_ollama()`](https://ragnar.tidyverse.org/dev/reference/embed_ollama.md)
  [`embed_openai()`](https://ragnar.tidyverse.org/dev/reference/embed_ollama.md)
  [`embed_lm_studio()`](https://ragnar.tidyverse.org/dev/reference/embed_ollama.md)
  : Embed Text
- [`embed_snowflake()`](https://ragnar.tidyverse.org/dev/reference/embed_snowflake.md)
  : Generate embeddings using Snowflake

## Retrieve

Retrieve relevant chunks using both vector and text search.

- [`ragnar_retrieve()`](https://ragnar.tidyverse.org/dev/reference/ragnar_retrieve.md)
  :

  Retrieve chunks from a `RagnarStore`

- [`ragnar_retrieve_bm25()`](https://ragnar.tidyverse.org/dev/reference/ragnar_retrieve_bm25.md)
  : Retrieves chunks using the BM25 score

- [`ragnar_retrieve_vss()`](https://ragnar.tidyverse.org/dev/reference/ragnar_retrieve_vss.md)
  : Vector Similarity Search Retrieval

- [`ragnar_register_tool_retrieve()`](https://ragnar.tidyverse.org/dev/reference/ragnar_register_tool_retrieve.md)
  : Register a 'retrieve' tool with ellmer

- [`mcp_serve_store()`](https://ragnar.tidyverse.org/dev/reference/mcp_serve_store.md)
  : Serve a Ragnar store over MCP

- [`chunks_deoverlap()`](https://ragnar.tidyverse.org/dev/reference/chunks_deoverlap.md)
  : Merge overlapping chunks in retrieved results
