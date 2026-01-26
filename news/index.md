# Changelog

## ragnar 0.3.0

CRAN release: 2026-01-23

### Breaking changes

- In
  [`ragnar_find_links()`](https://ragnar.tidyverse.org/reference/ragnar_find_links.md),
  the default `children_only = FALSE` now returns all links on a page.
  If you relied on the previous default, set `children_only = TRUE`
  ([\#115](https://github.com/tidyverse/ragnar/issues/115)).

- [`ragnar_register_tool_retrieve()`](https://ragnar.tidyverse.org/reference/ragnar_register_tool_retrieve.md)
  now uses `search_{store@name}` as the default tool name prefix
  (instead of `rag_retrieve_from_{store@name}`), so you may need to
  update any code that refers to the tool name explicitly
  ([\#123](https://github.com/tidyverse/ragnar/issues/123),
  [\#127](https://github.com/tidyverse/ragnar/issues/127)).

### New features

- New
  [`embed_azure_openai()`](https://ragnar.tidyverse.org/reference/embed_azure_openai.md)
  supports embeddings from Azure AI Foundry
  ([\#144](https://github.com/tidyverse/ragnar/issues/144)).

- New
  [`embed_snowflake()`](https://ragnar.tidyverse.org/reference/embed_snowflake.md)
  supports embeddings via the Snowflake Cortex Embedding API
  ([\#148](https://github.com/tidyverse/ragnar/issues/148)).

- New
  [`mcp_serve_store()`](https://ragnar.tidyverse.org/reference/mcp_serve_store.md)
  lets local MCP clients (e.g. Codex CLI or Claude Code) search a
  `RagnarStore`
  ([\#123](https://github.com/tidyverse/ragnar/issues/123)).

- [`ragnar_retrieve()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve.md)
  (and the corresponding ellmer retrieval tool) now accepts a vector of
  queries ([\#150](https://github.com/tidyverse/ragnar/issues/150)).

- New
  [`ragnar_store_atlas()`](https://ragnar.tidyverse.org/reference/ragnar_store_atlas.md)
  visualizes store embeddings
  ([\#124](https://github.com/tidyverse/ragnar/issues/124)).

- New
  [`ragnar_store_ingest()`](https://ragnar.tidyverse.org/reference/ragnar_store_ingest.md)
  prepares documents in parallel with mirai and inserts them into a
  store ([\#133](https://github.com/tidyverse/ragnar/issues/133)).

### Minor improvements and fixes

- [`embed_ollama()`](https://ragnar.tidyverse.org/reference/embed_ollama.md)
  now defaults to the `embeddinggemma` model
  ([\#121](https://github.com/tidyverse/ragnar/issues/121)).

- [`embed_openai()`](https://ragnar.tidyverse.org/reference/embed_ollama.md)
  error messages are now surfaced to the user
  ([\#112](https://github.com/tidyverse/ragnar/issues/112)).

- Embedding helpers now share a generalized request retry policy,
  configurable via `options(ragnar.embed.req_retry = ...)`
  ([\#138](https://github.com/tidyverse/ragnar/issues/138)).

- ragnar now requires mirai \>= 2.5.1
  ([\#139](https://github.com/tidyverse/ragnar/issues/139)).

- [`print()`](https://rdrr.io/r/base/print.html) on a `RagnarStore` now
  shows the store location
  ([\#116](https://github.com/tidyverse/ragnar/issues/116)).

- [`ragnar_retrieve_bm25()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve_bm25.md)
  now orders results by descending score
  ([\#122](https://github.com/tidyverse/ragnar/issues/122)).

- [`ragnar_retrieve()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve.md)
  no longer returns duplicate rows when called with multiple queries
  ([\#153](https://github.com/tidyverse/ragnar/issues/153)).

- The ellmer retrieval tool now omits score columns from its output
  ([\#130](https://github.com/tidyverse/ragnar/issues/130)).

- [`ragnar_store_inspect()`](https://ragnar.tidyverse.org/reference/ragnar_store_inspect.md)
  now includes keyboard shortcuts, a draggable divider, improved preview
  linkification, better metadata display, and other UI tweaks
  ([\#120](https://github.com/tidyverse/ragnar/issues/120),
  [\#117](https://github.com/tidyverse/ragnar/issues/117),
  [\#118](https://github.com/tidyverse/ragnar/issues/118)).

- [`ragnar_find_links()`](https://ragnar.tidyverse.org/reference/ragnar_find_links.md)
  works better with local HTML files
  ([\#115](https://github.com/tidyverse/ragnar/issues/115)).

- [`ragnar_store_insert()`](https://ragnar.tidyverse.org/reference/ragnar_store_insert.md)
  and
  [`ragnar_store_update()`](https://ragnar.tidyverse.org/reference/ragnar_store_insert.md)
  (v2 stores) now handle stores that are missing `store@schema` metadata
  ([\#146](https://github.com/tidyverse/ragnar/issues/146)).

- [`read_as_markdown()`](https://ragnar.tidyverse.org/reference/read_as_markdown.md)
  once again fetches YouTube transcripts and now supports
  `youtube_transcript_formatter`, so you can add timestamps or links to
  the transcript output
  ([\#149](https://github.com/tidyverse/ragnar/issues/149)).

- [`read_as_markdown()`](https://ragnar.tidyverse.org/reference/read_as_markdown.md)
  gains an `origin` argument to customize the `@origin` recorded on
  returned documents
  ([\#128](https://github.com/tidyverse/ragnar/issues/128)).

- [`read_as_markdown()`](https://ragnar.tidyverse.org/reference/read_as_markdown.md)
  now correctly reads plain-text files with non-ASCII characters
  ([\#151](https://github.com/tidyverse/ragnar/issues/151)).

- Vignette heading levels were fixed
  ([\#129](https://github.com/tidyverse/ragnar/issues/129)).

- Added an example using `sentence-transformers` embeddings
  ([\#131](https://github.com/tidyverse/ragnar/issues/131)).

## ragnar 0.2.1

CRAN release: 2025-08-19

- [`ragnar_register_tool_retrieve()`](https://ragnar.tidyverse.org/reference/ragnar_register_tool_retrieve.md)
  now registers a tool that will not return previously returned chunks,
  enabling the LLM to perform deeper searches of a ragnar store with
  repeated tool calls
  ([\#106](https://github.com/tidyverse/ragnar/issues/106)).

- Updates for ellmer v0.3.0 and duckdb v1.3.1
  ([\#99](https://github.com/tidyverse/ragnar/issues/99))

- Improved docs and error message in
  [`ragnar_store_insert()`](https://ragnar.tidyverse.org/reference/ragnar_store_insert.md)
  ([@mattwarkentin](https://github.com/mattwarkentin),
  [\#88](https://github.com/tidyverse/ragnar/issues/88))

- [`ragnar_find_links()`](https://ragnar.tidyverse.org/reference/ragnar_find_links.md)
  can now parse `sitemap.xml` files. It also gains a `validate`
  argument, allowing for sending a `HEAD` request to each link and
  filtering out broken links
  ([\#83](https://github.com/tidyverse/ragnar/issues/83)).

- `ragnar_inspector()` now renders all urls as clickable links in the
  chunk markdown viewer, even if url is not a formal markdown link
  ([\#82](https://github.com/tidyverse/ragnar/issues/82)).

- Before running examples and tests we now check if ragnar can load
  DuckDB extensions. This fixes issues in environments where DuckDB
  pre-built binaries for extensions are not compatible with the
  installed DuckDB version
  ([\#94](https://github.com/tidyverse/ragnar/issues/94)).

- Added `embed_lm_studio` to use LMStudio as an embedding provider
  ([\#100](https://github.com/tidyverse/ragnar/issues/100)).

- Fixed a bug causing
  [`ragnar_retrieve()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve.md)
  to fail when documents were inserted without an origin
  ([\#102](https://github.com/tidyverse/ragnar/issues/102)).

- We now suppress a “Couldn’t find ffmpeg or avconv” warning when
  importing markitdown when using
  [`read_as_markdown()`](https://ragnar.tidyverse.org/reference/read_as_markdown.md).
  The warning would only be relevant for users doing audio transcription
  ([\#103](https://github.com/tidyverse/ragnar/issues/103)).

- Added `embed_google_gemini` to use Google Gemini API as an embedding
  provider ([\#105](https://github.com/tidyverse/ragnar/issues/105)).

## ragnar 0.2.0

CRAN release: 2025-07-12

- [`ragnar_store_create()`](https://ragnar.tidyverse.org/reference/ragnar_store_create.md)
  gains a new argument: `version`, with default `2`. Store version 2
  adds support for chunk deoverlapping on retrieval and automatic chunk
  augmentation with headings. To support these features, the internal
  schema and ingestion requirements are different. See
  [`markdown_chunk()`](https://ragnar.tidyverse.org/reference/markdown_chunk.md)
  and new S7 classes `MarkdownDocument` and `MarkdownDocumentChunks`.
  Backwards compatibility is maintained with version = 1.
  ([\#58](https://github.com/tidyverse/ragnar/issues/58),
  [\#39](https://github.com/tidyverse/ragnar/issues/39),
  [\#36](https://github.com/tidyverse/ragnar/issues/36))

- [`ragnar_store_create()`](https://ragnar.tidyverse.org/reference/ragnar_store_create.md)
  now supports Date and POSIXct classes supplied to `extra_cols`.

- [`ragnar_store_create()`](https://ragnar.tidyverse.org/reference/ragnar_store_create.md)
  now supports remote MotherDuck Databases specified with `md:<dbname>`
  as the `location` argument.
  ([\#50](https://github.com/tidyverse/ragnar/issues/50))

- [`ragnar_retrieve()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve.md)
  and friends gain a `filter` argument, adding support for efficiently
  filtering retrieval results.

- [`ragnar_retrieve_bm25()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve_bm25.md)
  gains arguments `b`, `k`, and `conjunctive`
  ([\#56](https://github.com/tidyverse/ragnar/issues/56)).

- [`ragnar_retrieve_vss()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve_vss.md)
  gains argument `query_vector`, supporting workflows that preprocess
  the query string before embedding.

- [`ragnar_retrieve_vss()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve_vss.md)
  set of valid `method` choices have been updated to a narrower set to
  ensure that an `HNSW` index scan is used.

- Passing a `tbl(store)` to
  [`ragnar_retrieve()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve.md)
  is deprecated.

- New chunker
  [`markdown_chunk()`](https://ragnar.tidyverse.org/reference/markdown_chunk.md)
  with support for chunk heading context generation, semantic boundary
  selection, overlapping chunks, document segmentation, and more.
  ([\#56](https://github.com/tidyverse/ragnar/issues/56))

- New function
  [`embed_google_vertex()`](https://ragnar.tidyverse.org/reference/embed_google_vertex.md)
  ([@dfalbel](https://github.com/dfalbel),
  [\#49](https://github.com/tidyverse/ragnar/issues/49))

- New function
  [`embed_databricks()`](https://ragnar.tidyverse.org/reference/embed_databricks.md)
  ([@atheriel](https://github.com/atheriel),
  [\#45](https://github.com/tidyverse/ragnar/issues/45))

- New function
  [`ragnar_chunks_view()`](https://ragnar.tidyverse.org/reference/ragnar_chunks_view.md)
  for quickly previewing chunks
  ([\#42](https://github.com/tidyverse/ragnar/issues/42))

- [`ragnar_register_tool_retrieve()`](https://ragnar.tidyverse.org/reference/ragnar_register_tool_retrieve.md)
  gains optional `name` and `title` arguments to allow for more
  descriptive tool registration. These values can also be set in
  [`ragnar_store_create()`](https://ragnar.tidyverse.org/reference/ragnar_store_create.md)
  ([\#43](https://github.com/tidyverse/ragnar/issues/43)).

- [`ragnar_read()`](https://ragnar.tidyverse.org/reference/ragnar_read.md)
  and
  [`read_as_markdown()`](https://ragnar.tidyverse.org/reference/read_as_markdown.md)
  now accept paths that begin with `~`
  ([@topepo](https://github.com/topepo),
  [\#46](https://github.com/tidyverse/ragnar/issues/46),
  [\#48](https://github.com/tidyverse/ragnar/issues/48)).

- Changes to
  [`read_as_markdown()`](https://ragnar.tidyverse.org/reference/read_as_markdown.md)
  HTML conversion
  ([\#40](https://github.com/tidyverse/ragnar/issues/40),
  [\#51](https://github.com/tidyverse/ragnar/issues/51)):

  - New arguments `html_extract_selectors` and `html_zap_selectors`
    provide a flexible way to exclude some html page elements from being
    included in the converted markdown.
  - code blocks now include the language, if available.
  - Fixed handling of nested code fences in markdown output.

## ragnar 0.1.0

CRAN release: 2025-05-30

- Initial CRAN submission.
