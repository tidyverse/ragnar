# ragnar (development version)

# ragnar 0.3.0

## Breaking changes

-   In `ragnar_find_links()`, the default `children_only = FALSE` now
    returns all links on a page. If you relied on the previous default,
    set `children_only = TRUE` (#115).

-   `ragnar_register_tool_retrieve()` now uses `search_{store@name}` as
    the default tool name prefix (instead of
    `rag_retrieve_from_{store@name}`), so you may need to update any
    code that refers to the tool name explicitly (#123, #127).

## New features

-   New `embed_azure_openai()` supports embeddings from Azure AI Foundry
    (#144).

-   New `embed_snowflake()` supports embeddings via the Snowflake Cortex
    Embedding API (#148).

-   New `mcp_serve_store()` lets local MCP clients (e.g. Codex CLI or
    Claude Code) search a `RagnarStore` (#123).

-   `ragnar_retrieve()` (and the corresponding ellmer retrieval tool)
    now accepts a vector of queries (#150).

-   New `ragnar_store_atlas()` visualizes store embeddings (#124).

-   New `ragnar_store_ingest()` prepares documents in parallel with
    mirai and inserts them into a store (#133).

## Minor improvements and fixes

-   `embed_ollama()` now defaults to the `embeddinggemma` model (#121).

-   `embed_openai()` error messages are now surfaced to the user (#112).

-   Embedding helpers now share a generalized request retry policy,
    configurable via `options(ragnar.embed.req_retry = ...)` (#138).

-   ragnar now requires mirai >= 2.5.1 (#139).

-   `print()` on a `RagnarStore` now shows the store location (#116).

-   `ragnar_retrieve_bm25()` now orders results by descending score
    (#122).

-   `ragnar_retrieve()` no longer returns duplicate rows when called
    with multiple queries (#153).

-   The ellmer retrieval tool now omits score columns from its output
    (#130).

-   `ragnar_store_inspect()` now includes keyboard shortcuts, a
    draggable divider, improved preview linkification, better metadata
    display, and other UI tweaks (#120, #117, #118).

-   `ragnar_find_links()` works better with local HTML files (#115).

-   `ragnar_store_insert()` and `ragnar_store_update()` (v2 stores) now
    handle stores that are missing `store@schema` metadata (#146).

-   `read_as_markdown()` once again fetches YouTube transcripts and now
    supports `youtube_transcript_formatter`, so you can add timestamps
    or links to the transcript output (#149).

-   `read_as_markdown()` gains an `origin` argument to customize the
    `@origin` recorded on returned documents (#128).

-   `read_as_markdown()` now correctly reads plain-text files with
    non-ASCII characters (#151).

-   Vignette heading levels were fixed (#129).

-   Added an example using `sentence-transformers` embeddings (#131).

# ragnar 0.2.1

-   `ragnar_register_tool_retrieve()` now registers a tool that will not
    return previously returned chunks, enabling the LLM to perform
    deeper searches of a ragnar store with repeated tool calls (#106).

-   Updates for ellmer v0.3.0 and duckdb v1.3.1 (#99)

-   Improved docs and error message in `ragnar_store_insert()`
    (@mattwarkentin, #88)

-   `ragnar_find_links()` can now parse `sitemap.xml` files. It also
    gains a `validate` argument, allowing for sending a `HEAD` request
    to each link and filtering out broken links (#83).

-   `ragnar_inspector()` now renders all urls as clickable links in the
    chunk markdown viewer, even if url is not a formal markdown link
    (#82).

-   Before running examples and tests we now check if ragnar can load
    DuckDB extensions. This fixes issues in environments where DuckDB
    pre-built binaries for extensions are not compatible with the
    installed DuckDB version (#94).

-   Added `embed_lm_studio` to use LMStudio as an embedding provider
    (#100).

-   Fixed a bug causing `ragnar_retrieve()` to fail when documents were
    inserted without an origin (#102).

-   We now suppress a "Couldn't find ffmpeg or avconv" warning when
    importing markitdown when using `read_as_markdown()`. The warning
    would only be relevant for users doing audio transcription (#103).

-   Added `embed_google_gemini` to use Google Gemini API as an embedding
    provider (#105).

# ragnar 0.2.0

-   `ragnar_store_create()` gains a new argument: `version`, with
    default `2`. Store version 2 adds support for chunk deoverlapping on
    retrieval and automatic chunk augmentation with headings. To support
    these features, the internal schema and ingestion requirements are
    different. See `markdown_chunk()` and new S7 classes
    `MarkdownDocument` and `MarkdownDocumentChunks`. Backwards
    compatibility is maintained with version = 1. (#58, #39, #36)

-   `ragnar_store_create()` now supports Date and POSIXct classes
    supplied to `extra_cols`.

-   `ragnar_store_create()` now supports remote MotherDuck Databases
    specified with `md:<dbname>` as the `location` argument. (#50)

-   `ragnar_retrieve()` and friends gain a `filter` argument, adding
    support for efficiently filtering retrieval results.

-   `ragnar_retrieve_bm25()` gains arguments `b`, `k`, and `conjunctive`
    (#56).

-   `ragnar_retrieve_vss()` gains argument `query_vector`, supporting
    workflows that preprocess the query string before embedding.

-   `ragnar_retrieve_vss()` set of valid `method` choices have been
    updated to a narrower set to ensure that an `HNSW` index scan is
    used.

-   Passing a `tbl(store)` to `ragnar_retrieve()` is deprecated.

-   New chunker `markdown_chunk()` with support for chunk heading
    context generation, semantic boundary selection, overlapping chunks,
    document segmentation, and more. (#56)

-   New function `embed_google_vertex()` (@dfalbel, #49)

-   New function `embed_databricks()` (@atheriel, #45)

-   New function `ragnar_chunks_view()` for quickly previewing chunks
    (#42)

-   `ragnar_register_tool_retrieve()` gains optional `name` and `title`
    arguments to allow for more descriptive tool registration. These
    values can also be set in `ragnar_store_create()` (#43).

-   `ragnar_read()` and `read_as_markdown()` now accept paths that begin
    with `~` (@topepo, #46, #48).

-   Changes to `read_as_markdown()` HTML conversion (#40, #51):

    -   New arguments `html_extract_selectors` and `html_zap_selectors`
        provide a flexible way to exclude some html page elements from
        being included in the converted markdown.
    -   code blocks now include the language, if available.
    -   Fixed handling of nested code fences in markdown output.

# ragnar 0.1.0

-   Initial CRAN submission.
