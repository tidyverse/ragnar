# ragnar 0.4.0

## Breaking changes

-   `ragnar_read()`, `ragnar_chunk()`, `ragnar_segment()`, and
    `ragnar_chunk_segments()` now emit deprecation warnings. They have been
    documented as deprecated since ragnar 0.2.0. Use `read_as_markdown()` and
    `markdown_chunk()` instead.

-   `embed_ollama()` now defaults to
    `model = "embeddinggemma-2:270m-q8_0-text"`. Existing stores keep the
    embedding model they were created with. Run
    `ollama pull embeddinggemma-2:270m-q8_0-text` before creating new stores
    with the default.

-   Retrieval now rejects metadata columns named `metric_name` or `metric_value`
    (ignoring case) and asks for different column names.

-   ragnar now requires DuckDB >= 1.5.0, whose row-ID fetching keeps indexed
    VSS retrieval bounded to the selected matches.

## New features

-   `ragnar_store_ingest()` now uses modification times or a hash of the
    prepared Markdown to skip unchanged sources (#200). Older v2 stores open
    unchanged in read-only mode (the default) and their schema is upgraded on
    disk when connected with `read_only = FALSE`. Existing documents have
    unknown freshness and are reprocessed when their sources are next ingested;
    later runs can skip unchanged sources (#218).

-   `ragnar_store_sync()` removes documents outside a supplied set of origins
    (#201).

-   `ragnar_store_audit()` reports expected, ingested, and missing origins,
    stored chunk counts, and missing URLs, optionally grouped by a column in
    the expected data frame (@andrie, #203, #210).

-   `ragnar_store_ingest()` now returns an `ingest_summary` attribute reporting
    the status, stored chunk count, and worker error message for each input path.
    It honors index requests when all paths already exist (#202, #208).

-   `ragnar_find_links(metadata = TRUE)` returns a tibble with `source` and
    `last_modified` columns, so timestamps stay with their links when joining or
    binding rows. The default remains a character vector (#212).

-   `ragnar_find_links()` reads modification dates and datetimes from sitemaps,
    avoiding extra timestamp requests for URLs with a known date. Invalid sitemap
    locations are reported in the `problems` attribute. Crawl and validation
    problems have the documented `link` and `problem` columns (@andrie, #212).

-   `ragnar_find_links()` can now include the starting page with
    `include_root = TRUE` (#199).

-   `ragnar_retrieve_vss()` filters can now reference `metric_value`,
    `metric_name`, and, for version 2 stores, `origin` and `text`. Filtered
    searches spanning the initial 5,000 candidates now respect `top_k`.

## Minor improvements and fixes

-   `ragnar_retrieve_vss()` now fetches embeddings and document text for output
    after ranking and filtering candidates, reducing intermediate memory use.
    Store connections disable insertion-order preservation, and the store
    documentation explains how to set a deployment-specific DuckDB memory limit
    (#188).

-   `ragnar_store_ingest()` now reports worker failures as
    `ragnar_ingest_worker_error` warnings with a `path` field identifying the
    failed input, including when `prepare()` returns an invalid result or a
    worker exits (#205).

-   `ragnar_store_ingest()` now limits its default worker count based on the
    detected core count, with a minimum of two workers. If core detection is
    unavailable, four cores are assumed (#206).

-   `ragnar_store_ingest()` skips unchanged files with subsecond modification
    times consistently with DuckDB 2.0.

-   `ragnar_store_ingest()` now prints document origins literally in progress
    messages.

-   `ragnar_find_links()` now returns an empty character vector by default when
    no links are found or all links are filtered out, instead of erroring.

-   `markdown_chunk()` no longer misplaces heading boundaries when the input
    contains combining characters (NFD-normalized text) (@taekop, #158).

-   Embedding helpers such as `embed_bedrock()`, `embed_azure_openai()`, and
    `embed_google_vertex()` now return a function that preserves the supplied
    configuration arguments when `x` is omitted or `NULL`, instead of erroring
    (#187).

-   Windows Python environments now require ONNX Runtime 1.20.1 or earlier,
    avoiding native import failures on CRAN's Windows builders.

-   Installation attempts to warm the Python dependency cache in a separate
    process, except on native Windows ARM64. Native import failures during
    warm-up remain visible in the installation log but no longer abort package
    installation.

-   On native Windows ARM64, MarkItDown conversion and DuckDB stores now
    report that they are unsupported before initializing them.
    Other features remain enabled.

# ragnar 0.3.1

-   `ragnar_store_atlas()` now works with Embedding Atlas >= 0.20.0 and uses
    `nanoarrow` instead of `arrow` for data transfer (#168).

-   `chunks_deoverlap()` now accepts empty input, so `ragnar_retrieve()` no
    longer errors when all chunks are filtered out (#174).

-   `embed_openai()` now accepts integer `dims` values (#175).

-   ragnar now disables ONNX Runtime telemetry (#177).

-   ragnar now requires Python < 3.14 to avoid incompatible MarkItDown
    dependencies.

-   ragnar can now load without an import warning with development versions of S7.

-   `ragnar_store_ingest()` now uses the updated `mirai::race_mirai()` API.
    ragnar now requires mirai >= 2.6.0 (#159).

-   The documentation for `MarkdownDocument()` and `MarkdownDocumentChunks()`
    now includes constructor usage.

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
