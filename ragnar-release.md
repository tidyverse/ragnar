---
output: hugodown::hugo_document
slug: ragnar-0-3
title: ragnar 0.3
date: 2026-01-23
author: Tomasz Kalinowski
description: >
  ragnar adds faster ingestion, new embedding providers, improved
  retrieval, and new integrations for using Ragnar stores from tools.
categories: [package]
tags: [ragnar, ai]
editor:
  markdown:
    canonical: true
    wrap: 72
---

# ragnar 0.3

We’re happy to announce a new release of
[ragnar](https://ragnar.tidyverse.org/), an R package for building
trustworthy retrieval-augmented generation (RAG) workflows.

If you’re new to ragnar, the quickest way to get oriented is the
[Getting Started vignette](https://ragnar.tidyverse.org/articles/ragnar.html).
If you’ve already built a store with ragnar 0.2, this release focuses on
making it easier to scale ingestion, use more embedding providers, and
connect your store to the tools you already use.

You can install ragnar from CRAN with:

``` r
install.packages("ragnar")
```

You can install the development version from GitHub with:

``` r
# install.packages("pak")
pak::pak("tidyverse/ragnar")
```

## What’s new

### Faster ingestion with `ragnar_store_ingest()`

Ingestion is usually the slowest part of building a knowledge store.
`ragnar_store_ingest()` parallelizes the *document preparation* step with
[mirai](https://mirai.r-lib.org), and then writes prepared chunks to the
store in the main process. It’s designed to make it easy to ingest
hundreds (or thousands) of pages without hand-rolling your own parallel
pipeline.

``` r
store <- ragnar_store_create(
  "docs.ragnar.duckdb",
  embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small")
)

paths <- ragnar_find_links("https://quarto.org/sitemap.xml")

ragnar_store_ingest(store, paths, n_workers = 4)
```

### Better retrieval: multiple queries and fewer duplicates

Retrieval is where ragnar tries to be pragmatic: we run both semantic
search (embeddings) and keyword search (BM25) because they fail in
different ways. This release makes it easier to do that intentionally.

- `ragnar_retrieve()` now accepts a *vector of queries*, so you can pass
  one query tuned for semantic search and one tuned for keywords.
- `ragnar_register_tool_retrieve()` uses a new default tool name prefix:
  `search_{store@name}` (instead of `rag_retrieve_from_{store@name}`).
- The ellmer retrieval tool continues to avoid returning previously
  returned chunks, enabling deeper searches via repeated tool calls.
- BM25 result ordering was corrected to sort by descending score.
- Duplicate rows from `ragnar_retrieve()` when running multiple queries
  were removed.

``` r
ragnar_retrieve(
  store,
  c(
    "How do I subset a data frame with a logical vector?",
    "subset dataframe logical vector"
  ),
  top_k = 10
)
```

### New embedding providers: Azure OpenAI and Snowflake

ragnar’s embedding helpers continue to expand so you can use the
infrastructure you already have:

- `embed_azure_openai()` supports embeddings from Azure AI Foundry.
- `embed_snowflake()` supports embeddings via the Snowflake Cortex
  Embedding API.

These integrate the same way as the other providers: you choose an embed
function when creating a store, and ragnar uses it during insertion and
retrieval.

### Better document reading (including YouTube transcripts)

Small improvements in document conversion add up, because low-quality
ingestion leads to low-quality retrieval.

- `read_as_markdown()` once again fetches YouTube transcripts and now
  supports a `youtube_transcript_formatter` so you can include timestamps
  or links in the transcript output.
- Reading plain text with non-ASCII content was fixed.
- `read_as_markdown()` gained an `origin` argument to control what gets
  recorded on returned documents.

### New integrations: serve a store over MCP

`mcp_serve_store()` lets you expose a `RagnarStore` as an MCP tool. This
is particularly useful if you already have a local store and want an
MCP-enabled client (like Codex CLI or Claude Code) to query it directly.

For example, with Codex CLI you can add something like this to
`~/.codex/config.toml`:

``` toml
[mcp_servers.my_store]
command = "Rscript"
args = [
  "-e",
  "ragnar::mcp_serve_store('docs.ragnar.duckdb', top_k=10)"
]
```

### New ways to inspect a store

ragnar now has more tools to help you understand what your store
contains and why retrieval is (or isn’t) working:

- The Store Inspector UI received a number of usability improvements
  (keyboard shortcuts, improved preview, better metadata display, and
  general bug fixes).
- `ragnar_store_atlas()` integrates with the Python Embedding Atlas
  project to visualize your embedding space (via reticulate).

## Get started

- **Install:** `install.packages("ragnar")`
- **Read:** https://ragnar.tidyverse.org/articles/ragnar.html
- **Reference:** https://ragnar.tidyverse.org/reference/

## Additional improvements

- `ragnar_find_links()` works better with local HTML files, and the new
  default `children_only = FALSE` returns all links on a page.
- `embed_ollama()` now defaults to the `embeddinggemma` model.
- Error messages from `embed_openai()` are now surfaced to the user.
- The `RagnarStore` print method now shows the store location.

For a complete list of changes, see the
[NEWS](https://github.com/tidyverse/ragnar/blob/main/NEWS.md).
