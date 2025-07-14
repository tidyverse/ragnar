# ragnar 0.2.0

* `ragnar_store_create()` gains a new argument: `version`, with default `2`.
  Store version 2 adds support for chunk deoverlapping on retrieval and automatic chunk augmentation with headings.
  To support these features, the internal schema and ingestion requirements are different.
  See `markdown_chunk()` and new S7 classes `MarkdownDocument` and `MarkdownDocumentChunks`.
  Backwards compatibility is maintained with version = 1. (#58, #39, #36)
  
* `ragnar_store_create()` now supports Date and POSIXct classes supplied to `extra_cols`.

* `ragnar_store_create()` now supports remote MotherDuck Databases specified with `md:<dbname>` as
the `location` argument. (#50)

* `ragnar_retrieve()` and friends gain a `filter` argument, adding support for efficiently
  filtering retrieval results.

* `ragnar_retrieve_bm25()` gains arguments `b`, `k`, and `conjunctive` (#56).

* `ragnar_retrieve_vss()` gains argument `query_vector`, supporting workflows that preprocess the query string before embedding.

* `ragnar_retrieve_vss()` set of valid `method` choices have been updated to a narrower set to ensure that an `HNSW` index scan is used.

* Passing a `tbl(store)` to `ragnar_retrieve()` is deprecated.

* New chunker `markdown_chunk()` with support for chunk heading context generation,
  semantic boundary selection, overlapping chunks, document segmentation, and more. (#56)

* New function `embed_google_vertex()` (@dfalbel, #49)

* New function `embed_databricks()` (@atheriel, #45)

* New function `ragnar_chunks_view()` for quickly previewing chunks (#42)

* `ragnar_register_tool_retrieve()` gains optional `name` and `title` arguments
  to allow for more descriptive tool registration. These values can also be set
  in `ragnar_store_create()` (#43).

* `ragnar_read()` and `read_as_markdown()` now accept paths
  that begin with `~` (@topepo, #46, #48).

* Changes to `read_as_markdown()` HTML conversion (#40, #51):

  * New arguments `html_extract_selectors` and `html_zap_selectors` provide a flexible way to
    exclude some html page elements from being included in the converted markdown.
  * code blocks now include the language, if available.
  * Fixed handling of nested code fences in markdown output.

# ragnar 0.1.0

* Initial CRAN submission.
