# ragnar (development version)

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
