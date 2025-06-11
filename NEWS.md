# ragnar (development version)

* New function `embed_databricks()` (@atheriel, #45)

* New function `ragnar_chunks_view()` for quickly previewing chunks (#42)

* `ragnar_register_tool_retrieve()` gains optional `name` and `title` arguments
  to allow for more descriptive tool registration. These values can also be set
  in `ragnar_store_create()` (#43).

* `ragnar_read()` and `read_as_markdown()` now accept paths
  that begin with `~` (@topepo, #46, #48).

* Changes to `read_as_markdown()` HTML conversion (#40):

  * If a 'main' tag is present, content outside the 'main' tag is now excluded
    by default. To restore the previous behavior and include the sidebar, header,
    footer, and other navigational elements in the converted markdown, use
    `read_as_markdown(x, main_only=FALSE)`.

  * Fixed handling of nested code fences in markdown output.

# ragnar 0.1.0

* Initial CRAN submission.
