# Launch the Ragnar Store Inspector

Launches a Shiny app for interactively browsing a Ragnar store,
previewing document chunks, and testing search behavior.

## Usage

``` r
ragnar_store_inspect(store, ...)
```

## Arguments

- store:

  A `RagnarStore` object to inspect.

- ...:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

`NULL` (invisibly).

## Details

The Store Inspector is a Shiny app for exploring a `RagnarStore`. Use it
to quickly see what was ingested and preview search results for
different queries. Type a query in the search bar and choose BM25 or
VSS. The list of documents on the left updates, and clicking a row shows
its text and metadata on the right. You can drag the divider to resize
the document list and preview area.

The preview area shows the chunk content. You can view it as rendered
Markdown or switch to “Raw Text” to see the stored text (long lines are
wrapped). Metadata is shown above the text in YAML format, including any
extra fields stored with the chunk.

## Keyboard Shortcuts

|                  |                                                 |                        |
|------------------|-------------------------------------------------|------------------------|
| *Context*        | *Shortcut*                                      | *Action*               |
| Global           | `/`, `Esc`                                      | Focus search; clear it |
| Documents list   | `ArrowUp`/`ArrowDown`, `j`/`k`                  | Move selection         |
| Vertical Divider | `ArrowLeft`/`ArrowRight` (+`Shift`), `g`/`Home` | Resize; reset          |
