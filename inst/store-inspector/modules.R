storeInspectorUI <- function(id, search_types = c("BM25", "VSS")) {
  ns <- \(i) shiny::NS(id, i)

  shiny::tags$html(
    id = ns("html"),
    shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "app.out.css"
      ),
      shiny::tags$script(src = "inspector.js")
    ),
    shiny::tags$body(
      class = "flex flex-col max-h-screen min-h-screen h-full bg-white",
      # Consolidated header: title + search + search-type switch
      shiny::div(
        class = "flex-none bg-blue-500 p-2 gap-2",
        shiny::div(
          class = "flex flex-row items-center gap-2",
          shiny::div(
            "Ragnar Store Inspector",
            class = "flex-none text-lg text-white"
          ),
          shiny::div(
            class = "flex grow justify-center",
            shiny::div(
              class = "flex flex-row items-center gap-2 w-full max-w-2xl justify-center",
              shiny::div(
                class = "flex w-full max-w-2xl shadow-md p-2 rounded-full bg-gray-100 items-center gap-2 focus-within:shadow-blue-500/50",
                shiny::icon(
                  name = "magnifying-glass",
                  class = "flex flex-none text-gray-400"
                ),
                shiny::tags$input(
                  class = "flex-grow bg-transparent outline-none",
                  `data-inspector-query` = "1",
                  id = ns("query"),
                  type = "search",
                  placeholder = "Search the store ..."
                )
              ),
              switchInput(ns("search_type"), search_types)
            )
          ),
          # right spacer to balance layout
          shiny::div(class = "flex-none")
        )
      ),
      # Subheader: Documents + Preview headings
      shiny::div(
        class = "flex flex-row flex-none p-2 border-b pb-1 border-gray-200 items-center justify-between",
        shiny::div(shiny::uiOutput(ns("doc_header"))),
        shiny::div(
          class = "flex flex-row items-center gap-2",
          shiny::h3("Document preview", class = "text-md"),
          switchInput(ns("markdown"), c("Preview", "Raw Text"))
        )
      ),
      shiny::div(
      class = "flex grow p-2 gap-1 h-full overflow-hidden content-split",
      style = "--left-pane-width: 38.2%;",
        listDocumentsUI(ns("document_list")),
        shiny::div(
          id = 'split-resizer',
          class = "flex-none",
          tabindex = 0,
          role = "separator",
          `aria-orientation` = "vertical",
          `aria-controls` = paste(ns("document_list-panel"), ns("preview_panel")),
          style = "width: 6px; cursor: col-resize; background-color: #e5e7eb; border-radius: 3px;"
        ),
        shiny::div(
          id = ns("preview_panel"),
          class = "flex flex-col gap-2 overflow-hidden",
          style = "flex-basis: 61.8%;",
          shiny::uiOutput(
            class = "flex-1",
            style = "min-height: 0;",
            ns("preview")
          )
        )
      ),
    )
  )
}


storeInspectorServer <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    query <- shiny::debounce(shiny::reactive(input$query), 1000)
    search_type <- switchServer("search_type")

    documents <- shiny::reactive({
      if (is.null(query()) || nchar(query()) <= 0) {
        d <- dplyr::tbl(store) |>
          head(100) |>
          dplyr::collect()
        attr(d, "no_filter") <- TRUE
        return(d)
      }

      tryCatch(
        {
          if (search_type() == "VSS") {
            ragnar::ragnar_retrieve_vss(store, query(), top_k = 100)
          } else {
            ragnar::ragnar_retrieve_bm25(store, query(), top_k = 100)
          }
        },
        error = function(err) {
          structure(
            data.frame(),
            error = conditionMessage(err),
            class = "error"
          )
        }
      )
    })

    selectedDocumentId <- listDocumentsServer("document_list", documents)

    selectedDocument <- shiny::reactive({
      if (is.null(selectedDocumentId())) {
        return(NULL)
      }
      docs <- documents()
      docs[docs$chunk_id == selectedDocumentId(), , drop = FALSE]
    })

    preview_type <- switchServer("markdown")

    output$preview <- shiny::renderUI({
      if (is.null(selectedDocument()$text) || nrow(selectedDocument()) == 0) {
        return(shiny::tags$div("Select a document to preview"))
      }

      preview <- if (is.null(preview_type()) || preview_type() == "Preview") {
        # Render markdown to HTML and parse for linkification
        html_preview <- shiny::markdown(selectedDocument()$text)
        html_preview <- xml2::read_html(as.character(html_preview))

        html_preview |>
          xml2::xml_find_all(
            ".//*[not(*) and
                not(self::a) and
                string-length(normalize-space(text())) > 0]"
          ) |>
          lapply(function(node) {
            tryCatch(
              {
                text <- as.character(node)
                # make sure text that look like a link is clickable,
                # but do not match tibble-truncated URLs ending with … (\u2026)
                text <- text |>
                  stringi::stri_replace_all_regex(
                    r"((https?://[^\s\)\]\\>"]++)(?<!\x{2026}))",
                    r"(<a target='_blank' href="$1">$1</a>)"
                  )
                doc <- xml2::read_html(paste0("<div>", text, "</div>"))
                new_node <- xml2::xml_find_first(doc, "//div/*[1]")
                if (!inherits(new_node, "xml_node")) {
                  return()
                }
                xml2::xml_replace(node, new_node)
              },
              error = function(err) NULL
            )
          })

        shiny::tags$iframe(
          class = "size-full text-pretty",
          srcdoc = as.character(html_preview)
        )
      } else {
        shiny::tags$pre(
          class = "text-xs text-pretty whitespace-pre-wrap break-words",
          style = "white-space: pre-wrap; overflow-wrap: anywhere;",
          selectedDocument()$text
        )
      }

      # Select metadata columns robustly (schema may be NULL for v2 connections)
      # Keep useful fields like origin/context/extra cols; drop internals/heavy fields
      # fmt: skip
      to_drop <- c(
        "text", "embedding", "metric_name", "metric_value",
        "doc_id", "chunk_id", "start", "end"
      )
      metadata <- selectedDocument() |>
        dplyr::select(-dplyr::any_of(to_drop))

      shiny::div(
        class = "flex flex-col gap-2 size-full",
        style = "min-height: 0;",
        shiny::div(
          class = "pb-2 max-h-1/3 overflow-y-auto",
          style = "border-bottom: 2px solid #e5e7eb;",
          shiny::pre(
            class = "text-xs text-pretty",
            yaml::as.yaml(
              as.list(metadata),
              handlers = list(
                POSIXct = as.character,
                Date = as.character
              )
            )
          )
        ),
        shiny::div(
          class = "flex-1 overflow-auto",
          style = "min-height: 0;",
          preview
        )
      )
    })

    # Documents header (moved to top-level subheader row)
    output$doc_header <- shiny::renderUI({
      n <- tryCatch(nrow(documents()), error = function(...) 0L)
      label <- if (n == 100L) "100+" else as.character(n)
      shiny::h3(sprintf("Documents (%s)", label), class = "text-md p-1")
    })
  })
}


# Components --------------------------------------------------------------

#' @param documents Is a data.frame with documents returned from [ragnar_retrieve_vss()]
#'   or [ragnar_retrieve_bm25()].
listDocumentsUI <- function(id) {
  ns <- \(i) shiny::NS(id, i)

  clickHandler <- shiny::tags$script(shiny::HTML(glue::glue(
    "
      // This handles clicks on the listing div and
      // 1. Make sure the clicked line get border, indicating it was clicked
      // 2. Updates the selected_document elemtn.
      $(document).on('click', '#{ns('list')}', function(e) {{
          const x = $(e.target).closest('.document-summary');
          if (x.length) {{
            const siblings = x.siblings().removeClass('border border-sky-500');
            x.addClass('border border-sky-500');
            Shiny.setInputValue('{ns('selected_document')}', parseInt(x.attr('data-document-id')));
          }}
      }});

      // Make the list focusable for keyboard navigation
      $(function() {{
        const $list = $('#{ns('list')}');
        if ($list.length) {{ $list.attr('tabindex', 0); }}
      }});

      // Keyboard navigation: Up/Down (and Vim j/k) to change selection
      $(document).on('keydown', '#{ns('list')}', function(e) {{
        const $list = $(this);
        const isDown = (e.key === 'ArrowDown' || e.key === 'j');
        const isUp   = (e.key === 'ArrowUp'   || e.key === 'k');
        if (!(isDown || isUp)) return;
        e.preventDefault();

        const $items = $list.find('.document-summary');
        if ($items.length === 0) return;

        let $current = $items.filter('.border.border-sky-500').first();
        if ($current.length === 0) {{
          $current = $items.first();
        }}

        const $target = isDown ? $current.next('.document-summary') : $current.prev('.document-summary');
        if ($target.length) {{
          $items.removeClass('border border-sky-500');
          $target.addClass('border border-sky-500');
          Shiny.setInputValue('{ns('selected_document')}', parseInt($target.attr('data-document-id')));
        }}
      }});

      // We also add a handler the server can call to update selected document
      // on its own.
      Shiny.addCustomMessageHandler('update_selected_document', function(value) {{
        Shiny.setInputValue('{ns('selected_document')}', value);
      }});
    "
  )))

  shiny::tagList(
    clickHandler,
    shiny::div(
      id = ns("document_list-panel"),
      class = "left-pane flex flex-col gap-2 overflow-x-hidden overflow-y-auto",
      style = "flex: 0 0 var(--left-pane-width); width: var(--left-pane-width); min-width: 200px;",
      shiny::uiOutput(ns("list"), class = "flex flex-col gap-1")
    )
  )
}

listDocumentsServer <- function(id, documents) {
  stopifnot(shiny::is.reactive(documents))
  ns <- \(i) shiny::NS(id, i)

  shiny::moduleServer(id, function(input, output, session) {
    updateSelectedDocument <- function(value) {
      session$sendCustomMessage("update_selected_document", value)
    }

    output$list <- shiny::renderUI({
      if (inherits(documents(), "error")) {
        updateSelectedDocument(NULL)
        return(shiny::tags$div(
          class = "flex flex-col text-sm text-center",
          shiny::p(
            "Error retrieving documents",
            class = "text-red-500 font-bold"
          ),
          shiny::tags$pre(attr(documents(), "error"))
        ))
      }

      if (nrow(documents()) == 0) {
        updateSelectedDocument(NULL)
        return(shiny::tags$div(
          class = "text-sm text-center",
          "No documents found"
        ))
      }

      # Preserve user selection across re-renders where possible.
      # Use module-local input id; do not re-namespace here.
      doc_ids <- documents()$chunk_id
      current <- input$selected_document
      desired <- if (length(doc_ids) == 0) {
        NULL
      } else if (!is.null(current) && current %in% doc_ids) {
        current
      } else {
        doc_ids[[1]]
      }
      if (!identical(current, desired)) {
        updateSelectedDocument(desired)
      }

      summaries <- documents() |>
        dplyr::mutate(.rn = dplyr::row_number()) |>
        dplyr::group_split(.rn) |>
        lapply(
          function(d) {
            documentSummaryUI(
              ns(glue::glue("document-{d$chunk_id}")),
              d,
              active = d$chunk_id == desired
            )
          }
        )

      shiny::tagList(
        if (attr(documents(), "no_filter") %||% FALSE) {
          shiny::tags$div(
            class = "text-sm text-center",
            "No search query, showing first few documents"
          )
        },
        !!!summaries
      )
    })

    shiny::reactive(input$selected_document)
  })
}


#' @param document Is supposed to be a single row returned by ragnar_retrieve_vss
#'   or ragnar_retrieve_bm25.
#' @noRd
documentSummaryUI <- function(id, document, active = FALSE) {
  stopifnot(is.data.frame(document))
  ns <- \(i) shiny::NS(id, i)

  origin <- document$origin
  origin_uri <- tryCatch(
    {
      httr2::url_parse(origin)
      origin
    },
    error = function(e) {
      abs_path <- fs::path_abs(origin)
      glue::glue("file://{abs_path}")
    }
  )

  n_char <- nchar(document$text)

  row_class <- "document-summary flex flex-col bg-gray-100 hover:bg-gray-200 rounded-md w-full text-xs justify-evenly py-2"
  if (isTRUE(active)) {
    row_class <- paste(row_class, "border border-sky-500")
  }

  shiny::div(
    id = ns("summary"),
    "data-document-id" = document$chunk_id,
    class = row_class,
    shiny::div(
      class = "flex flex-row items-center gap-1 py-1 px-2 font-mono text-gray-900 w-full",
      shiny::icon("file", class = "flex-none"),
      shiny::div(
        class = "flex-none font-semibold text-gray-700",
        "origin:"
      ),
      shiny::div(
        class = "flex grow font-mono overflow-hidden",
        shiny::a(
          class = "no-underline hover:underline decoration-sky-500 truncate",
          target = "_blank",
          href = origin_uri,
          origin
        ),
      ),
      shiny::div(
        class = "rounded-full flex-none justify-self-end font-light",
        glue::glue("id: #{document$chunk_id}")
      )
    ),
    if (!is.null(document[["metric_name"]])) {
      shiny::div(
        class = "flex flex-row items-center gap-1 py-1 px-2 font-mono text-gray-500",
        shiny::icon(
          "gauge",
          class = "font-light flex-none"
        ),
        shiny::div(
          class = "flex-none font-bold",
          glue::glue("{document$metric_name}:")
        ),
        shiny::div(
          class = "flex-none font-light",
          round(document$metric_value, 3)
        )
      )
    },
    shiny::div(
      class = "flex flex-row items-center gap-1 py-1 px-2 font-mono text-gray-500",
      shiny::div(
        class = "flex-none font-bold",
        "# characters:"
      ),
      shiny::div(
        class = "flex-none font-light",
        prettyNum(n_char, big.mark = ",")
      ),
      shiny::div(
        class = "font-thin",
        "|"
      ),
      shiny::div(
        class = "flex-none font-bold",
        "# tokens:"
      ),
      shiny::div(
        class = "flex-none font-light",
        glue::glue("~{prettyNum(as.integer(n_char/4), big.mark = ',')}")
      )
    )
  )
}


switchInput <- function(id, switch_values) {
  ns <- \(i) shiny::NS(id, i)
  stopifnot(length(switch_values) <= 2, is.character(switch_values))

  container <- function(...) {
    shiny::div(
      class = "flex flex-row bg-gray-200 rounded-full p-1 gap-1 text-xs",
      shiny::tags$script(shiny::HTML(glue::glue(
        "(function() {{
        function init() {{
          console.log('setting input value');
          Shiny.setInputValue('{ns('value')}', '{switch_values[1]}');
        }}

        function waitForShiny() {{
          if (window.Shiny && Shiny.setInputValue) {{
            init();
          }} else {{
            setTimeout(waitForShiny, 50);
          }}
        }}

        waitForShiny();
      }})()"
      ))),
      ...
    )
  }

  if (length(switch_values) == 1) {
    return(container(
      shiny::tags$button(
        class = ns('btn'),
        class = "rounded-full p-1 px-4 bg-white disabled",
        disabled = TRUE,
        switch_values
      )
    ))
  }

  jsHandler <- function(val) {
    glue::glue(
      "(function() {{
        Shiny.setInputValue('{ns('value')}', '{val}');
        var $element = $('.{ns('btn')}');
        $element.
          toggleClass('bg-white disabled').
          prop('disabled', function(i, v) {{ return !v; }});
    }})();"
    )
  }

  container(
    shiny::tags$button(
      class = ns('btn'),
      class = "rounded-full p-1 px-4 bg-white disabled",
      onClick = jsHandler(switch_values[1]),
      disabled = NA,
      switch_values[1]
    ),
    shiny::tags$button(
      class = ns('btn'),
      class = "rounded-full p-1 px-4",
      onClick = jsHandler(switch_values[2]),
      switch_values[2]
    )
  )
}

switchServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive(input$value)
  })
}
