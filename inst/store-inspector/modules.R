storeInspectorUI <- function(id) {
  ns <- \(i) shiny::NS(id, i)

  shiny::tags$html(
    id = ns("html"),
    shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "app.out.css"
      ),
    ),
    shiny::tags$body(
      class = "flex flex-col max-h-screen min-h-screen h-full",
      shiny::div(
        class = "flex-none bg-blue-500 p-2 gap-2",
        shiny::div(
          "Ragnar Store Inspector",
          class = "flex-none text-lg text-white"
        ),
      ),
      shiny::div(
        class = "flex flex-row flex-none bg-yellow p-2 justify-center text-sm gap-2 items-center",
        shiny::div(
          class = "flex grow max-w-96 shadow-md p-2 rounded-full bg-gray-100 items-center gap-2 focus-within:shadow-blue-500/50",
          shiny::icon(
            name = "magnifying-glass",
            class = "flex flex-none text-gray-400"
          ),
          shiny::tags$input(
            class = "flex-grow bg-transparent outline-none",
            id = ns("query"),
            type = "search",
            placeholder = "Search the store ..."
          )
        ),
        switchInput(ns("search_type"), "VSS", "BM25")
      ),
      shiny::div(
        class = "flex grow p-2 gap-2 h-full overflow-hidden",
        listDocumentsUI(ns("document_list")),
        shiny::div(
          class = "flex flex-col gap-2 basis-1/2 overflow-auto",
          shiny::div(
            class = "flex flex-row justify-between pr-1 border-b pb-2 border-gray-200 items-center gap-1",
            shiny::h3("Document preview", class = "text-md font-mono"),
            switchInput(ns("markdown"), "Preview", "Raw Text")
          ),
          shiny::uiOutput(
            class = "h-full",
            ns("preview")
          )
        )
      )
    )
  )
}

storeInspectorServer <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    query <- shiny::debounce(shiny::reactive(input$query), 1000)
    is_vss <- switchServer("search_type")

    documents <- shiny::reactive({
      if (is.null(query()) || nchar(query()) <= 0) {
        return(data.frame())
      }

      tryCatch(
        {
          if (is_vss()) {
            ragnar::ragnar_retrieve_vss(store, query(), top_k = 10)
          } else {
            ragnar::ragnar_retrieve_bm25(store, query(), top_k = 10)
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
    selectedDocumentText <- shiny::reactive({
      if (is.null(selectedDocumentId())) {
        return(NULL)
      }
      docs <- documents()
      docs$text[docs$id == selectedDocumentId()]
    })
    is_markdown <- switchServer("markdown")

    output$preview <- shiny::renderUI({
      if (is.null(selectedDocumentText())) {
        return(tags$div("Select a document to preview"))
      }

      if (is_markdown()) {
        shiny::tags$iframe(
          class = "size-full",
          srcdoc = shiny::markdown(selectedDocumentText())
        )
      } else {
        shiny::tags$pre(
          class = "text-xs",
          selectedDocumentText()
        )
      }
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
      class = "flex flex-col gap-2 basis-1/2 overflow-x-hidden overflow-y-auto",
      shiny::div(
        class = "flex flex-row justify-between pr-1 border-b pb-2 border-gray-200 items-center gap-1",
        shiny::h3("Documents", class = "text-md font-mono p-1")
      ),
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

      updateSelectedDocument(head(documents(), 1)$id)
      summaries <- documents() |>
        dplyr::mutate(.rn = dplyr::row_number()) |>
        dplyr::group_split(.rn) |>
        lapply(
          function(d)
            documentSummaryUI(
              ns(glue::glue("document-{d$id}")),
              d,
              active = d$.rn == 1
            )
        )
      shiny::tagList(!!!summaries)
    })

    shiny::reactive(input$selected_document)
  })
}


#' @param document Is supposed to be a single row returned by ragnar_retrive_vss
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

  shiny::div(
    id = ns("summary"),
    "data-document-id" = document$id,
    class = "document-summary flex flex-col bg-gray-100 hover:bg-gray-200 rounded-md w-full text-xs justify-evenly py-2",
    class = if (active) "border border-sky-500" else NULL, # two class fields are concatenated.
    div(
      class = "flex flex-row items-center gap-1 py-1 px-2 font-mono text-gray-900 w-full",
      icon("file", class = "flex-none"),
      div(
        class = "flex-none font-semibold text-gray-700",
        "origin:"
      ),
      div(
        class = "flex grow font-mono overflow-hidden",
        a(
          class = "no-underline hover:underline decoration-sky-500 truncate",
          target = "_blank",
          href = origin_uri,
          origin
        ),
      ),
      div(
        class = "rounded-full flex-none justify-self-end font-light",
        glue::glue("id: #{document$id}")
      )
    ),
    div(
      class = "flex flex-row items-center gap-1 py-1 px-2 font-mono text-gray-500",
      icon(
        "gauge",
        class = "font-light flex-none"
      ),
      div(
        class = "flex-none font-bold",
        glue::glue("{document$metric_name}:")
      ),
      div(
        class = "flex-none font-light",
        round(document$metric_value, 3)
      )
    ),
    div(
      class = "flex flex-rows items-center gap-1 py-1 px-2 font-mono text-gray 500",
      div(
        class = "flex-none font-bol",
        "# characters:"
      ),
      div(
        class = "flex-none font-light",
        prettyNum(n_char, big.mark = ",")
      ),
      div(
        class = "font-thin",
        "|"
      ),
      div(
        class = "flex-none font-bol",
        "# tokens:"
      ),
      div(
        class = "flex-none font-light",
        glue::glue("~{as.integer(prettyNum(n_char/4, big.mark = ','))}")
      )
    )
  )
}


switchInput <- function(id, trueLabel, falseLabel) {
  ns <- \(i) shiny::NS(id, i)

  jsHandler <- function(val) {
    glue::glue(
      "(function() {{
        Shiny.setInputValue('{ns('value')}', {val});
        var $element = $('.{ns('btn')}');
        $element.
          toggleClass('bg-white disabled').
          prop('disabled', function(i, v) {{ return !v; }});
    }})();"
    )
  }

  shiny::div(
    class = "flex flex-row bg-gray-200 rounded-full p-1 gap-1 text-xs",
    shiny::tags$button(
      class = ns('btn'),
      class = "rounded-full p-1 px-4 bg-white disabled",
      onClick = jsHandler('true'),
      disabled = NA,
      trueLabel
    ),
    shiny::tags$button(
      class = ns('btn'),
      class = "rounded-full p-1 px-4",
      onClick = jsHandler('false'),
      falseLabel
    )
  )
}

switchServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive(input$value %||% TRUE)
  })
}
