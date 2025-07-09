# see ragnar_store_inspect() for instructions tousing the app
local({
  source("modules.R", local = TRUE)
  store <- getOption("ragnar_inspector_store", NULL)

  if (is.null(store@embed)) {
    search_types <- "BM25"
  } else {
    search_types <- c("VSS", "BM25")
  }

  ui <- storeInspectorUI("ragnarInspector", search_types)
  server <- function(input, output, session) {
    if (is.null(store)) {
      stop("No store provided")
    }
    storeInspectorServer("ragnarInspector", store)
  }

  shiny::shinyApp(ui, server)
})
