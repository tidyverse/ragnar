# see ragnar_store_inspect() for instructions tousing the app

source("modules.R")

ui <- storeInspectorUI("ragnarInspector")
server <- function(input, output, session) {
  store <- getOption("ragnar_inspector_store", NULL)
  if (is.null(store)) {
    stop("No store provided")
  }
  storeInspectorServer("ragnarInspector", store)
}

shiny::shinyApp(ui, server)
