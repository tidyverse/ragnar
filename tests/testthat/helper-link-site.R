local_link_site <- function(
  pages,
  max_requests = Inf,
  status_codes = integer(),
  .local_envir = parent.frame()
) {
  app <- webfakes::new_app()
  app$locals$pages <- pages
  app$locals$status_codes <- status_codes
  app$locals$requests <- data.frame(method = character(), path = character())
  app$locals$max_requests <- max_requests
  app$use(function(req, res, locals) {
    if (req$path == "/requests") {
      return(res$send_json(locals$requests))
    }
    locals$requests <- rbind(
      locals$requests,
      data.frame(method = req$method, path = req$path)
    )
    # Stop broken redirect tests at the server, before exhausting R's stack.
    if (nrow(locals$requests) > locals$max_requests) {
      return(res$send_status(508))
    }
    res$set_header("Last-Modified", "Wed, 21 Oct 2015 07:28:00 GMT")
    if (req$path %in% names(locals$status_codes)) {
      return(res$send_status(locals$status_codes[[req$path]]))
    }
    res$send(locals$pages[[req$path]])
  })
  server <- webfakes::new_app_process(app)
  withr::defer(server$stop(), envir = .local_envir)
  server
}
