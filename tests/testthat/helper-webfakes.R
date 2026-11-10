# Start a webfakes app in a subprocess for the calling test. Skipped on CRAN:
# webfakes starts the subprocess with callr's fixed 3s startup timeout, which a
# loaded check machine can exceed.
local_app_process <- function(app, .local_envir = parent.frame()) {
  skip_on_cran()
  server <- webfakes::new_app_process(app)
  withr::defer(server$stop(), envir = .local_envir)
  server
}
