# webfakes waits only 5s for each app subprocess to start, and slow CI runners
# (notably macOS x86_64) intermittently exceed that.
withr::local_envvar(
  R_WEBFAKES_PROCESS_TIMEOUT = "60000",
  .local_envir = testthat::teardown_env()
)
