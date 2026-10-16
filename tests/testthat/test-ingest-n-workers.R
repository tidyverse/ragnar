test_that("default workers respect core counts and workload size", {
  skip_if_cant_load_duckdb_extensions()

  store <- ragnar_store_create(embed = NULL)
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))

  cases <- data.frame(
    n_cores = c(2L, 32L, 16L, 8L, 1L, 4L, NA_integer_, NA_integer_),
    n_paths = c(40L, 100L, 3L, 12L, 40L, 20L, 40L, 12L),
    expected = c(2L, 8L, 2L, 3L, 2L, 4L, 4L, 3L)
  )

  for (i in seq_len(nrow(cases))) {
    local({
      case <- cases[i, ]
      local_mocked_bindings(
        detectCores = function(...) case$n_cores,
        .package = "parallel"
      )
      local_mocked_bindings(
        daemons = function(n, ...) {
          rlang::abort(
            "Worker count captured",
            class = "test_worker_count",
            n = n
          )
        },
        .package = "mirai"
      )

      condition <- expect_error(
        ragnar_store_ingest(
          store,
          paths = paste0("page-", seq_len(case$n_paths)),
          progress = FALSE,
          build_index = FALSE
        ),
        class = "test_worker_count"
      )
      expect_equal(condition$n, case$expected, info = paste("case", i))
    })
  }
})
