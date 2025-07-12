test_that("ragnar_read", {
  skip_on_cran()
  # There is no way to test any functions that call markitdown.convert() on
  # CRAN because the python module 'markitdown' calls magika (to infer file
  # type), which runs a small deep learning model using onnxruntime, which
  # we have no way to reach in and limit threads on. This means we can't
  # test 'read_as_markdown()' or 'ragnar_read()'. Otherwise CRAN complains:
  # > Running R code in 'testthat.R' had CPU time 2.6 times elapsed time
  # So we skip on CRAN :(
  doc <- test_doc()

  # Reading the document without any arguments yields a single row data frame
  document <- ragnar_read(doc)
  expect_equal(nrow(document), 1)
  expect_equal(colnames(document), c("origin", "hash", "text"))
})


test_that("ragnar_read() empty doc", {
  skip_on_cran() # See comment (above) in test-read-markdown.R
  jpg <- file.path(R.home("doc"), "html", "logo.jpg")
  expect_no_error(ragnar_read(jpg))
})

test_that("ragnar_read() doc in ~", {
  skip_on_cran() # See comment (above) in test-read-markdown.R
  withr::with_tempfile("tilde_file", tmpdir = "~", fileext = ".md", {
    file.copy(
      system.file("store-inspector", "README.md", package = "ragnar"),
      tilde_file
    )
    expect_no_error(ragnar_read(tilde_file))
  })
})


test_that("markitdown patches", {
  skip_if_offline()
  skip_on_cran() # See comment (above) in test-read-markdown.R
  url = "https://quarto.org/docs/computations/r.html"
  htmlfile <- withr::local_tempfile(fileext = ".html")
  download.file(url, htmlfile, quiet = TRUE)

  # ensure that 'main_only' returns a shorter document (omitting sidebar)
  main_only <- read_as_markdown(htmlfile, main_only = TRUE)
  not_main_only <- read_as_markdown(htmlfile, main_only = FALSE)
  expect_lt(stri_length(main_only), stri_length(not_main_only))

  # ensure that 'main_only' is a strict slice of the full converted document
  # (modulo the page title)
  main_only_sans_title <- stri_replace_first_regex(main_only, "^# .*\n+", "")
  expect_true(stri_detect_fixed(not_main_only, main_only_sans_title))

  # ensure that some fences have been expanded
  expect_true(stri_detect_fixed(main_only, "````"))
  expect_true(stri_detect_fixed(not_main_only, "````"))
})
