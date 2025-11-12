test_that("read_as_markdown() can fetch YouTube transcript text", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci() # youtube api + github runner == :(

  url <- "https://www.youtube.com/watch?v=JELG5jktC5E"

  r_output <- capture.output(
    py_output <- reticulate::py_capture_output(
      md <- read_as_markdown(url)
    )
  )
  expect_equal(r_output, character())
  expect_equal(py_output, "")

  expect_true(S7::S7_inherits(md, MarkdownDocument))
  expect_identical(md@origin, url)

  text <- as.character(md)

  expect_true(stri_detect_fixed(
    text,
    "### Transcript\nEveryone, thanks for coming."
  ))
})

test_that("read_as_markdown() example formatter adds timestamped links", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci() # youtube api + github runner == :(

  url <- "https://www.youtube.com/watch?v=JELG5jktC5E"

  format_youtube_timestamp <- function(time) {
    h <- time %/% 3600
    time <- time %% 3600
    m <- time %/% 60
    time <- time %% 60
    s <- floor(time)
    out <- paste0(h, "h", m, "m", s, "s")
    out <- sub("^0h", "", out)
    out <- sub("^0m", "", out)
    out
  }

  format_transcript_with_timestamps <-
    function(data, min_timestamp_stride_seconds = 30, links = FALSE) {
      ts <- format_youtube_timestamp(data$start)
      if (links) {
        video_id <- attr(data, "youtube_metadata")$video_id
        ts <- sprintf("\n<https://youtu.be/%s?t=%s>\n", video_id, ts)
      } else {
        ts <- sprintf("\n[%s] ", ts)
      }

      if (!is.null(min_timestamp_stride_seconds)) {
        x <- data$start
        show <- c(TRUE, as.logical(diff(x %/% min_timestamp_stride_seconds)))
        ts[!show] <- ""
      }

      paste0(ts, data$text, sep = "", collapse = "\n")
    }

  md <- read_as_markdown(
    url,
    youtube_transcript_formatter = \(data) {
      format_transcript_with_timestamps(data, links = TRUE)
    }
  )

  expect_true(S7::S7_inherits(md, MarkdownDocument))
  expect_identical(md@origin, url)

  text <- as.character(md)
  expect_true(stri_detect_fixed(text, "<https://youtu.be/JELG5jktC5E?t=5s"))
})
