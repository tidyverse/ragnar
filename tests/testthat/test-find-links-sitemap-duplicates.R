test_that("duplicate sitemap locations retain their first known date", {
  app <- webfakes::new_app()
  app$locals$requests <- data.frame(method = character(), path = character())
  app$use(function(req, res, locals) {
    if (req$path == "/requests") {
      return(res$send_json(locals$requests))
    }
    locals$requests <- rbind(
      locals$requests,
      data.frame(method = req$method, path = req$path)
    )
    if (req$path == "/sitemap.xml") {
      root <- paste0("http://", req$get_header("host"))
      paths <- c(
        "",
        "/",
        "/invalid#old",
        "/invalid#new",
        "/same",
        "/same",
        "/known",
        "/known",
        "/unknown",
        "/unknown"
      )
      dates <- c(
        NA,
        "2024-03-15",
        "invalid",
        "2024-04-16",
        NA,
        "2024-05-17",
        "2024-06-18",
        "2024-07-19",
        NA,
        "invalid"
      )
      lastmod <- ifelse(
        is.na(dates),
        "",
        paste0("<lastmod>", dates, "</lastmod>")
      )
      return(res$send(paste0(
        "<urlset>",
        paste0(
          "<url><loc>",
          root,
          paths,
          "</loc>",
          lastmod,
          "</url>",
          collapse = ""
        ),
        "</urlset>"
      )))
    }
    res$set_header("Last-Modified", "Wed, 21 Oct 2015 07:28:00 GMT")
    res$send("<p>Page</p>")
  })
  site <- webfakes::new_app_process(app)
  withr::defer(site$stop())

  expected <- tibble(
    source = site$url(c("/", "/invalid", "/known", "/same", "/unknown")),
    last_modified = as.POSIXct(
      c("2024-03-15", "2024-04-16", "2024-06-18", "2024-05-17", NA),
      tz = "UTC"
    )
  )
  expect_equal(
    ragnar_find_links(site$url("/sitemap.xml"), metadata = TRUE),
    expected
  )

  expected$last_modified[[5L]] <- as.POSIXct("2015-10-21 07:28:00", tz = "UTC")
  expect_equal(
    ragnar_find_links(
      site$url("/sitemap.xml"),
      metadata = TRUE,
      get_timestamps = TRUE
    ),
    expected
  )
  requests <- httr2::request(site$url("/requests")) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
  expect_equal(requests$path[requests$method == "head"], "/unknown")
})
