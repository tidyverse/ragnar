test_doc <- function() {
  tmp <- tempfile(fileext = ".html")
  
  httr2::request("https://r4ds.hadley.nz/base-R.html") |> 
    httr2::req_cache(path = tempdir()) |> 
    httr2::req_perform(path = tmp)

  tmp
}
