# legacy chunking functions are deprecated

    Code
      x <- ragnar_chunk(text, max_size = 20)
    Condition
      Warning:
      `ragnar_chunk()` was deprecated in ragnar 0.2.0.
      i Please use `markdown_chunk()` instead.
    Code
      x <- ragnar_segment(text)
    Condition
      Warning:
      `ragnar_segment()` was deprecated in ragnar 0.2.0.
      i Please use `markdown_chunk()` instead.
    Code
      x <- ragnar_chunk_segments(c("First. ", "Second. "), max_size = 20)
    Condition
      Warning:
      `ragnar_chunk_segments()` was deprecated in ragnar 0.2.0.
      i Please use `markdown_chunk()` instead.

