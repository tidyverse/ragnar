# ragnar (development version)

* Changes to `read_as_markdown()` HTML conversion (#40):

  * If a 'main' tag is present, content outside the 'main' tag is now excluded 
    by default. To restore the previous behavior and include the sidebar, header,
    footer, and other navigational elements in the converted markdown, use
    `read_as_markdown(x, main_only=FALSE)`.
    
  * Fixed handling of nested code fences in markdown output.

# ragnar 0.1.0

* Initial CRAN submission.
