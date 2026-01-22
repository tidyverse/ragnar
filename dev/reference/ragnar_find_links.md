# Find links on a page

Find links on a page

## Usage

``` r
ragnar_find_links(
  x,
  depth = 0L,
  children_only = FALSE,
  progress = TRUE,
  ...,
  url_filter = identity,
  validate = FALSE
)
```

## Arguments

- x:

  URL, HTML file path, or XML document. For Markdown, convert to HTML
  using
  [`commonmark::markdown_html()`](https://docs.ropensci.org/commonmark/reference/commonmark.html)
  first.

- depth:

  Integer specifying how many levels deep to crawl for links. When
  `depth > 0`, the function will follow child links (links with `x` as a
  prefix) and collect links from those pages as well.

- children_only:

  Logical or string. If `TRUE`, returns only child links (those having
  `x` as a prefix). If `FALSE`, returns all links found on the page.
  Note that regardless of this setting, only child links are followed
  when `depth > 0`.

- progress:

  Logical, draw a progress bar if `depth > 0`.

- ...:

  Currently unused. Must be empty.

- url_filter:

  A function that takes a character vector of URL's and may subset them
  to return a smaller list. This can be useful for filtering out URL's
  by rules different than `children_only` which only checks the prefix.

- validate:

  Default is `FALSE`. If `TRUE` sends a `HEAD` request for each link and
  removes those that are not accessible. Requests are sent in parallel
  using
  [`httr2::req_perform_parallel()`](https://httr2.r-lib.org/reference/req_perform_parallel.html).

## Value

A character vector of links on the page.

## Examples

``` r
# \dontrun{
ragnar_find_links("https://r4ds.hadley.nz/base-R.html")
#>  [1] "https://adv-r.hadley.nz/subsetting.html"                        
#>  [2] "https://dplyr.tidyverse.org/reference/across.html"              
#>  [3] "https://dplyr.tidyverse.org/reference/arrange.html"             
#>  [4] "https://dplyr.tidyverse.org/reference/filter.html"              
#>  [5] "https://dplyr.tidyverse.org/reference/group_by.html"            
#>  [6] "https://dplyr.tidyverse.org/reference/mutate.html"              
#>  [7] "https://dplyr.tidyverse.org/reference/pull.html"                
#>  [8] "https://dplyr.tidyverse.org/reference/relocate.html"            
#>  [9] "https://dplyr.tidyverse.org/reference/select.html"              
#> [10] "https://dplyr.tidyverse.org/reference/summarise.html"           
#> [11] "https://gist.github.com/hadley/1986a273e384fb2d4d752c18ed71bedf"
#> [12] "https://gist.github.com/hadley/c430501804349d382ce90754936ab8ec"
#> [13] "https://github.com/hadley/r4ds"                                 
#> [14] "https://github.com/hadley/r4ds/edit/main/base-R.qmd"            
#> [15] "https://github.com/hadley/r4ds/issues/new"                      
#> [16] "https://purrr.tidyverse.org/reference/map.html"                 
#> [17] "https://quarto.org"                                             
#> [18] "https://r4ds.hadley.nz/EDA.html"                                
#> [19] "https://r4ds.hadley.nz/arrow.html"                              
#> [20] "https://r4ds.hadley.nz/base-R.html"                             
#> [21] "https://r4ds.hadley.nz/communicate.html"                        
#> [22] "https://r4ds.hadley.nz/communication.html"                      
#> [23] "https://r4ds.hadley.nz/data-import.html"                        
#> [24] "https://r4ds.hadley.nz/data-tidy.html"                          
#> [25] "https://r4ds.hadley.nz/data-transform.html"                     
#> [26] "https://r4ds.hadley.nz/data-visualize.html"                     
#> [27] "https://r4ds.hadley.nz/databases.html"                          
#> [28] "https://r4ds.hadley.nz/datetimes.html"                          
#> [29] "https://r4ds.hadley.nz/factors.html"                            
#> [30] "https://r4ds.hadley.nz/functions.html"                          
#> [31] "https://r4ds.hadley.nz/import.html"                             
#> [32] "https://r4ds.hadley.nz/intro.html"                              
#> [33] "https://r4ds.hadley.nz/iteration.html"                          
#> [34] "https://r4ds.hadley.nz/joins.html"                              
#> [35] "https://r4ds.hadley.nz/layers.html"                             
#> [36] "https://r4ds.hadley.nz/logicals.html"                           
#> [37] "https://r4ds.hadley.nz/missing-values.html"                     
#> [38] "https://r4ds.hadley.nz/numbers.html"                            
#> [39] "https://r4ds.hadley.nz/preface-2e.html"                         
#> [40] "https://r4ds.hadley.nz/program.html"                            
#> [41] "https://r4ds.hadley.nz/quarto-formats.html"                     
#> [42] "https://r4ds.hadley.nz/quarto.html"                             
#> [43] "https://r4ds.hadley.nz/rectangling.html"                        
#> [44] "https://r4ds.hadley.nz/regexps.html"                            
#> [45] "https://r4ds.hadley.nz/spreadsheets.html"                       
#> [46] "https://r4ds.hadley.nz/strings.html"                            
#> [47] "https://r4ds.hadley.nz/transform.html"                          
#> [48] "https://r4ds.hadley.nz/visualize.html"                          
#> [49] "https://r4ds.hadley.nz/webscraping.html"                        
#> [50] "https://r4ds.hadley.nz/whole-game.html"                         
#> [51] "https://r4ds.hadley.nz/workflow-basics.html"                    
#> [52] "https://r4ds.hadley.nz/workflow-help.html"                      
#> [53] "https://r4ds.hadley.nz/workflow-scripts.html"                   
#> [54] "https://r4ds.hadley.nz/workflow-style.html"                     
#> [55] "https://rdrr.io/r/base/Arithmetic.html"                         
#> [56] "https://rdrr.io/r/base/Extremes.html"                           
#> [57] "https://rdrr.io/r/base/NA.html"                                 
#> [58] "https://rdrr.io/r/base/apply.html"                              
#> [59] "https://rdrr.io/r/base/c.html"                                  
#> [60] "https://rdrr.io/r/base/cbind.html"                              
#> [61] "https://rdrr.io/r/base/data.frame.html"                         
#> [62] "https://rdrr.io/r/base/do.call.html"                            
#> [63] "https://rdrr.io/r/base/lapply.html"                             
#> [64] "https://rdrr.io/r/base/length.html"                             
#> [65] "https://rdrr.io/r/base/levels.html"                             
#> [66] "https://rdrr.io/r/base/library.html"                            
#> [67] "https://rdrr.io/r/base/list.files.html"                         
#> [68] "https://rdrr.io/r/base/list.html"                               
#> [69] "https://rdrr.io/r/base/logical.html"                            
#> [70] "https://rdrr.io/r/base/mean.html"                               
#> [71] "https://rdrr.io/r/base/numeric.html"                            
#> [72] "https://rdrr.io/r/base/order.html"                              
#> [73] "https://rdrr.io/r/base/seq.html"                                
#> [74] "https://rdrr.io/r/base/subset.html"                             
#> [75] "https://rdrr.io/r/base/sum.html"                                
#> [76] "https://rdrr.io/r/base/tapply.html"                             
#> [77] "https://rdrr.io/r/base/transform.html"                          
#> [78] "https://rdrr.io/r/base/vector.html"                             
#> [79] "https://rdrr.io/r/base/which.html"                              
#> [80] "https://rdrr.io/r/base/with.html"                               
#> [81] "https://rdrr.io/r/graphics/hist.html"                           
#> [82] "https://rdrr.io/r/graphics/plot.default.html"                   
#> [83] "https://rdrr.io/r/stats/Uniform.html"                           
#> [84] "https://rdrr.io/r/utils/str.html"                               
#> [85] "https://readxl.tidyverse.org/reference/read_excel.html"         
#> [86] "https://tibble.tidyverse.org/reference/tibble.html"             
#> [87] "https://tidyselect.r-lib.org/reference/starts_with.html"        
#> [88] "https://tidyverse.tidyverse.org"                                
ragnar_find_links("https://ellmer.tidyverse.org/")
#>  [1] "http://schloerke.com"                                                                                  
#>  [2] "https://ai.google.dev/gemini-api/terms"                                                                
#>  [3] "https://cloud.r-project.org/package=ellmer"                                                            
#>  [4] "https://docs.posit.co/connect/user/oauth-integrations"                                                 
#>  [5] "https://docs.posit.co/ide/server-pro/user/posit-workbench/managed-credentials/managed-credentials.html"
#>  [6] "https://ellmer.tidyverse.org"                                                                          
#>  [7] "https://ellmer.tidyverse.org/LICENSE-text.html"                                                        
#>  [8] "https://ellmer.tidyverse.org/LICENSE.html"                                                             
#>  [9] "https://ellmer.tidyverse.org/articles/ellmer.html"                                                     
#> [10] "https://ellmer.tidyverse.org/articles/programming.html"                                                
#> [11] "https://ellmer.tidyverse.org/articles/prompt-design.html"                                              
#> [12] "https://ellmer.tidyverse.org/articles/streaming-async.html"                                            
#> [13] "https://ellmer.tidyverse.org/articles/structured-data.html"                                            
#> [14] "https://ellmer.tidyverse.org/articles/tool-calling.html"                                               
#> [15] "https://ellmer.tidyverse.org/authors.html"                                                             
#> [16] "https://ellmer.tidyverse.org/index.html"                                                               
#> [17] "https://ellmer.tidyverse.org/news/index.html"                                                          
#> [18] "https://ellmer.tidyverse.org/reference/chat-any.html"                                                  
#> [19] "https://ellmer.tidyverse.org/reference/chat_anthropic.html"                                            
#> [20] "https://ellmer.tidyverse.org/reference/chat_aws_bedrock.html"                                          
#> [21] "https://ellmer.tidyverse.org/reference/chat_azure_openai.html"                                         
#> [22] "https://ellmer.tidyverse.org/reference/chat_cloudflare.html"                                           
#> [23] "https://ellmer.tidyverse.org/reference/chat_databricks.html"                                           
#> [24] "https://ellmer.tidyverse.org/reference/chat_deepseek.html"                                             
#> [25] "https://ellmer.tidyverse.org/reference/chat_github.html"                                               
#> [26] "https://ellmer.tidyverse.org/reference/chat_google_gemini.html"                                        
#> [27] "https://ellmer.tidyverse.org/reference/chat_groq.html"                                                 
#> [28] "https://ellmer.tidyverse.org/reference/chat_huggingface.html"                                          
#> [29] "https://ellmer.tidyverse.org/reference/chat_mistral.html"                                              
#> [30] "https://ellmer.tidyverse.org/reference/chat_ollama.html"                                               
#> [31] "https://ellmer.tidyverse.org/reference/chat_openai.html"                                               
#> [32] "https://ellmer.tidyverse.org/reference/chat_openrouter.html"                                           
#> [33] "https://ellmer.tidyverse.org/reference/chat_perplexity.html"                                           
#> [34] "https://ellmer.tidyverse.org/reference/chat_snowflake.html"                                            
#> [35] "https://ellmer.tidyverse.org/reference/chat_vllm.html"                                                 
#> [36] "https://ellmer.tidyverse.org/reference/content_image_url.html"                                         
#> [37] "https://ellmer.tidyverse.org/reference/index.html"                                                     
#> [38] "https://ellmer.tidyverse.org/reference/live_console.html"                                              
#> [39] "https://garrickadenbuie.com"                                                                           
#> [40] "https://github.com/posit-dev/chatlas"                                                                  
#> [41] "https://github.com/tidyverse/ellmer"                                                                   
#> [42] "https://github.com/tidyverse/ellmer/issues"                                                            
#> [43] "https://hadley.nz"                                                                                     
#> [44] "https://ollama.com"                                                                                    
#> [45] "https://opensource.org/licenses/mit-license.php"                                                       
#> [46] "https://orcid.org/0000-0001-9986-114X"                                                                 
#> [47] "https://orcid.org/0000-0002-7111-0077"                                                                 
#> [48] "https://orcid.org/0000-0003-4757-117X"                                                                 
#> [49] "https://pkgdown.r-lib.org"                                                                             
#> [50] "https://posit-dev.github.io/mcptools"                                                                  
#> [51] "https://posit-dev.github.io/shinychat"                                                                 
#> [52] "https://posit.co/blog/announcing-ellmer"                                                               
#> [53] "https://r6.r-lib.org"                                                                                  
#> [54] "https://ragnar.tidyverse.org"                                                                          
#> [55] "https://rdrr.io/r/base/library.html"                                                                   
#> [56] "https://rdrr.io/r/utils/install.packages.html"                                                         
#> [57] "https://rdrr.io/r/utils/str.html"                                                                      
#> [58] "https://ror.org/03wc8by49"                                                                             
#> [59] "https://tidyverse.org/blog/2025/11/ellmer-0-4-0"                                                       
#> [60] "https://vitals.tidyverse.org"                                                                          
#> [61] "https://www.posit.co"                                                                                  
#> [62] "https://www.tidyverse.org/blog/2025/05/ellmer-0-2-0"                                                   
#> [63] "https://www.tidyverse.org/blog/2025/07/ellmer-0-3-0"                                                   
ragnar_find_links(
  paste0("https://github.com/Snowflake-Labs/sfquickstarts/",
         "tree/master/site/sfguides/src/build_a_custom_model_for_anomaly_detection"),
  children_only = "https://github.com/Snowflake-Labs/sfquickstarts",
  depth = 1
)
#> Error in open.connection(con, open = mode): cannot open the connection
# }
```
