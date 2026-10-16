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
  validate = FALSE,
  get_timestamps = FALSE,
  include_root = FALSE
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

  Default is `FALSE`. If `TRUE`, checks that local files exist and sends
  a `HEAD` request for each remote link, removing those that are not
  accessible. Requests are sent in parallel using
  [`httr2::req_perform_parallel()`](https://httr2.r-lib.org/reference/req_perform_parallel.html).

- get_timestamps:

  Default is `FALSE`. If `TRUE`, sends parallel `HEAD` requests for any
  discovered URLs that were not visited during crawling, and records
  their `Last-Modified` response header. Pages fetched during crawling
  already have their timestamp from the `GET` response at no extra cost.
  Useful for incremental ingest: callers can skip re-fetching pages
  whose timestamp has not changed since the last run. Note that not all
  hosting platforms set `Last-Modified`; S3-backed sites (including many
  `docs.posit.co` pages) and GitHub Pages do, while Netlify-hosted sites
  (e.g. `quarto.org`) do not. Missing headers and local file timestamps
  are recorded as `NA`.

- include_root:

  Default is `FALSE`. If `TRUE`, also includes the starting page in the
  results, subject to `children_only`, `url_filter`, and `validate`.
  Sitemap inputs contribute only their listed URLs. If `FALSE`, the
  starting page can still be returned when it is found through a link.

## Value

A `ragnar_links` object: a character vector of URLs, sorted
alphabetically and deduplicated, with the following attributes:

- `last_modified`:

  A named `POSIXct` vector mapping each crawled URL to its
  `Last-Modified` timestamp. Always present. URLs for which the server
  did not return a timestamp (either because the header was absent or
  `get_timestamps = FALSE` and the page was not visited) are `NA`.

- `problems`:

  A data frame with columns `link` and `problem`, present only when one
  or more URLs could not be fetched during crawling.

Joins and row-binding combine links as ordinary character vectors. To
keep timestamps in the combined data, store them in a separate column
first.

## Examples

``` r
# \dontrun{
ragnar_find_links("https://r4ds.hadley.nz/base-R.html")
#> # A ragnar_links vector: 88 URLs
#>    url                                                  last_modified
#> 1  https://adv-r.hadley.nz/subsetting.html              <NA>         
#> 2  https://dplyr.tidyverse.org/reference/across.html    <NA>         
#> 3  https://dplyr.tidyverse.org/reference/arrange.html   <NA>         
#> 4  https://dplyr.tidyverse.org/reference/filter.html    <NA>         
#> 5  https://dplyr.tidyverse.org/reference/group_by.html  <NA>         
#> 6  https://dplyr.tidyverse.org/reference/mutate.html    <NA>         
#> 7  https://dplyr.tidyverse.org/reference/pull.html      <NA>         
#> 8  https://dplyr.tidyverse.org/reference/relocate.html  <NA>         
#> 9  https://dplyr.tidyverse.org/reference/select.html    <NA>         
#> 10 https://dplyr.tidyverse.org/reference/summarise.html <NA>         
#> # ... with 78 more rows
ragnar_find_links("https://ellmer.tidyverse.org/")
#> # A ragnar_links vector: 67 URLs
#>    url                                                                                                   
#> 1  http://schloerke.com/                                                                                 
#> 2  https://ai.google.dev/gemini-api/terms                                                                
#> 3  https://cloud.r-project.org/package=ellmer                                                            
#> 4  https://docs.posit.co/connect/user/oauth-integrations/                                                
#> 5  https://docs.posit.co/ide/server-pro/user/posit-workbench/managed-credentials/managed-credentials.html
#> 6  https://ellmer.tidyverse.org/                                                                         
#> 7  https://ellmer.tidyverse.org/LICENSE-text.html                                                        
#> 8  https://ellmer.tidyverse.org/LICENSE.html                                                             
#> 9  https://ellmer.tidyverse.org/articles/ellmer.html                                                     
#> 10 https://ellmer.tidyverse.org/articles/programming.html                                                
#>    last_modified      
#> 1  <NA>               
#> 2  <NA>               
#> 3  <NA>               
#> 4  <NA>               
#> 5  <NA>               
#> 6  2026-10-15 16:18:20
#> 7  <NA>               
#> 8  <NA>               
#> 9  <NA>               
#> 10 <NA>               
#> # ... with 57 more rows
# Include the starting page when collecting documents for ingestion
ragnar_find_links("https://ellmer.tidyverse.org/", include_root = TRUE)
#> # A ragnar_links vector: 67 URLs
#>    url                                                                                                   
#> 1  http://schloerke.com/                                                                                 
#> 2  https://ai.google.dev/gemini-api/terms                                                                
#> 3  https://cloud.r-project.org/package=ellmer                                                            
#> 4  https://docs.posit.co/connect/user/oauth-integrations/                                                
#> 5  https://docs.posit.co/ide/server-pro/user/posit-workbench/managed-credentials/managed-credentials.html
#> 6  https://ellmer.tidyverse.org/                                                                         
#> 7  https://ellmer.tidyverse.org/LICENSE-text.html                                                        
#> 8  https://ellmer.tidyverse.org/LICENSE.html                                                             
#> 9  https://ellmer.tidyverse.org/articles/ellmer.html                                                     
#> 10 https://ellmer.tidyverse.org/articles/programming.html                                                
#>    last_modified      
#> 1  <NA>               
#> 2  <NA>               
#> 3  <NA>               
#> 4  <NA>               
#> 5  <NA>               
#> 6  2026-10-15 16:18:20
#> 7  <NA>               
#> 8  <NA>               
#> 9  <NA>               
#> 10 <NA>               
#> # ... with 57 more rows
ragnar_find_links(
  paste0("https://github.com/Snowflake-Labs/sfquickstarts/",
         "tree/master/site/sfguides/src/build_a_custom_model_for_anomaly_detection"),
  children_only = "https://github.com/Snowflake-Labs/sfquickstarts",
  depth = 1
)
#> Error in open.connection(con, open = mode): cannot open the connection

# Retrieve Last-Modified timestamps for all discovered URLs
links <- ragnar_find_links(
  "https://docs.posit.co/",
  children_only = TRUE,
  depth = 1,
  get_timestamps = TRUE
)
#> ⠙ Finding links: 51 | On queue: 0 | Current depth: 1 | [2s]
#> [working] (623 + 0) -> 6 -> 482 | ■■■■■■■■■■■■■■                    43%
#> [working] (0 + 0) -> 0 -> 1111 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%
#> ⠙ Finding links: 51 | On queue: 0 | Current depth: 1 | [2s]
attr(links, "last_modified")
#>                                                                                                                  https://docs.posit.co/ 
#>                                                                                                               "2026-10-15 18:37:04 UTC" 
#>                                                                                                         https://docs.posit.co/chronicle 
#>                                                                                                               "2026-09-22 13:51:33 UTC" 
#>                                                                                                        https://docs.posit.co/chronicle/ 
#>                                                                                                               "2026-09-22 13:51:33 UTC" 
#>                                                                           https://docs.posit.co/chronicle/getting-started/installation/ 
#>                                                                                                               "2026-09-22 13:51:32 UTC" 
#>                                                                       https://docs.posit.co/chronicle/getting-started/requirements.html 
#>                                                                                                               "2026-09-22 13:51:32 UTC" 
#>                                                                                     https://docs.posit.co/chronicle/news/whats-new.html 
#>                                                                                                               "2026-09-22 13:51:33 UTC" 
#>                                                                                                             https://docs.posit.co/cloud 
#>                                                                                                               "2026-09-29 21:31:25 UTC" 
#>                                                                                         https://docs.posit.co/cloud-products/index.html 
#>                                                                                                               "2026-10-15 18:28:59 UTC" 
#>                                                                                               https://docs.posit.co/concepts/index.html 
#>                                                                                                               "2026-10-15 18:28:59 UTC" 
#>                                                                                 https://docs.posit.co/concepts/managed-credentials.html 
#>                                                                                                               "2026-10-15 18:28:59 UTC" 
#>                                                                                                    https://docs.posit.co/connect-cloud/ 
#>                                                                                                               "2026-10-16 14:02:42 UTC" 
#>                                                                                                          https://docs.posit.co/connect/ 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                   https://docs.posit.co/connect/2024.04.0/admin/appendix/configuration/ 
#>                                                                                                               "2024-04-29 21:37:21 UTC" 
#>                                                                                                https://docs.posit.co/connect/2026.03.1/ 
#>                                                                                                               "2026-04-02 22:02:34 UTC" 
#>                                                                                                    https://docs.posit.co/connect/admin/ 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                                          https://docs.posit.co/connect/admin/access-controls/index.html 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                                   https://docs.posit.co/connect/admin/appendix/advanced-ldap/index.html 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                              https://docs.posit.co/connect/admin/appendix/advanced-programmatic-provisioning/index.html 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                             https://docs.posit.co/connect/admin/appendix/advanced-user-group/index.html 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                                                 https://docs.posit.co/connect/admin/appendix/airgapped/ 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                                       https://docs.posit.co/connect/admin/appendix/airgapped/index.html 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                                        https://docs.posit.co/connect/admin/appendix/branding/index.html 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                                                       https://docs.posit.co/connect/admin/appendix/cli/ 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                                             https://docs.posit.co/connect/admin/appendix/cli/index.html 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                                             https://docs.posit.co/connect/admin/appendix/configuration/ 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                                   https://docs.posit.co/connect/admin/appendix/configuration/index.html 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                                  https://docs.posit.co/connect/admin/appendix/custom-landing/index.html 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                                https://docs.posit.co/connect/admin/appendix/custom-messaging/index.html 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                                    https://docs.posit.co/connect/admin/appendix/off-host/arch-overview/ 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                          https://docs.posit.co/connect/admin/appendix/off-host/arch-overview/index.html 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                                      https://docs.posit.co/connect/admin/appendix/off-host/autoscaling/ 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                            https://docs.posit.co/connect/admin/appendix/off-host/autoscaling/index.html 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                https://docs.posit.co/connect/admin/appendix/off-host/execution-environment-permissions/ 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                      https://docs.posit.co/connect/admin/appendix/off-host/execution-environment-permissions/index.html 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                              https://docs.posit.co/connect/admin/appendix/off-host/execution-environment-volume-mounts/ 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                    https://docs.posit.co/connect/admin/appendix/off-host/execution-environment-volume-mounts/index.html 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                           https://docs.posit.co/connect/admin/appendix/off-host/execution-environments/ 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                 https://docs.posit.co/connect/admin/appendix/off-host/execution-environments/index.html 
#>                                                                                                               "2026-10-13 16:17:43 UTC" 
#>                                                         https://docs.posit.co/connect/admin/appendix/off-host/helm-reference/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                         https://docs.posit.co/connect/admin/appendix/off-host/resource-bases/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                 https://docs.posit.co/connect/admin/appendix/off-host/service-accounts/ 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                       https://docs.posit.co/connect/admin/appendix/off-host/service-accounts/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                https://docs.posit.co/connect/admin/appendix/off-host/support/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                  https://docs.posit.co/connect/admin/appendix/off-host/temporary-dns-records/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                                      https://docs.posit.co/connect/admin/appendix/scim/ 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                    https://docs.posit.co/connect/admin/appendix/scim/api-reference.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                            https://docs.posit.co/connect/admin/appendix/scim/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                           https://docs.posit.co/connect/admin/appendix/troubleshooting/ 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                 https://docs.posit.co/connect/admin/appendix/troubleshooting/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                                    https://docs.posit.co/connect/admin/auditing/events/ 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                          https://docs.posit.co/connect/admin/auditing/events/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                                 https://docs.posit.co/connect/admin/auditing/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                                     https://docs.posit.co/connect/admin/authentication/ 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                 https://docs.posit.co/connect/admin/authentication/identity-federation/ 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                       https://docs.posit.co/connect/admin/authentication/identity-federation/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                           https://docs.posit.co/connect/admin/authentication/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                             https://docs.posit.co/connect/admin/authentication/ldap-based/active-directory-double-bind/ 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                   https://docs.posit.co/connect/admin/authentication/ldap-based/active-directory-double-bind/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                   https://docs.posit.co/connect/admin/authentication/ldap-based/active-directory-single-bind/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                               https://docs.posit.co/connect/admin/authentication/ldap-based/ldap-double-bind/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                               https://docs.posit.co/connect/admin/authentication/ldap-based/ldap-single-bind/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                 https://docs.posit.co/connect/admin/authentication/migration/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                       https://docs.posit.co/connect/admin/authentication/oauth-clients/ 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                             https://docs.posit.co/connect/admin/authentication/oauth-clients/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                               https://docs.posit.co/connect/admin/authentication/oauth2-openid-based/entra-id-openid-connect/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                https://docs.posit.co/connect/admin/authentication/oauth2-openid-based/google/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                   https://docs.posit.co/connect/admin/authentication/oauth2-openid-based/okta-openid-connect/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                               https://docs.posit.co/connect/admin/authentication/oauth2-openid-based/onelogin-openid-connect/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                        https://docs.posit.co/connect/admin/authentication/oauth2-openid-based/openid-connect/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                        https://docs.posit.co/connect/admin/authentication/pam-based/kerberos/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                             https://docs.posit.co/connect/admin/authentication/pam-based/pam/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                            https://docs.posit.co/connect/admin/authentication/password/ 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                  https://docs.posit.co/connect/admin/authentication/password/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                   https://docs.posit.co/connect/admin/authentication/proxied/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                  https://docs.posit.co/connect/admin/authentication/saml-based/entra-id-saml/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                      https://docs.posit.co/connect/admin/authentication/saml-based/jumpcloud/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                      https://docs.posit.co/connect/admin/authentication/saml-based/okta-saml/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                  https://docs.posit.co/connect/admin/authentication/saml-based/onelogin-saml/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                           https://docs.posit.co/connect/admin/authentication/saml-based/saml/index.html 
#>                                                                                                               "2026-10-13 16:17:44 UTC" 
#>                                                                                          https://docs.posit.co/connect/admin/chronicle/ 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                                https://docs.posit.co/connect/admin/chronicle/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                                    https://docs.posit.co/connect/admin/connect-gallery/ 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                          https://docs.posit.co/connect/admin/connect-gallery/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                              https://docs.posit.co/connect/admin/connect-mcp/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                              https://docs.posit.co/connect/admin/content-management/api-keys/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                      https://docs.posit.co/connect/admin/content-management/git-backed/ 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                            https://docs.posit.co/connect/admin/content-management/git-backed/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                       https://docs.posit.co/connect/admin/content-management/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                                 https://docs.posit.co/connect/admin/database/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                                 https://docs.posit.co/connect/admin/database/migration/ 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                       https://docs.posit.co/connect/admin/database/migration/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                                  https://docs.posit.co/connect/admin/database/postgres/ 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                        https://docs.posit.co/connect/admin/database/postgres/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                          https://docs.posit.co/connect/admin/database/sqlite/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                              https://docs.posit.co/connect/admin/directories/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                                              https://docs.posit.co/connect/admin/email/ 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                                    https://docs.posit.co/connect/admin/email/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                                    https://docs.posit.co/connect/admin/getting-started/ 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                          https://docs.posit.co/connect/admin/getting-started/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                    https://docs.posit.co/connect/admin/getting-started/initial-configuration/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                      https://docs.posit.co/connect/admin/getting-started/initial-configuration/network-ports/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                   https://docs.posit.co/connect/admin/getting-started/initial-configuration/ssl-certificates/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                            https://docs.posit.co/connect/admin/getting-started/local-install/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                  https://docs.posit.co/connect/admin/getting-started/local-install/initial-configuration/network-ports/ 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                               https://docs.posit.co/connect/admin/getting-started/local-install/initial-configuration/ssl-certificates/ 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                       https://docs.posit.co/connect/admin/getting-started/local-install/manual-install/ 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                               https://docs.posit.co/connect/admin/getting-started/local-install/multi-server/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                             https://docs.posit.co/connect/admin/getting-started/local-install/server-install/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                   https://docs.posit.co/connect/admin/getting-started/off-host-install/ 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                    https://docs.posit.co/connect/admin/getting-started/off-host-install/configure-helm-chart/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                         https://docs.posit.co/connect/admin/getting-started/off-host-install/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                          https://docs.posit.co/connect/admin/getting-started/off-host-install/kubernetes-cluster-preparation/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                   https://docs.posit.co/connect/admin/getting-started/off-host-install/kubernetes-deployment/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                              https://docs.posit.co/connect/admin/getting-started/off-host-install/kubernetes-post-deployment/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                       https://docs.posit.co/connect/admin/getting-started/requirements/ 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                             https://docs.posit.co/connect/admin/getting-started/requirements/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                   https://docs.posit.co/connect/admin/getting-started/sizing/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                                   https://docs.posit.co/connect/admin/group-management/ 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                         https://docs.posit.co/connect/admin/group-management/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                   https://docs.posit.co/connect/admin/historical-information/index.html 
#>                                                                                                               "2026-10-13 16:17:45 UTC" 
#>                                                                                          https://docs.posit.co/connect/admin/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                              https://docs.posit.co/connect/admin/integrations/audit-alerting/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                                          https://docs.posit.co/connect/admin/integrations/data-sources/ 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                                https://docs.posit.co/connect/admin/integrations/data-sources/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                                         https://docs.posit.co/connect/admin/integrations/efs/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                            https://docs.posit.co/connect/admin/integrations/git-repositories/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                                    https://docs.posit.co/connect/admin/integrations/oauth-integrations/ 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                          https://docs.posit.co/connect/admin/integrations/oauth-integrations/anthropic/ 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                https://docs.posit.co/connect/admin/integrations/oauth-integrations/anthropic/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                                https://docs.posit.co/connect/admin/integrations/oauth-integrations/aws/ 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                      https://docs.posit.co/connect/admin/integrations/oauth-integrations/aws/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                            https://docs.posit.co/connect/admin/integrations/oauth-integrations/connect/ 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                  https://docs.posit.co/connect/admin/integrations/oauth-integrations/connect/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                      https://docs.posit.co/connect/admin/integrations/oauth-integrations/credential-delivery/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                   https://docs.posit.co/connect/admin/integrations/oauth-integrations/custom/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                         https://docs.posit.co/connect/admin/integrations/oauth-integrations/databricks/ 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                               https://docs.posit.co/connect/admin/integrations/oauth-integrations/databricks/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                      https://docs.posit.co/connect/admin/integrations/oauth-integrations/faq/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                              https://docs.posit.co/connect/admin/integrations/oauth-integrations/github 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                             https://docs.posit.co/connect/admin/integrations/oauth-integrations/github/ 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                   https://docs.posit.co/connect/admin/integrations/oauth-integrations/github/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                       https://docs.posit.co/connect/admin/integrations/oauth-integrations/google-cloud/ 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                             https://docs.posit.co/connect/admin/integrations/oauth-integrations/google-cloud/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                             https://docs.posit.co/connect/admin/integrations/oauth-integrations/google/ 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                   https://docs.posit.co/connect/admin/integrations/oauth-integrations/google/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                          https://docs.posit.co/connect/admin/integrations/oauth-integrations/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                          https://docs.posit.co/connect/admin/integrations/oauth-integrations/microsoft/ 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                https://docs.posit.co/connect/admin/integrations/oauth-integrations/microsoft/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                             https://docs.posit.co/connect/admin/integrations/oauth-integrations/openai/ 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                   https://docs.posit.co/connect/admin/integrations/oauth-integrations/openai/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                 https://docs.posit.co/connect/admin/integrations/oauth-integrations/palantir/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                         https://docs.posit.co/connect/admin/integrations/oauth-integrations/salesforce/ 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                               https://docs.posit.co/connect/admin/integrations/oauth-integrations/salesforce/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                            https://docs.posit.co/connect/admin/integrations/oauth-integrations/security 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                 https://docs.posit.co/connect/admin/integrations/oauth-integrations/security/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                          https://docs.posit.co/connect/admin/integrations/oauth-integrations/snowflake/ 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                                https://docs.posit.co/connect/admin/integrations/oauth-integrations/snowflake/index.html 
#>                                                                                                               "2026-10-13 16:17:46 UTC" 
#>                                          https://docs.posit.co/connect/admin/integrations/oauth-integrations/troubleshooting/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                              https://docs.posit.co/connect/admin/integrations/oauth-integrations/vault/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                    https://docs.posit.co/connect/admin/integrations/oauth-integrations/vault/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                       https://docs.posit.co/connect/admin/integrations/package-manager/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                             https://docs.posit.co/connect/admin/integrations/package-manager/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                 https://docs.posit.co/connect/admin/integrations/posit-cloud/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                     https://docs.posit.co/connect/admin/integrations/tableau/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                       https://docs.posit.co/connect/admin/integrations/version-control/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                          https://docs.posit.co/connect/admin/licensing/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                https://docs.posit.co/connect/admin/licensing/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                     https://docs.posit.co/connect/admin/load-balancing/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                           https://docs.posit.co/connect/admin/load-balancing/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                            https://docs.posit.co/connect/admin/logging/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                  https://docs.posit.co/connect/admin/logging/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                        https://docs.posit.co/connect/admin/nameservice/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                              https://docs.posit.co/connect/admin/nameservice/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                             https://docs.posit.co/connect/admin/nodejs/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                   https://docs.posit.co/connect/admin/nodejs/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                https://docs.posit.co/connect/admin/nodejs/package-management/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                       https://docs.posit.co/connect/admin/opentelemetry 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                      https://docs.posit.co/connect/admin/opentelemetry/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                         https://docs.posit.co/connect/admin/opentelemetry/alerting.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                               https://docs.posit.co/connect/admin/opentelemetry/content-operations.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                https://docs.posit.co/connect/admin/opentelemetry/content-telemetry.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                  https://docs.posit.co/connect/admin/opentelemetry/getting-started.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                           https://docs.posit.co/connect/admin/opentelemetry/health-and-performance.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                            https://docs.posit.co/connect/admin/opentelemetry/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                        https://docs.posit.co/connect/admin/opentelemetry/job-queue.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                 https://docs.posit.co/connect/admin/opentelemetry/license-capacity.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                               https://docs.posit.co/connect/admin/opentelemetry/request-rejections.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                           https://docs.posit.co/connect/admin/opentelemetry/signal-reference-guide.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                               https://docs.posit.co/connect/admin/opentelemetry/signal-reference-guide/ 
#>                                                                                                                                      NA 
#>                                                                                https://docs.posit.co/connect/admin/operational-metrics/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                      https://docs.posit.co/connect/admin/operational-metrics/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                 https://docs.posit.co/connect/admin/process-management/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                       https://docs.posit.co/connect/admin/process-management/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                https://docs.posit.co/connect/admin/programmatic-provisioning/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                    https://docs.posit.co/connect/admin/proxy/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                      https://docs.posit.co/connect/admin/public-access/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                            https://docs.posit.co/connect/admin/public-access/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                             https://docs.posit.co/connect/admin/python/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                   https://docs.posit.co/connect/admin/python/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                          https://docs.posit.co/connect/admin/python/package-management/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                https://docs.posit.co/connect/admin/python/package-management/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                             https://docs.posit.co/connect/admin/quarto/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                   https://docs.posit.co/connect/admin/quarto/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                                  https://docs.posit.co/connect/admin/r/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                                        https://docs.posit.co/connect/admin/r/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                               https://docs.posit.co/connect/admin/r/package-management/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                     https://docs.posit.co/connect/admin/r/package-management/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                    https://docs.posit.co/connect/admin/reference-architectures/aws/k8s/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                          https://docs.posit.co/connect/admin/reference-architectures/aws/k8s/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                          https://docs.posit.co/connect/admin/reference-architectures/aws/load-balanced/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                https://docs.posit.co/connect/admin/reference-architectures/aws/load-balanced/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                   https://docs.posit.co/connect/admin/reference-architectures/aws/single-server-robust/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                         https://docs.posit.co/connect/admin/reference-architectures/aws/single-server-robust/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                          https://docs.posit.co/connect/admin/reference-architectures/aws/single-server/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                https://docs.posit.co/connect/admin/reference-architectures/aws/single-server/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                                  https://docs.posit.co/connect/admin/reference-architectures/azure/k8s/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                        https://docs.posit.co/connect/admin/reference-architectures/azure/k8s/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                        https://docs.posit.co/connect/admin/reference-architectures/azure/load-balanced/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                              https://docs.posit.co/connect/admin/reference-architectures/azure/load-balanced/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                 https://docs.posit.co/connect/admin/reference-architectures/azure/single-server-robust/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                       https://docs.posit.co/connect/admin/reference-architectures/azure/single-server-robust/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                        https://docs.posit.co/connect/admin/reference-architectures/azure/single-server/ 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                              https://docs.posit.co/connect/admin/reference-architectures/azure/single-server/index.html 
#>                                                                                                               "2026-10-13 16:17:47 UTC" 
#>                                                              https://docs.posit.co/connect/admin/reference-architectures/faq/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                      https://docs.posit.co/connect/admin/reference-architectures/generic-architectures/ 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                            https://docs.posit.co/connect/admin/reference-architectures/generic-architectures/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                   https://docs.posit.co/connect/admin/reference-architectures/overview/ 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                         https://docs.posit.co/connect/admin/reference-architectures/overview/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                       https://docs.posit.co/connect/admin/runtime-management/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                                           https://docs.posit.co/connect/admin/security/ 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                  https://docs.posit.co/connect/admin/security/embedding-cors/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                            https://docs.posit.co/connect/admin/security/encryption-key/ 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                  https://docs.posit.co/connect/admin/security/encryption-key/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                             https://docs.posit.co/connect/admin/security/hardening-checklist/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                    https://docs.posit.co/connect/admin/security/http-headers/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                           https://docs.posit.co/connect/admin/security/https/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                                 https://docs.posit.co/connect/admin/security/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                              https://docs.posit.co/connect/admin/security/network-encryption/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                              https://docs.posit.co/connect/admin/security/package-management/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                               https://docs.posit.co/connect/admin/security/server-user/ 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                     https://docs.posit.co/connect/admin/security/server-user/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                            https://docs.posit.co/connect/admin/security/service-tokens/ 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                  https://docs.posit.co/connect/admin/security/service-tokens/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                        https://docs.posit.co/connect/admin/security/sessions/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                               https://docs.posit.co/connect/admin/security/tls-cipher-suites/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                https://docs.posit.co/connect/admin/server-management/backups/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                             https://docs.posit.co/connect/admin/server-management/disk-space/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                 https://docs.posit.co/connect/admin/server-management/docker/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                        https://docs.posit.co/connect/admin/server-management/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                              https://docs.posit.co/connect/admin/server-management/job-queue/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                    https://docs.posit.co/connect/admin/server-management/migration/container/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                              https://docs.posit.co/connect/admin/server-management/migration/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                   https://docs.posit.co/connect/admin/server-management/migration/kubernetes/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                          https://docs.posit.co/connect/admin/server-management/migration/nfs/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                https://docs.posit.co/connect/admin/server-management/restore/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                   https://docs.posit.co/connect/admin/server-management/runtime-caches/ 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                         https://docs.posit.co/connect/admin/server-management/runtime-caches/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                               https://docs.posit.co/connect/admin/tensorflow/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                                            https://docs.posit.co/connect/admin/upgrade/ 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                                  https://docs.posit.co/connect/admin/upgrade/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                                    https://docs.posit.co/connect/admin/user-management/ 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                          https://docs.posit.co/connect/admin/user-management/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                      https://docs.posit.co/connect/admin/user-management/restore-admin/ 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                            https://docs.posit.co/connect/admin/user-management/restore-admin/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                                                      https://docs.posit.co/connect/api/ 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                                            https://docs.posit.co/connect/api/index.html 
#>                                                                                                               "2026-10-13 16:17:48 UTC" 
#>                                                                                                 https://docs.posit.co/connect/cookbook/ 
#>                                                                                                               "2026-10-13 16:17:50 UTC" 
#>                                                              https://docs.posit.co/connect/cookbook/content/integrations/github/python/ 
#>                                                                                                               "2026-10-13 16:17:49 UTC" 
#>                                                                   https://docs.posit.co/connect/cookbook/content/integrations/github/r/ 
#>                                                                                                               "2026-10-13 16:17:49 UTC" 
#>                                                              https://docs.posit.co/connect/cookbook/content/integrations/google/python/ 
#>                                                                                                               "2026-10-13 16:17:49 UTC" 
#>                                                      https://docs.posit.co/connect/cookbook/content/integrations/msgraph/viewer/python/ 
#>                                                                                                               "2026-10-13 16:17:49 UTC" 
#>                                                           https://docs.posit.co/connect/cookbook/content/integrations/msgraph/viewer/r/ 
#>                                                                                                               "2026-10-13 16:17:49 UTC" 
#>                                                   https://docs.posit.co/connect/cookbook/content/integrations/salesforce/viewer/python/ 
#>                                                                                                               "2026-10-13 16:17:49 UTC" 
#>                                                   https://docs.posit.co/connect/cookbook/content/integrations/sharepoint/viewer/python/ 
#>                                                                                                               "2026-10-13 16:17:49 UTC" 
#>                                                        https://docs.posit.co/connect/cookbook/content/integrations/sharepoint/viewer/r/ 
#>                                                                                                               "2026-10-13 16:17:49 UTC" 
#>                                                                        https://docs.posit.co/connect/cookbook/content/rendering-content 
#>                                                                                                               "2026-10-13 16:17:49 UTC" 
#>                                                                       https://docs.posit.co/connect/cookbook/content/restarting-content 
#>                                                                                                               "2026-10-13 16:17:49 UTC" 
#>                                                                                       https://docs.posit.co/connect/cookbook/index.html 
#>                                                                                                               "2026-10-13 16:17:50 UTC" 
#>                                      https://docs.posit.co/connect/cookbook/off-host-execution/creating-a-nodejs-execution-environment/ 
#>                                                                                                               "2026-10-13 16:17:50 UTC" 
#>                                                                                  https://docs.posit.co/connect/cookbook/runtime-caches/ 
#>                                                                                                               "2026-10-13 16:17:51 UTC" 
#>                                                                                                   https://docs.posit.co/connect/how-to/ 
#>                                                                                                               "2026-10-13 16:17:51 UTC" 
#>                                                                                         https://docs.posit.co/connect/how-to/index.html 
#>                                                                                                               "2026-10-13 16:17:51 UTC" 
#>                                                             https://docs.posit.co/connect/how-to/read-secrets-from-aws-secrets-manager/ 
#>                                                                                                               "2026-10-13 16:17:51 UTC" 
#>                                                                   https://docs.posit.co/connect/how-to/run-content-as-the-current-user/ 
#>                                                                                                               "2026-10-13 16:17:51 UTC" 
#>                                                                                                https://docs.posit.co/connect/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                                 https://docs.posit.co/connect/licenses/ 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                                  https://docs.posit.co/connect/llms.txt 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                                      https://docs.posit.co/connect/mcp/ 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                            https://docs.posit.co/connect/mcp/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                                     https://docs.posit.co/connect/news/ 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                           https://docs.posit.co/connect/news/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                                     https://docs.posit.co/connect/user/ 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                            https://docs.posit.co/connect/user/api-keys/ 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                  https://docs.posit.co/connect/user/api-keys/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                          https://docs.posit.co/connect/user/audit-logs/ 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                https://docs.posit.co/connect/user/audit-logs/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                     https://docs.posit.co/connect/user/bokeh/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                               https://docs.posit.co/connect/user/connect-mcp/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                          https://docs.posit.co/connect/user/connecting/ 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                     https://docs.posit.co/connect/user/content-bundles/ 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                           https://docs.posit.co/connect/user/content-bundles/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                              https://docs.posit.co/connect/user/content-logs/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                          https://docs.posit.co/connect/user/content-overview/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                    https://docs.posit.co/connect/user/content-settings/ 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                          https://docs.posit.co/connect/user/content-settings/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                   https://docs.posit.co/connect/user/content-telemetry/ 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                         https://docs.posit.co/connect/user/content-telemetry/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                          https://docs.posit.co/connect/user/curating-content/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                               https://docs.posit.co/connect/user/custom-docs/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                          https://docs.posit.co/connect/user/custom-messaging/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                      https://docs.posit.co/connect/user/dash/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                        https://docs.posit.co/connect/user/early-access/ 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                              https://docs.posit.co/connect/user/early-access/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                   https://docs.posit.co/connect/user/embedding-content/ 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                         https://docs.posit.co/connect/user/embedding-content/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                              https://docs.posit.co/connect/user/environments/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                             https://docs.posit.co/connect/user/fastapi/ 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                   https://docs.posit.co/connect/user/fastapi/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                     https://docs.posit.co/connect/user/flask/index.html 
#>                                                                                                               "2026-10-13 16:17:52 UTC" 
#>                                                                                          https://docs.posit.co/connect/user/git-backed/ 
#>                                                                                                               "2026-10-13 16:17:53 UTC" 
#>                                                                                https://docs.posit.co/connect/user/git-backed/index.html 
#>                                                                                                               "2026-10-13 16:17:53 UTC" 
#>                                                                                              https://docs.posit.co/connect/user/gradio/ 
#>                                                                                                               "2026-10-13 16:17:53 UTC" 
#>                                                                                    https://docs.posit.co/connect/user/gradio/index.html 
#>                                                                                                               "2026-10-13 16:17:53 UTC" 
#>                                                                                           https://docs.posit.co/connect/user/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                     https://docs.posit.co/connect/user/interactive-documents/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                          https://docs.posit.co/connect/user/jupyter-notebook/index.html 
#>                                                                                                               "2026-10-13 16:17:54 UTC" 
#>                                                                                 https://docs.posit.co/connect/user/licensing/index.html 
#>                                                                                                               "2026-10-13 16:17:54 UTC" 
#>                                                                          https://docs.posit.co/connect/user/managing-connect/index.html 
#>                                                                                                               "2026-10-13 16:17:54 UTC" 
#>                                                                               https://docs.posit.co/connect/user/managing-integrations/ 
#>                                                                                                               "2026-10-13 16:17:54 UTC" 
#>                                                                     https://docs.posit.co/connect/user/managing-integrations/index.html 
#>                                                                                                               "2026-10-13 16:17:54 UTC" 
#>                                                                                            https://docs.posit.co/connect/user/manifest/ 
#>                                                                                                               "2026-10-13 16:17:54 UTC" 
#>                                                                                  https://docs.posit.co/connect/user/manifest/index.html 
#>                                                                                                               "2026-10-13 16:17:54 UTC" 
#>                                                                                         https://docs.posit.co/connect/user/mcp-servers/ 
#>                                                                                                               "2026-10-13 16:17:54 UTC" 
#>                                                                               https://docs.posit.co/connect/user/mcp-servers/index.html 
#>                                                                                                               "2026-10-13 16:17:54 UTC" 
#>                                                                                   https://docs.posit.co/connect/user/metrics/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                    https://docs.posit.co/connect/user/nodejs/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                       https://docs.posit.co/connect/user/notifications/ 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                             https://docs.posit.co/connect/user/notifications/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                  https://docs.posit.co/connect/user/oauth-integrations/ 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                        https://docs.posit.co/connect/user/oauth-integrations/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                               https://docs.posit.co/connect/user/orientation/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                               https://docs.posit.co/connect/user/panel/ 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                     https://docs.posit.co/connect/user/panel/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                           https://docs.posit.co/connect/user/param-rmarkdown/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                      https://docs.posit.co/connect/user/pins/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                   https://docs.posit.co/connect/user/plumber/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                 https://docs.posit.co/connect/user/processes/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                  https://docs.posit.co/connect/user/publishing-agentic/ 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                        https://docs.posit.co/connect/user/publishing-agentic/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                       https://docs.posit.co/connect/user/publishing-cli-apps/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                       https://docs.posit.co/connect/user/publishing-cli-html/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                   https://docs.posit.co/connect/user/publishing-cli-manifest/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                             https://docs.posit.co/connect/user/publishing-cli-notebook/ 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                   https://docs.posit.co/connect/user/publishing-cli-notebook/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                     https://docs.posit.co/connect/user/publishing-cli-quarto/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                            https://docs.posit.co/connect/user/publishing-cli/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                          https://docs.posit.co/connect/user/publishing-connect-gallery/ 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                https://docs.posit.co/connect/user/publishing-connect-gallery/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                 https://docs.posit.co/connect/user/publishing-github-actions/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                       https://docs.posit.co/connect/user/publishing-overview/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                       https://docs.posit.co/connect/user/publishing-packages/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                https://docs.posit.co/connect/user/publishing-positron-vscode/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                              https://docs.posit.co/connect/user/publishing-r/index.html 
#>                                                                                                               "2026-10-13 16:17:56 UTC" 
#>                                                                        https://docs.posit.co/connect/user/publishing-rstudio/index.html 
#>                                                                                                               "2026-10-13 16:17:56 UTC" 
#>                                                                           https://docs.posit.co/connect/user/publishing-spcs/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                          https://docs.posit.co/connect/user/publishing/ 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                              https://docs.posit.co/connect/user/quarto/ 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                    https://docs.posit.co/connect/user/quarto/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                           https://docs.posit.co/connect/user/refresh-rebuild/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                      https://docs.posit.co/connect/user/report-history/ 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                            https://docs.posit.co/connect/user/report-history/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                 https://docs.posit.co/connect/user/rmarkdown/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                                https://docs.posit.co/connect/user/rpy2/ 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                      https://docs.posit.co/connect/user/rpy2/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                          https://docs.posit.co/connect/user/scheduling/ 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                https://docs.posit.co/connect/user/scheduling/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                             https://docs.posit.co/connect/user/scripts/ 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                   https://docs.posit.co/connect/user/scripts/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                              https://docs.posit.co/connect/user/shiny-python/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                     https://docs.posit.co/connect/user/shiny/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                      https://docs.posit.co/connect/user/source-versions 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                              https://docs.posit.co/connect/user/static/ 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                    https://docs.posit.co/connect/user/static/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                 https://docs.posit.co/connect/user/streamlit/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                       https://docs.posit.co/connect/user/structuring-content/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                             https://docs.posit.co/connect/user/system-checks/index.html 
#>                                                                                                               "2026-10-13 16:17:55 UTC" 
#>                                                                                   https://docs.posit.co/connect/user/tableau/index.html 
#>                                                                                                               "2026-10-13 16:17:56 UTC" 
#>                                                                                      https://docs.posit.co/connect/user/tags/index.html 
#>                                                                                                               "2026-10-13 16:17:56 UTC" 
#>                                                                                https://docs.posit.co/connect/user/tensorflow/index.html 
#>                                                                                                               "2026-10-13 16:17:56 UTC" 
#>                                                                           https://docs.posit.co/connect/user/troubleshooting/index.html 
#>                                                                                                               "2026-10-13 16:17:56 UTC" 
#>                                                                                  https://docs.posit.co/connect/user/trusted-publishing/ 
#>                                                                                                               "2026-10-13 16:17:56 UTC" 
#>                                                                        https://docs.posit.co/connect/user/trusted-publishing/index.html 
#>                                                                                                               "2026-10-13 16:17:56 UTC" 
#>                                                                                https://docs.posit.co/connect/user/usage-data/index.html 
#>                                                                                                               "2026-10-13 16:17:56 UTC" 
#>                                                                              https://docs.posit.co/connect/user/user-metrics/index.html 
#>                                                                                                               "2026-10-13 16:17:56 UTC" 
#>                                                                                        https://docs.posit.co/connect/user/user-preview/ 
#>                                                                                                               "2026-10-13 16:17:56 UTC" 
#>                                                                              https://docs.posit.co/connect/user/user-preview/index.html 
#>                                                                                                               "2026-10-13 16:17:56 UTC" 
#>                                                                                   https://docs.posit.co/connect/user/vetiver/index.html 
#>                                                                                                               "2026-10-13 16:17:56 UTC" 
#>                                                                                     https://docs.posit.co/connect/user/viewing-content/ 
#>                                                                                                               "2026-10-13 16:17:56 UTC" 
#>                                                                           https://docs.posit.co/connect/user/viewing-content/index.html 
#>                                                                                                               "2026-10-13 16:17:56 UTC" 
#>                                                                                                     https://docs.posit.co/data-sources/ 
#>                                                                                                               "2026-10-15 18:01:01 UTC" 
#>                                                                                https://docs.posit.co/data-sources/admin/databricks.html 
#>                                                                                                               "2026-10-15 18:01:01 UTC" 
#>                                                                                     https://docs.posit.co/data-sources/admin/index.html 
#>                                                                                                               "2026-10-15 18:01:01 UTC" 
#>                                                                                   https://docs.posit.co/data-sources/admin/pro-drivers/ 
#>                                                                                                               "2026-10-15 18:01:01 UTC" 
#>                                                                 https://docs.posit.co/data-sources/admin/pro-drivers/configuration.html 
#>                                                                                                               "2026-10-15 18:01:01 UTC" 
#>                                                                    https://docs.posit.co/data-sources/admin/pro-drivers/connecting.html 
#>                                                                                                               "2026-10-15 18:01:01 UTC" 
#>                                                                         https://docs.posit.co/data-sources/admin/pro-drivers/index.html 
#>                                                                                                               "2026-10-15 18:01:01 UTC" 
#>                                                                  https://docs.posit.co/data-sources/admin/pro-drivers/installation.html 
#>                                                                                                               "2026-10-15 18:01:01 UTC" 
#>                                                                 https://docs.posit.co/data-sources/admin/pro-drivers/release-notes.html 
#>                                                                                                               "2026-10-15 18:01:01 UTC" 
#>                                                               https://docs.posit.co/data-sources/admin/pro-drivers/troubleshooting.html 
#>                                                                                                               "2026-10-15 18:01:01 UTC" 
#>                                                                                 https://docs.posit.co/data-sources/admin/snowflake.html 
#>                                                                                                               "2026-10-15 18:01:01 UTC" 
#>                                                                                           https://docs.posit.co/data-sources/index.html 
#>                                                                                                               "2026-10-15 18:01:01 UTC" 
#>                                                                                      https://docs.posit.co/data-sources/user/index.html 
#>                                                                                                               "2026-10-15 18:01:01 UTC" 
#>                                                                                             https://docs.posit.co/enterprise/index.html 
#>                                                                                                               "2026-10-15 18:29:16 UTC" 
#>                                                                                                  https://docs.posit.co/getting-started/ 
#>                                                                                                               "2026-10-15 18:29:16 UTC" 
#>                                                                                https://docs.posit.co/getting-started/configuration.html 
#>                                                                                                               "2026-10-15 18:29:16 UTC" 
#>                                                                                     https://docs.posit.co/getting-started/database.html 
#>                                                                                                               "2026-10-15 18:29:16 UTC" 
#>                                                                                        https://docs.posit.co/getting-started/index.html 
#>                                                                                                               "2026-10-15 18:29:16 UTC" 
#>                                                                              https://docs.posit.co/getting-started/install-planner.html 
#>                                                                                                               "2026-10-15 18:29:16 UTC" 
#>                                                                                   https://docs.posit.co/getting-started/networking.html 
#>                                                                                                               "2026-10-15 18:29:16 UTC" 
#>                                                                                       https://docs.posit.co/getting-started/sizing.html 
#>                                                                                                               "2026-10-15 18:29:16 UTC" 
#>                                                                                      https://docs.posit.co/getting-started/storage.html 
#>                                                                                                               "2026-10-15 18:29:16 UTC" 
#>                                                                                                            https://docs.posit.co/guides 
#>                                                                                                               "2026-10-15 18:29:16 UTC" 
#>                                                                                                       https://docs.posit.co/guides.html 
#>                                                                                                               "2026-10-15 18:29:16 UTC" 
#>                                                                                                             https://docs.posit.co/helm/ 
#>                                                                                                               "2026-10-15 18:39:27 UTC" 
#>                                                                             https://docs.posit.co/helm/charts/posit-chronicle/NEWS.html 
#>                                                                                                               "2026-10-15 18:39:26 UTC" 
#>                                                                           https://docs.posit.co/helm/charts/posit-chronicle/README.html 
#>                                                                                                               "2026-10-15 18:39:26 UTC" 
#>                                                                             https://docs.posit.co/helm/charts/rstudio-connect/NEWS.html 
#>                                                                                                               "2026-10-15 18:39:26 UTC" 
#>                                                                           https://docs.posit.co/helm/charts/rstudio-connect/README.html 
#>                                                                                                               "2026-10-15 18:39:26 UTC" 
#>                                                                                  https://docs.posit.co/helm/charts/rstudio-pm/NEWS.html 
#>                                                                                                               "2026-10-15 18:39:26 UTC" 
#>                                                                                https://docs.posit.co/helm/charts/rstudio-pm/README.html 
#>                                                                                                               "2026-10-15 18:39:26 UTC" 
#>                                                                           https://docs.posit.co/helm/charts/rstudio-workbench/NEWS.html 
#>                                                                                                               "2026-10-15 18:39:26 UTC" 
#>                                                                         https://docs.posit.co/helm/charts/rstudio-workbench/README.html 
#>                                                                                                               "2026-10-15 18:39:26 UTC" 
#>                                                                          https://docs.posit.co/helm/docs/migrating-to-posit-images.html 
#>                                                                                                               "2026-10-15 18:39:26 UTC" 
#>                                                                                  https://docs.posit.co/helm/examples/connect/index.html 
#>                                                                                                               "2026-10-15 18:39:26 UTC" 
#>                                  https://docs.posit.co/helm/examples/connect/upgrade-launcher-to-kubernetes/launcher-to-kubernetes.html 
#>                                                                                                               "2026-10-15 18:39:27 UTC" 
#>                                                                          https://docs.posit.co/helm/examples/package-manager/index.html 
#>                                                                                                               "2026-10-15 18:39:27 UTC" 
#>                                                                                https://docs.posit.co/helm/examples/workbench/index.html 
#>                                                                                                               "2026-10-15 18:39:27 UTC" 
#>                                                                https://docs.posit.co/how-to-guides/guides/install-ssl-certificates.html 
#>                                                                                                               "2026-10-15 18:29:16 UTC" 
#>                                                     https://docs.posit.co/how-to-guides/guides/migrate-shiny-server-pro-to-connect.html 
#>                                                                                                               "2026-10-15 18:29:16 UTC" 
#>                                                                                          https://docs.posit.co/how-to-guides/index.html 
#>                                                                                                               "2026-10-15 18:29:16 UTC" 
#>                                                                             https://docs.posit.co/how-to-guides/upgrade-os-and-product/ 
#>                                                                                                               "2026-10-15 18:29:16 UTC" 
#>                                                                                                  https://docs.posit.co/ide/desktop-pro/ 
#>                                                                                                               "2026-10-14 22:29:05 UTC" 
#>                                                                                        https://docs.posit.co/ide/desktop-pro/2026.10.0/ 
#>                                                                                                               "2026-10-15 17:10:56 UTC" 
#>                                                    https://docs.posit.co/ide/desktop-pro/database_drivers/install_database_drivers.html 
#>                                                                                                               "2026-10-14 22:29:05 UTC" 
#>                                                                   https://docs.posit.co/ide/desktop-pro/getting_started/activation.html 
#>                                                                                                               "2026-10-14 22:29:05 UTC" 
#>                                                                 https://docs.posit.co/ide/desktop-pro/getting_started/installation.html 
#>                                                                                                               "2026-10-14 22:29:05 UTC" 
#>                                                                                                         https://docs.posit.co/ide/news/ 
#>                                                                                                               "2026-10-14 22:30:13 UTC" 
#>                                                                                                   https://docs.posit.co/ide/server-pro/ 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                              https://docs.posit.co/ide/server-pro/2023.09.1/database/configuration.html 
#>                                                                                                               "2023-10-18 22:31:05 UTC" 
#>                                          https://docs.posit.co/ide/server-pro/2024.04.0/auditing_and_monitoring/prometheus_metrics.html 
#>                                                                                                               "2024-11-22 15:17:32 UTC" 
#>                                                 https://docs.posit.co/ide/server-pro/2024.04.0/user_provisioning/user_provisioning.html 
#>                                                                                                               "2024-11-22 15:17:32 UTC" 
#>                                https://docs.posit.co/ide/server-pro/2024.09.0/getting_started/installation/kubernetes_installation.html 
#>                                                                                                               "2024-11-22 17:11:18 UTC" 
#>                                                        https://docs.posit.co/ide/server-pro/2024.09.1/getting_started/requirements.html 
#>                                                                                                               "2025-03-19 15:22:31 UTC" 
#>                                              https://docs.posit.co/ide/server-pro/2026.01.1/user/positron/guide/positron-assistant.html 
#>                                                                                                               "2026-03-02 14:37:37 UTC" 
#>                                          https://docs.posit.co/ide/server-pro/2026.04.0/admin/positron_sessions/positron-assistant.html 
#>                                                                                                               "2026-05-07 02:55:23 UTC" 
#>                                                          https://docs.posit.co/ide/server-pro/access_and_security/outgoing_proxies.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                            https://docs.posit.co/ide/server-pro/access_and_security/secure_sockets.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                   https://docs.posit.co/ide/server-pro/access_and_security/security_considerations.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                                                             https://docs.posit.co/ide/server-pro/admin/ 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                 https://docs.posit.co/ide/server-pro/admin/access_and_security/access_and_security.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                             https://docs.posit.co/ide/server-pro/admin/access_and_security/cookies.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                            https://docs.posit.co/ide/server-pro/admin/access_and_security/network_port_and_address.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                    https://docs.posit.co/ide/server-pro/admin/access_and_security/outgoing_proxies.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                        https://docs.posit.co/ide/server-pro/admin/access_and_security/pam_sessions.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                https://docs.posit.co/ide/server-pro/admin/access_and_security/running_with_a_proxy.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                      https://docs.posit.co/ide/server-pro/admin/access_and_security/secure_sockets.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                             https://docs.posit.co/ide/server-pro/admin/access_and_security/security_considerations.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                               https://docs.posit.co/ide/server-pro/admin/access_and_security/selinux_configuration.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                               https://docs.posit.co/ide/server-pro/admin/access_and_security/selinux_customization.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                  https://docs.posit.co/ide/server-pro/admin/access_and_security/server_permissions.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                                       https://docs.posit.co/ide/server-pro/admin/ai/agent-activity.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                                 https://docs.posit.co/ide/server-pro/admin/ai/ai-provider-controls.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                                   https://docs.posit.co/ide/server-pro/admin/ai/assistant-controls.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                            https://docs.posit.co/ide/server-pro/admin/ai/upgrading_posit_assistant.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                  https://docs.posit.co/ide/server-pro/admin/auditing_and_monitoring/audit_coverage.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                          https://docs.posit.co/ide/server-pro/admin/auditing_and_monitoring/audited_workbench_jobs.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                         https://docs.posit.co/ide/server-pro/admin/auditing_and_monitoring/auditing_and_monitoring.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                          https://docs.posit.co/ide/server-pro/admin/auditing_and_monitoring/auditing_configuration.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                       https://docs.posit.co/ide/server-pro/admin/auditing_and_monitoring/chronicle.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                        https://docs.posit.co/ide/server-pro/admin/auditing_and_monitoring/monitoring_configuration.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                   https://docs.posit.co/ide/server-pro/admin/auditing_and_monitoring/opentelemetry.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                              https://docs.posit.co/ide/server-pro/admin/auditing_and_monitoring/prometheus_metrics.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                            https://docs.posit.co/ide/server-pro/admin/auditing_and_monitoring/server_health_checks.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                   https://docs.posit.co/ide/server-pro/admin/authenticating_users/active_directory.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                               https://docs.posit.co/ide/server-pro/admin/authenticating_users/authenticating_users.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                           https://docs.posit.co/ide/server-pro/admin/authenticating_users/authentication_migration.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                              https://docs.posit.co/ide/server-pro/admin/authenticating_users/authorization_timeout.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                    https://docs.posit.co/ide/server-pro/admin/authenticating_users/aws_credentials.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                 https://docs.posit.co/ide/server-pro/admin/authenticating_users/customizing_signin.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                        https://docs.posit.co/ide/server-pro/admin/authenticating_users/delegated_azure_credentials.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                      https://docs.posit.co/ide/server-pro/admin/authenticating_users/openid_connect_authentication.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                 https://docs.posit.co/ide/server-pro/admin/authenticating_users/pam_authentication.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                             https://docs.posit.co/ide/server-pro/admin/authenticating_users/proxied_authentication.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                 https://docs.posit.co/ide/server-pro/admin/authenticating_users/restricting_access.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                           https://docs.posit.co/ide/server-pro/admin/authenticating_users/saml_sso.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                               https://docs.posit.co/ide/server-pro/admin/authenticating_users/user_access_overview.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                               https://docs.posit.co/ide/server-pro/admin/data_connectivity/connectivity_using_odbc.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                         https://docs.posit.co/ide/server-pro/admin/data_connectivity/connectivity_using_r_packages.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                     https://docs.posit.co/ide/server-pro/admin/data_connectivity/data_connectivity.html 
#>                                                                                                               "2026-10-14 22:41:21 UTC" 
#>                                                         https://docs.posit.co/ide/server-pro/admin/data_connectivity/snippet_files.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                            https://docs.posit.co/ide/server-pro/admin/database/audit/configuration.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                 https://docs.posit.co/ide/server-pro/admin/database/audit/database.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                  https://docs.posit.co/ide/server-pro/admin/database/configuration.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                       https://docs.posit.co/ide/server-pro/admin/database/database.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                      https://docs.posit.co/ide/server-pro/admin/database/migration.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                    https://docs.posit.co/ide/server-pro/admin/getting_started/accessing_the_server.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                   https://docs.posit.co/ide/server-pro/admin/getting_started/initial-configuration.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                      https://docs.posit.co/ide/server-pro/admin/getting_started/installation/index.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                     https://docs.posit.co/ide/server-pro/admin/getting_started/installation/install-rstudio-server.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                               https://docs.posit.co/ide/server-pro/admin/getting_started/installation/installation.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                    https://docs.posit.co/ide/server-pro/admin/getting_started/installation/kubernetes_installation.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                  https://docs.posit.co/ide/server-pro/admin/getting_started/installation/multi_server_installation.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                       https://docs.posit.co/ide/server-pro/admin/getting_started/management_script.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                              https://docs.posit.co/ide/server-pro/admin/getting_started/networking.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                            https://docs.posit.co/ide/server-pro/admin/getting_started/requirements.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                  https://docs.posit.co/ide/server-pro/admin/getting_started/sizing.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                 https://docs.posit.co/ide/server-pro/admin/getting_started/support.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                              https://docs.posit.co/ide/server-pro/admin/hardening/browser_security.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                      https://docs.posit.co/ide/server-pro/admin/hardening/database.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                    https://docs.posit.co/ide/server-pro/admin/hardening/encryption.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                  https://docs.posit.co/ide/server-pro/admin/hardening/example_secure_configuration.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                     https://docs.posit.co/ide/server-pro/admin/hardening/hardening.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                         https://docs.posit.co/ide/server-pro/admin/hardening/other.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                            https://docs.posit.co/ide/server-pro/admin/hardening/r_session_security.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                                   https://docs.posit.co/ide/server-pro/admin/index.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                  https://docs.posit.co/ide/server-pro/admin/integration/amazon-efs.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                               https://docs.posit.co/ide/server-pro/admin/integration/aws-sagemaker.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                         https://docs.posit.co/ide/server-pro/admin/integration/configure-sagemaker.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                https://docs.posit.co/ide/server-pro/admin/integration/custom_oauth.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                  https://docs.posit.co/ide/server-pro/admin/integration/databricks.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                       https://docs.posit.co/ide/server-pro/admin/integration/index.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                  https://docs.posit.co/ide/server-pro/admin/integration/jupyter-ha.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                          https://docs.posit.co/ide/server-pro/admin/integration/jupyter-kubernetes.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                            https://docs.posit.co/ide/server-pro/admin/integration/jupyter-multiple-python-versions.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                               https://docs.posit.co/ide/server-pro/admin/integration/jupyter-slurm.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                          https://docs.posit.co/ide/server-pro/admin/integration/jupyter-standalone.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                     https://docs.posit.co/ide/server-pro/admin/integration/jupyter.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                         https://docs.posit.co/ide/server-pro/admin/integration/launcher-kubernetes.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                              https://docs.posit.co/ide/server-pro/admin/integration/launcher-slurm.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                     https://docs.posit.co/ide/server-pro/admin/integration/pyspark.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                  https://docs.posit.co/ide/server-pro/admin/integration/reticulate.html 
#>                                                                                                               "2026-10-14 22:41:22 UTC" 
#>                                                                   https://docs.posit.co/ide/server-pro/admin/integration/snowflake.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                  https://docs.posit.co/ide/server-pro/admin/integration/snowflake/native-app/index.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                                    https://docs.posit.co/ide/server-pro/admin/integration/sparklyr.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                              https://docs.posit.co/ide/server-pro/admin/job_launcher/configuration.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                               https://docs.posit.co/ide/server-pro/admin/job_launcher/job_launcher.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                          https://docs.posit.co/ide/server-pro/admin/job_launcher/kubernetes_plugin.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                    https://docs.posit.co/ide/server-pro/admin/job_launcher/launcher_administration.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                             https://docs.posit.co/ide/server-pro/admin/job_launcher/load_balancing.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                               https://docs.posit.co/ide/server-pro/admin/job_launcher/local_plugin.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                https://docs.posit.co/ide/server-pro/admin/job_launcher/managed_credentials_in_jobs.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                               https://docs.posit.co/ide/server-pro/admin/job_launcher/slurm_plugin.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                 https://docs.posit.co/ide/server-pro/admin/job_launcher/troubleshooting-kubernetes.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                        https://docs.posit.co/ide/server-pro/admin/job_launcher/using_docker_images.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                          https://docs.posit.co/ide/server-pro/admin/jupyter_sessions/configuration.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                       https://docs.posit.co/ide/server-pro/admin/jupyter_sessions/jupyter_sessions.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                   https://docs.posit.co/ide/server-pro/admin/jupyter_sessions/package_installation.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                        https://docs.posit.co/ide/server-pro/admin/jupyter_sessions/troubleshooting.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                   https://docs.posit.co/ide/server-pro/admin/license_management/license_management.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                  https://docs.posit.co/ide/server-pro/admin/load_balancing/access_and_availability.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                        https://docs.posit.co/ide/server-pro/admin/load_balancing/balancing_methods.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                            https://docs.posit.co/ide/server-pro/admin/load_balancing/configuration.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                           https://docs.posit.co/ide/server-pro/admin/load_balancing/load_balancing.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                           https://docs.posit.co/ide/server-pro/admin/load_balancing/load_balancing_vs_job_launcher.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                                           https://docs.posit.co/ide/server-pro/admin/positron_sessions/ 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                         https://docs.posit.co/ide/server-pro/admin/positron_sessions/configuration.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                               https://docs.posit.co/ide/server-pro/admin/positron_sessions/extension_configuration.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                                 https://docs.posit.co/ide/server-pro/admin/positron_sessions/index.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                  https://docs.posit.co/ide/server-pro/admin/positron_sessions/interpreter_settings.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                     https://docs.posit.co/ide/server-pro/admin/positron_sessions/multiple_sessions.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                  https://docs.posit.co/ide/server-pro/admin/positron_sessions/package_installation.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                       https://docs.posit.co/ide/server-pro/admin/positron_sessions/troubleshooting.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                    https://docs.posit.co/ide/server-pro/admin/positron_sessions/upgrading_positron.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                         https://docs.posit.co/ide/server-pro/admin/positron_sessions/user_settings.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                            https://docs.posit.co/ide/server-pro/admin/positron_sessions/workbench_job_interpreters.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                              https://docs.posit.co/ide/server-pro/admin/python/configuring_default.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                                            https://docs.posit.co/ide/server-pro/admin/python/index.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                                https://docs.posit.co/ide/server-pro/admin/python/installing_python.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                             https://docs.posit.co/ide/server-pro/admin/python/package_installation.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                https://docs.posit.co/ide/server-pro/admin/python/using_multiple_versions_of_python.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                           https://docs.posit.co/ide/server-pro/admin/quick-starts/active-directory.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                             https://docs.posit.co/ide/server-pro/admin/quick-starts/entraid-openid.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                                      https://docs.posit.co/ide/server-pro/admin/quick-starts/index.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                                https://docs.posit.co/ide/server-pro/admin/quick-starts/okta-openid.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                            https://docs.posit.co/ide/server-pro/admin/quick-starts/resource-limits.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                                   https://docs.posit.co/ide/server-pro/admin/r/configuring_default.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                                                 https://docs.posit.co/ide/server-pro/admin/r/index.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                                          https://docs.posit.co/ide/server-pro/admin/r/installing_r.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                                     https://docs.posit.co/ide/server-pro/admin/r/managing_upgrades.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                                https://docs.posit.co/ide/server-pro/admin/r/session_initialization.html 
#>                                                                                                               "2026-10-14 22:41:23 UTC" 
#>                                                          https://docs.posit.co/ide/server-pro/admin/r/using_multiple_versions_of_r.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                                     https://docs.posit.co/ide/server-pro/admin/reference-architectures/ 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                               https://docs.posit.co/ide/server-pro/admin/reference-architectures/architecture-faqs.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                     https://docs.posit.co/ide/server-pro/admin/reference-architectures/aws/aws-eks.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                           https://docs.posit.co/ide/server-pro/admin/reference-architectures/aws/aws-load-balanced.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                         https://docs.posit.co/ide/server-pro/admin/reference-architectures/aws/aws-parallelcluster.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                    https://docs.posit.co/ide/server-pro/admin/reference-architectures/aws/aws-single-server-robust.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                           https://docs.posit.co/ide/server-pro/admin/reference-architectures/aws/aws-single-server.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                 https://docs.posit.co/ide/server-pro/admin/reference-architectures/azure/azure-aks.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                       https://docs.posit.co/ide/server-pro/admin/reference-architectures/azure/azure-load-balanced.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                https://docs.posit.co/ide/server-pro/admin/reference-architectures/azure/azure-single-server-robust.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                       https://docs.posit.co/ide/server-pro/admin/reference-architectures/azure/azure-single-server.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                           https://docs.posit.co/ide/server-pro/admin/reference-architectures/generic-architectures.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                           https://docs.posit.co/ide/server-pro/admin/reference-architectures/index.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                         https://docs.posit.co/ide/server-pro/admin/reference/audit_data_dictionary.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                               https://docs.posit.co/ide/server-pro/admin/reference/data_dictionary.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                      https://docs.posit.co/ide/server-pro/admin/reference/header_cookie_dictionary.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                                 https://docs.posit.co/ide/server-pro/admin/reference/launcher_conf.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                        https://docs.posit.co/ide/server-pro/admin/reference/r_package_dependencies.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                                  https://docs.posit.co/ide/server-pro/admin/reference/rserver_conf.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                                 https://docs.posit.co/ide/server-pro/admin/reference/rsession_conf.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                          https://docs.posit.co/ide/server-pro/admin/reference/rstudio_ide_commands.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                            https://docs.posit.co/ide/server-pro/admin/reference/rstudio_server_cli.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                         https://docs.posit.co/ide/server-pro/admin/reference/session_user_settings.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                       https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/customizing_session_settings.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                               https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/directory_management.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                     https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/feature_limits.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                             https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/first_project_template.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                              https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/index.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/multiple_r_sessions.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                      https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/notifications.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                               https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/package_installation.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                    https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/project_sharing.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                         https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/r_executable_and_libraries.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                             https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/rstudio_connect_server.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                            https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/session_startup_scripts.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/terminal_websockets.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                            https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/user_and_group_profiles.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                               https://docs.posit.co/ide/server-pro/admin/rstudio_pro_sessions/workspace_management.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                              https://docs.posit.co/ide/server-pro/admin/server_management/administrative_dashboard.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                          https://docs.posit.co/ide/server-pro/admin/server_management/backup_guide.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                             https://docs.posit.co/ide/server-pro/admin/server_management/core_administrative_tasks.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                               https://docs.posit.co/ide/server-pro/admin/server_management/logging.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                     https://docs.posit.co/ide/server-pro/admin/server_management/server_management.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                      https://docs.posit.co/ide/server-pro/admin/server_management/server_migration.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                       https://docs.posit.co/ide/server-pro/admin/server_management/session_storage.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                                  https://docs.posit.co/ide/server-pro/admin/sessions/idle_timeouts.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                                  https://docs.posit.co/ide/server-pro/admin/sessions/session_hooks.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                              https://docs.posit.co/ide/server-pro/admin/tutorial_api/configuration.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                                  https://docs.posit.co/ide/server-pro/admin/tutorial_api/interface.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                               https://docs.posit.co/ide/server-pro/admin/tutorial_api/tutorial_api.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                     https://docs.posit.co/ide/server-pro/admin/tutorial_api/tutorial_api_callbacks.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                       https://docs.posit.co/ide/server-pro/admin/tutorial_api/tutorial_api_methods.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                        https://docs.posit.co/ide/server-pro/admin/upgrade/upgrade-from-open-source.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                                         https://docs.posit.co/ide/server-pro/admin/upgrade/upgrade.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                                https://docs.posit.co/ide/server-pro/admin/upgrade/upgrade_positron.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                                 https://docs.posit.co/ide/server-pro/admin/user_provisioning/azure.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                                         https://docs.posit.co/ide/server-pro/admin/user_provisioning/configuration.html 
#>                                                                                                               "2026-10-14 22:41:24 UTC" 
#>                                             https://docs.posit.co/ide/server-pro/admin/user_provisioning/just_in_time_provisioning.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                        https://docs.posit.co/ide/server-pro/admin/user_provisioning/license_limits.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                       https://docs.posit.co/ide/server-pro/admin/user_provisioning/managing_tokens.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                        https://docs.posit.co/ide/server-pro/admin/user_provisioning/managing_users.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                                  https://docs.posit.co/ide/server-pro/admin/user_provisioning/okta.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                                  https://docs.posit.co/ide/server-pro/admin/user_provisioning/sssd.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                           https://docs.posit.co/ide/server-pro/admin/user_provisioning/transferring_existing_users.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                     https://docs.posit.co/ide/server-pro/admin/user_provisioning/user_provisioning.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                           https://docs.posit.co/ide/server-pro/admin/vscode_sessions/configuration.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                 https://docs.posit.co/ide/server-pro/admin/vscode_sessions/extension_configuration.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                       https://docs.posit.co/ide/server-pro/admin/vscode_sessions/multiple_sessions.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                    https://docs.posit.co/ide/server-pro/admin/vscode_sessions/package_installation.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                         https://docs.posit.co/ide/server-pro/admin/vscode_sessions/troubleshooting.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                           https://docs.posit.co/ide/server-pro/admin/vscode_sessions/user_settings.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                        https://docs.posit.co/ide/server-pro/admin/vscode_sessions/vs_code_sessions.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                              https://docs.posit.co/ide/server-pro/admin/vscode_sessions/workbench_job_interpreters.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                                 https://docs.posit.co/ide/server-pro/admin/workbench_api/interface.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                             https://docs.posit.co/ide/server-pro/admin/workbench_api/workbench_api.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                    https://docs.posit.co/ide/server-pro/auditing_and_monitoring/prometheus_metrics.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                     https://docs.posit.co/ide/server-pro/authenticating_users/authenticating_users.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                          https://docs.posit.co/ide/server-pro/authenticating_users/aws_credentials.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                              https://docs.posit.co/ide/server-pro/authenticating_users/delegated_azure_credentials.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                     https://docs.posit.co/ide/server-pro/getting_started/installation/installation.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                          https://docs.posit.co/ide/server-pro/getting_started/installation/kubernetes_installation.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                        https://docs.posit.co/ide/server-pro/getting_started/installation/multi_server_installation.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                                    https://docs.posit.co/ide/server-pro/getting_started/networking.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                                  https://docs.posit.co/ide/server-pro/getting_started/requirements.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                                       https://docs.posit.co/ide/server-pro/getting_started/support.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                                                         https://docs.posit.co/ide/server-pro/index.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                                     https://docs.posit.co/ide/server-pro/integration/aws-sagemaker.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                  https://docs.posit.co/ide/server-pro/integration/jupyter-multiple-python-versions.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                                https://docs.posit.co/ide/server-pro/integration/jupyter-standalone.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                                    https://docs.posit.co/ide/server-pro/integration/launcher-slurm.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                                        https://docs.posit.co/ide/server-pro/integration/reticulate.html 
#>                                                                                                               "2026-10-14 22:41:25 UTC" 
#>                                                                                          https://docs.posit.co/ide/server-pro/licenses/ 
#>                                                                                                               "2026-10-14 22:41:26 UTC" 
#>                                                                                https://docs.posit.co/ide/server-pro/licenses/index.html 
#>                                                                                                               "2026-10-14 22:41:26 UTC" 
#>                                                                                https://docs.posit.co/ide/server-pro/load-balancing.html 
#>                                                                                                               "2026-10-14 22:41:26 UTC" 
#>                                                                 https://docs.posit.co/ide/server-pro/load_balancing/load_balancing.html 
#>                                                                                                               "2026-10-14 22:41:26 UTC" 
#>                                                                                    https://docs.posit.co/ide/server-pro/r-sessions.html 
#>                                                                                                               "2026-10-14 22:41:26 UTC" 
#>                                                                           https://docs.posit.co/ide/server-pro/reference-architectures/ 
#>                                                                                                               "2026-10-14 22:41:27 UTC" 
#>                                                               https://docs.posit.co/ide/server-pro/reference-architectures/aws-eks.html 
#>                                                                                                               "2025-04-03 19:54:08 UTC" 
#>                                                   https://docs.posit.co/ide/server-pro/reference-architectures/aws-parallelcluster.html 
#>                                                                                                               "2025-04-03 19:54:08 UTC" 
#>                                                     https://docs.posit.co/ide/server-pro/reference-architectures/aws-single-server.html 
#>                                                                                                               "2025-04-03 19:54:08 UTC" 
#>                                                 https://docs.posit.co/ide/server-pro/reference-architectures/generic-architectures.html 
#>                                                                                                               "2026-10-14 22:41:27 UTC" 
#>                                                                        https://docs.posit.co/ide/server-pro/reference/rserver_conf.html 
#>                                                                                                               "2025-09-08 22:19:16 UTC" 
#>                                                                                              https://docs.posit.co/ide/server-pro/user/ 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                                                    https://docs.posit.co/ide/server-pro/user/index.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                        https://docs.posit.co/ide/server-pro/user/jupyter-lab/getting-started/index.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                             https://docs.posit.co/ide/server-pro/user/jupyter-lab/guide/extensions.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                                    https://docs.posit.co/ide/server-pro/user/jupyter-lab/guide/git.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                              https://docs.posit.co/ide/server-pro/user/jupyter-lab/guide/jupyter-kernel-management.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                              https://docs.posit.co/ide/server-pro/user/jupyter-lab/guide/posit-workbench-extension.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                    https://docs.posit.co/ide/server-pro/user/jupyter-lab/guide/recommended-settings-and-extensions.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                               https://docs.posit.co/ide/server-pro/user/jupyter-lab/guide/settings.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                                https://docs.posit.co/ide/server-pro/user/jupyter-lab/reference/faq.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                    https://docs.posit.co/ide/server-pro/user/jupyter-lab/reference/other-resources.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                   https://docs.posit.co/ide/server-pro/user/jupyter-notebook/getting-started/index.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                             https://docs.posit.co/ide/server-pro/user/jupyter-notebook/guide/index.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                           https://docs.posit.co/ide/server-pro/user/jupyter-notebook/reference/faq.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                               https://docs.posit.co/ide/server-pro/user/jupyter-notebook/reference/other-resources.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                    https://docs.posit.co/ide/server-pro/user/posit-workbench/getting-started/index.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                     https://docs.posit.co/ide/server-pro/user/posit-workbench/guide/agent-activity.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                             https://docs.posit.co/ide/server-pro/user/posit-workbench/guide/python.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                                  https://docs.posit.co/ide/server-pro/user/posit-workbench/guide/r.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                       https://docs.posit.co/ide/server-pro/user/posit-workbench/guide/requirements.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                 https://docs.posit.co/ide/server-pro/user/posit-workbench/guide/session-management.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                            https://docs.posit.co/ide/server-pro/user/posit-workbench/guide/users-admins-superusers.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                  https://docs.posit.co/ide/server-pro/user/posit-workbench/managed-credentials/aws.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                https://docs.posit.co/ide/server-pro/user/posit-workbench/managed-credentials/azure.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                         https://docs.posit.co/ide/server-pro/user/posit-workbench/managed-credentials/custom_oauth.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                           https://docs.posit.co/ide/server-pro/user/posit-workbench/managed-credentials/databricks.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                  https://docs.posit.co/ide/server-pro/user/posit-workbench/managed-credentials/managed-credentials.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                            https://docs.posit.co/ide/server-pro/user/posit-workbench/managed-credentials/snowflake.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                            https://docs.posit.co/ide/server-pro/user/posit-workbench/reference/faq.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                https://docs.posit.co/ide/server-pro/user/posit-workbench/reference/other-resources.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                                     https://docs.posit.co/ide/server-pro/user/positron/getting-started/ 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                           https://docs.posit.co/ide/server-pro/user/positron/getting-started/index.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                     https://docs.posit.co/ide/server-pro/user/positron/guide/available-credentials.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                                https://docs.posit.co/ide/server-pro/user/positron/guide/extensions.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                                       https://docs.posit.co/ide/server-pro/user/positron/guide/git.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                               https://docs.posit.co/ide/server-pro/user/positron/guide/interpreter.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                           https://docs.posit.co/ide/server-pro/user/positron/guide/posit-assistant.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                 https://docs.posit.co/ide/server-pro/user/positron/guide/posit-workbench-extension.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                      https://docs.posit.co/ide/server-pro/user/positron/guide/proxying-web-servers.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                       https://docs.posit.co/ide/server-pro/user/positron/guide/recommended-settings-and-extensions.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                           https://docs.posit.co/ide/server-pro/user/positron/guide/session-timeout.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                                  https://docs.posit.co/ide/server-pro/user/positron/guide/settings.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                 https://docs.posit.co/ide/server-pro/user/positron/guide/using-managed-credentials.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                            https://docs.posit.co/ide/server-pro/user/positron/guide/workbench-jobs.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                                   https://docs.posit.co/ide/server-pro/user/positron/reference/faq.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                       https://docs.posit.co/ide/server-pro/user/positron/reference/other-resources.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                        https://docs.posit.co/ide/server-pro/user/rstudio-pro/getting-started/index.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                             https://docs.posit.co/ide/server-pro/user/rstudio-pro/guide/databricks.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                                    https://docs.posit.co/ide/server-pro/user/rstudio-pro/guide/git.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                    https://docs.posit.co/ide/server-pro/user/rstudio-pro/guide/multiple-r-versions.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                        https://docs.posit.co/ide/server-pro/user/rstudio-pro/guide/project-sharing.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                        https://docs.posit.co/ide/server-pro/user/rstudio-pro/guide/session-timeout.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                              https://docs.posit.co/ide/server-pro/user/rstudio-pro/guide/using-managed-credentials.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                         https://docs.posit.co/ide/server-pro/user/rstudio-pro/guide/workbench-jobs.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                                https://docs.posit.co/ide/server-pro/user/rstudio-pro/reference/faq.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                    https://docs.posit.co/ide/server-pro/user/rstudio-pro/reference/other-resources.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                            https://docs.posit.co/ide/server-pro/user/vs-code/getting-started/index.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                      https://docs.posit.co/ide/server-pro/user/vs-code/guide/available-credentials.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                                 https://docs.posit.co/ide/server-pro/user/vs-code/guide/extensions.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                                        https://docs.posit.co/ide/server-pro/user/vs-code/guide/git.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                  https://docs.posit.co/ide/server-pro/user/vs-code/guide/posit-workbench-extension.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                       https://docs.posit.co/ide/server-pro/user/vs-code/guide/proxying-web-servers.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                        https://docs.posit.co/ide/server-pro/user/vs-code/guide/python-environments.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                        https://docs.posit.co/ide/server-pro/user/vs-code/guide/recommended-settings-and-extensions.html 
#>                                                                                                               "2026-10-14 22:41:37 UTC" 
#>                                                            https://docs.posit.co/ide/server-pro/user/vs-code/guide/session-timeout.html 
#>                                                                                                               "2026-10-14 22:41:38 UTC" 
#>                                                                   https://docs.posit.co/ide/server-pro/user/vs-code/guide/settings.html 
#>                                                                                                               "2026-10-14 22:41:38 UTC" 
#>                                                  https://docs.posit.co/ide/server-pro/user/vs-code/guide/using-managed-credentials.html 
#>                                                                                                               "2026-10-14 22:41:38 UTC" 
#>                                                             https://docs.posit.co/ide/server-pro/user/vs-code/guide/workbench-jobs.html 
#>                                                                                                               "2026-10-14 22:41:38 UTC" 
#>                                                               https://docs.posit.co/ide/server-pro/user/vs-code/reference/appendix.html 
#>                                                                                                               "2026-10-14 22:41:38 UTC" 
#>                                                                    https://docs.posit.co/ide/server-pro/user/vs-code/reference/faq.html 
#>                                                                                                               "2026-10-14 22:41:38 UTC" 
#>                                                        https://docs.posit.co/ide/server-pro/user/vs-code/reference/other-resources.html 
#>                                                                                                               "2026-10-14 22:41:38 UTC" 
#>                                                               https://docs.posit.co/ide/server-pro/user_provisioning/configuration.html 
#>                                                                                                               "2026-10-14 22:41:38 UTC" 
#>                                                           https://docs.posit.co/ide/server-pro/user_provisioning/user_provisioning.html 
#>                                                                                                               "2026-10-14 22:41:38 UTC" 
#>                                                                                                         https://docs.posit.co/ide/user/ 
#>                                                                                                               "2026-10-14 22:31:14 UTC" 
#>                                                                                               https://docs.posit.co/ide/user/2026.10.0/ 
#>                                                                                                               "2026-10-15 17:12:20 UTC" 
#>                                                                            https://docs.posit.co/ide/user/ide/guide/tools/posit-ai.html 
#>                                                                                                               "2026-10-14 22:31:14 UTC" 
#>                                                                                                        https://docs.posit.co/index.html 
#>                                                                                                               "2026-10-15 18:37:04 UTC" 
#>                                                                                                  https://docs.posit.co/install-planner/ 
#>                                                                                                               "2026-10-15 18:37:04 UTC" 
#>                                                                                                     https://docs.posit.co/installation/ 
#>                                                                                                               "2026-10-15 18:37:04 UTC" 
#>                                                                                                     https://docs.posit.co/job-launcher/ 
#>                                                                                                               "2023-08-31 14:06:50 UTC" 
#>                                                                                      https://docs.posit.co/licensing/licensing-faq.html 
#>                                                                                                               "2026-10-15 18:37:04 UTC" 
#>                                                                                          https://docs.posit.co/licensing/licensing.html 
#>                                                                                                               "2026-10-15 18:37:04 UTC" 
#>                                                                                                     https://docs.posit.co/partnerships/ 
#>                                                                                                               "2026-10-15 15:43:02 UTC" 
#>                                                                                       https://docs.posit.co/partnerships/aws-sagemaker/ 
#>                                                                                                               "2026-10-15 15:43:02 UTC" 
#>                                                                             https://docs.posit.co/partnerships/aws-sagemaker/index.html 
#>                                                                                                               "2026-10-15 15:43:02 UTC" 
#>                                                                    https://docs.posit.co/partnerships/aws-sagemaker/positron/admin.html 
#>                                                                                                               "2026-10-15 15:43:02 UTC" 
#>                                                                     https://docs.posit.co/partnerships/aws-sagemaker/positron/user.html 
#>                                                                                                               "2026-10-15 15:43:02 UTC" 
#>                                                                                https://docs.posit.co/partnerships/databricks/index.html 
#>                                                                                                               "2026-10-15 15:43:02 UTC" 
#>                                                                                           https://docs.posit.co/partnerships/snowflake/ 
#>                                                                                                               "2026-10-15 15:43:03 UTC" 
#>                                                                                 https://docs.posit.co/partnerships/snowflake/index.html 
#>                                                                                                               "2026-10-15 15:43:03 UTC" 
#>                                                                    https://docs.posit.co/partnerships/snowflake/posit-team/install.html 
#>                                                                                                               "2026-10-15 15:43:03 UTC" 
#>                                                                                             https://docs.posit.co/platform-support.html 
#>                                                                                                               "2026-10-15 18:37:05 UTC" 
#>                                                                                                         https://docs.posit.co/posit-ai/ 
#>                                                                                                               "2026-10-01 22:15:20 UTC" 
#>                                                                                                    https://docs.posit.co/posit-ai/user/ 
#>                                                                                                               "2026-10-01 22:15:20 UTC" 
#>                                                                                    https://docs.posit.co/posit-ai/user/getting-started/ 
#>                                                                                                               "2026-10-01 22:15:20 UTC" 
#>                                                                                               https://docs.posit.co/posit-ai/whats-new/ 
#>                                                                                                               "2026-10-01 22:15:20 UTC" 
#>                                                                                                       https://docs.posit.co/posit-team/ 
#>                                                                                                               "2026-10-15 18:37:05 UTC" 
#>                                                                                             https://docs.posit.co/posit-team/index.html 
#>                                                                                                               "2026-10-15 18:37:05 UTC" 
#>                                                                                                https://docs.posit.co/previous-versions/ 
#>                                                                                                               "2026-10-15 18:37:05 UTC" 
#>                                                                                        https://docs.posit.co/previous-versions/connect/ 
#>                                                                                                               "2026-10-15 18:37:05 UTC" 
#>                                                                            https://docs.posit.co/previous-versions/package-manager.html 
#>                                                                                                               "2026-10-15 18:37:05 UTC" 
#>                                                                                        https://docs.posit.co/previous-versions/rstudio/ 
#>                                                                                                               "2026-10-15 18:37:05 UTC" 
#>                                                                                      https://docs.posit.co/reference-architectures.html 
#>                                                                                                               "2026-10-15 18:37:05 UTC" 
#>                                                                                  https://docs.posit.co/reference-architectures/aws.html 
#>                                                                                                               "2026-10-15 18:37:05 UTC" 
#>                                                                                https://docs.posit.co/reference-architectures/azure.html 
#>                                                                                                               "2026-10-15 18:37:05 UTC" 
#>                                                                           https://docs.posit.co/reference-architectures/posit-team.html 
#>                                                                                                               "2026-10-15 18:37:05 UTC" 
#>                                                                                     https://docs.posit.co/resources/install-nodejs.html 
#>                                                                                                               "2026-10-15 18:37:05 UTC" 
#>                                                                               https://docs.posit.co/resources/install-python-posit.html 
#>                                                                                                               "2026-10-15 18:37:05 UTC" 
#>                                                                                  https://docs.posit.co/resources/install-python-uv.html 
#>                                                                                                               "2026-10-15 18:37:05 UTC" 
#>                                                                                     https://docs.posit.co/resources/install-quarto.html 
#>                                                                                                               "2026-10-15 18:37:06 UTC" 
#>                                                                                    https://docs.posit.co/resources/install-r-source.qmd 
#>                                                                                                                                      NA 
#>                                                                                          https://docs.posit.co/resources/install-r.html 
#>                                                                                                               "2026-10-15 18:37:06 UTC" 
#>                                                                                              https://docs.posit.co/resources/install-r/ 
#>                                                                                                               "2026-10-15 18:37:06 UTC" 
#>                                                                                        https://docs.posit.co/resources/open-source.html 
#>                                                                                                               "2026-10-15 18:37:06 UTC" 
#>                                                                                             https://docs.posit.co/rsc/integration/rspm/ 
#>                                                                                                               "2026-10-15 18:37:06 UTC" 
#>                                                                                                 https://docs.posit.co/rsconnect-python/ 
#>                                                                                                               "2026-09-10 12:49:37 UTC" 
#>                                                                                                             https://docs.posit.co/rspm/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                             https://docs.posit.co/rspm/2023.04.0/admin/file-storage/s3/ 
#>                                                                                                               "2025-01-30 19:50:13 UTC" 
#>                                                               https://docs.posit.co/rspm/2023.04.0/admin/getting-started/configuration/ 
#>                                                                                                               "2025-01-30 19:50:13 UTC" 
#>                                                                            https://docs.posit.co/rspm/2023.04.0/admin/security/package/ 
#>                                                                                                               "2025-01-30 19:50:13 UTC" 
#>                                                                                https://docs.posit.co/rspm/2023.08.0/admin/repositories/ 
#>                                                                                                               "2025-01-30 19:50:11 UTC" 
#>                                                                 https://docs.posit.co/rspm/2024.04.4/admin/appendix/airgapped-installs/ 
#>                                                                                                               "2024-06-26 20:26:58 UTC" 
#>                                                                    https://docs.posit.co/rspm/2024.04.4/admin/r-packaging/curated-cran/ 
#>                                                                                                               "2024-06-26 20:26:59 UTC" 
#>                                                                                   https://docs.posit.co/rspm/2024.08.0/admin/admin-cli/ 
#>                                                                                                               "2024-08-09 20:12:17 UTC" 
#>                                                                      https://docs.posit.co/rspm/2024.08.0/admin/appendix/configuration/ 
#>                                                                                                               "2024-08-09 20:12:18 UTC" 
#>                                                               https://docs.posit.co/rspm/2024.08.0/admin/getting-started/configuration/ 
#>                                                                                                               "2024-08-09 20:12:18 UTC" 
#>                                                                            https://docs.posit.co/rspm/2024.11.0/admin/file-storage/nfs/ 
#>                                                                                                               "2024-12-04 20:01:44 UTC" 
#>                                                                https://docs.posit.co/rspm/2024.11.0/admin/getting-started/requirements/ 
#>                                                                                                               "2024-12-04 20:01:44 UTC" 
#>                                                                           https://docs.posit.co/rspm/2024.11.0/admin/metadata/metadata/ 
#>                                                                                                               "2024-12-04 20:01:44 UTC" 
#>                              https://docs.posit.co/rspm/2024.11.0/admin/reference-architectures/azure/reference-architecture-azure-k8s/ 
#>                                                                                                               "2024-12-04 20:01:45 UTC" 
#>                               https://docs.posit.co/rspm/2024.11.0/admin/reference-architectures/azure/reference-architecture-azure-lb/ 
#>                                                                                                               "2024-12-04 20:01:45 UTC" 
#>                    https://docs.posit.co/rspm/2024.11.0/admin/reference-architectures/azure/reference-architecture-azure-single-server/ 
#>                                                                                                               "2024-12-04 20:01:45 UTC" 
#>                                                                 https://docs.posit.co/rspm/2024.11.0/admin/reference-architectures/faq/ 
#>                                                                                                               "2024-12-04 20:01:45 UTC" 
#>                                               https://docs.posit.co/rspm/2024.11.0/admin/reference-architectures/generic-architectures/ 
#>                                                                                                               "2024-12-04 20:01:45 UTC" 
#>                                                                          https://docs.posit.co/rspm/2025.04.0/admin/authentication.html 
#>                                                                                                               "2025-04-10 18:10:49 UTC" 
#>                                                                        https://docs.posit.co/rspm/2025.04.0/admin/serving-binaries.html 
#>                                                                                                               "2025-04-10 18:10:50 UTC" 
#>                                                                    https://docs.posit.co/rspm/2025.09.0/admin/authentication/index.html 
#>                                                                                                               "2025-09-09 17:16:51 UTC" 
#>                                                                        https://docs.posit.co/rspm/2025.09.0/admin/serving-binaries.html 
#>                                                                                                               "2025-09-09 17:16:51 UTC" 
#>                                                                                  https://docs.posit.co/rspm/2025.12.0/admin/ai/mcp.html 
#>                                                                                                               "2025-12-23 22:45:10 UTC" 
#>                                                                       https://docs.posit.co/rspm/2025.12.0/admin/building-packages.html 
#>                                                                                                               "2025-12-23 22:45:11 UTC" 
#>                                                                                   https://docs.posit.co/rspm/2025.12.0/admin/proxy.html 
#>                                                                                                               "2025-12-23 22:45:11 UTC" 
#>                                                           https://docs.posit.co/rspm/2026.04.0/admin/getting-started/configuration.html 
#>                                                                                                               "2026-04-09 02:40:03 UTC" 
#>                                                                 https://docs.posit.co/rspm/2026.04.0/admin/vsx-packaging/local-vsx.html 
#>                                                                                                               "2026-04-09 02:40:05 UTC" 
#>                                                                   https://docs.posit.co/rspm/2026.04.0/admin/vsx-packaging/openvsx.html 
#>                                                                                                               "2026-04-09 02:40:05 UTC" 
#>                                                                         https://docs.posit.co/rspm/2026.06.0/admin/plugins/plugins.html 
#>                                                                                                               "2026-06-30 20:52:38 UTC" 
#>                                                                          https://docs.posit.co/rspm/2026.06.0/user/getting-started.html 
#>                                                                                                               "2026-06-30 20:52:41 UTC" 
#>                                                        https://docs.posit.co/rspm/2026.06.0/user/packages/customizing-package-tabs.html 
#>                                                                                                               "2026-06-30 20:52:41 UTC" 
#>                                                                                  https://docs.posit.co/rspm/2026.06.0/user/plugins.html 
#>                                                                                                               "2026-06-30 20:52:41 UTC" 
#>                                                                       https://docs.posit.co/rspm/2026.08.0/admin/admin-ui/overview.html 
#>                                                                                                               "2026-08-10 22:17:57 UTC" 
#>                                                            https://docs.posit.co/rspm/2026.08.0/admin/appendix/air-gapped-installs.html 
#>                                                                                                               "2026-08-10 22:17:57 UTC" 
#>                                         https://docs.posit.co/rspm/2026.08.0/admin/integrations/openid-connect-identity-federation.html 
#>                                                                                                               "2026-08-10 22:18:03 UTC" 
#>                                                                        https://docs.posit.co/rspm/2026.08.0/admin/serving-binaries.html 
#>                                                                                                               "2026-08-10 22:18:04 UTC" 
#>                                                                                 https://docs.posit.co/rspm/2026.08.0/admin/swagger.html 
#>                                                                                                               "2026-08-10 22:18:04 UTC" 
#>                                                            https://docs.posit.co/rspm/2026.09.0/admin/appendix/air-gapped-installs.html 
#>                                                                                                               "2026-09-09 14:03:29 UTC" 
#>                                                                     https://docs.posit.co/rspm/2026.09.0/admin/appendix/encryption.html 
#>                                                                                                               "2026-09-09 14:03:30 UTC" 
#>                                                          https://docs.posit.co/rspm/2026.09.0/admin/appendix/experimental-features.html 
#>                                                                                                               "2026-09-09 14:03:30 UTC" 
#>                                             https://docs.posit.co/rspm/2026.09.0/admin/getting-started/installation/installing-ppm.html 
#>                                                                                                               "2026-09-09 14:03:30 UTC" 
#>                                                                               https://docs.posit.co/rspm/2026.09.0/admin/licensing.html 
#>                                                                                                               "2026-09-09 14:03:30 UTC" 
#>                                                                               https://docs.posit.co/rspm/2026.09.0/admin/migration.html 
#>                                                                                                               "2026-09-09 14:03:30 UTC" 
#>                                                                         https://docs.posit.co/rspm/2026.09.0/admin/plugins/plugins.html 
#>                                                                                                               "2026-09-09 14:03:30 UTC" 
#>                                                                                   https://docs.posit.co/rspm/2026.09.0/admin/proxy.html 
#>                                                                                                               "2026-09-09 14:03:30 UTC" 
#>                                                                       https://docs.posit.co/rspm/2026.09.0/admin/server-management.html 
#>                                                                                                               "2026-09-09 14:03:31 UTC" 
#>                                                                                  https://docs.posit.co/rspm/2026.09.0/user/plugins.html 
#>                                                                                                               "2026-09-09 14:03:32 UTC" 
#>                                                                                                       https://docs.posit.co/rspm/admin/ 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                         https://docs.posit.co/rspm/admin/admin-cli.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                                             https://docs.posit.co/rspm/admin/admin-cli/ 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                                    https://docs.posit.co/rspm/admin/admin-ui/index.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                                 https://docs.posit.co/rspm/admin/admin-ui/overview.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                         https://docs.posit.co/rspm/admin/admin-ui/token-management.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                                            https://docs.posit.co/rspm/admin/ai/mcp.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                                            https://docs.posit.co/rspm/admin/alerts.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                      https://docs.posit.co/rspm/admin/appendix/air-gapped-installs.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                       https://docs.posit.co/rspm/admin/appendix/airgapped-installs.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                           https://docs.posit.co/rspm/admin/appendix/airgapped-installs/ 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                                https://docs.posit.co/rspm/admin/appendix/bootstrap.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                      https://docs.posit.co/rspm/admin/appendix/changing-runas-user.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                          https://docs.posit.co/rspm/admin/appendix/changing-runas-user/ 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                                 https://docs.posit.co/rspm/admin/appendix/cli/rspm.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                             https://docs.posit.co/rspm/admin/appendix/cli/rspm_add.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                      https://docs.posit.co/rspm/admin/appendix/cli/rspm_add_binary.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                         https://docs.posit.co/rspm/admin/appendix/cli/rspm_archive.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                       https://docs.posit.co/rspm/admin/appendix/cli/rspm_bootstrap.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                  https://docs.posit.co/rspm/admin/appendix/cli/rspm_bootstrap_keys.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                             https://docs.posit.co/rspm/admin/appendix/cli/rspm_bootstrap_signature.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                 https://docs.posit.co/rspm/admin/appendix/cli/rspm_bootstrap_token.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                         https://docs.posit.co/rspm/admin/appendix/cli/rspm_cluster.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                   https://docs.posit.co/rspm/admin/appendix/cli/rspm_cluster_nodes.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                 https://docs.posit.co/rspm/admin/appendix/cli/rspm_cluster_offline.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                  https://docs.posit.co/rspm/admin/appendix/cli/rspm_cluster_online.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                      https://docs.posit.co/rspm/admin/appendix/cli/rspm_completion.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                                          https://docs.posit.co/rspm/admin/appendix/cli/rspm_config.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                             https://docs.posit.co/rspm/admin/appendix/cli/rspm_config_experimental.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                     https://docs.posit.co/rspm/admin/appendix/cli/rspm_config_experimental_disable.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                      https://docs.posit.co/rspm/admin/appendix/cli/rspm_config_experimental_enable.html 
#>                                                                                                               "2026-10-14 20:13:13 UTC" 
#>                                                        https://docs.posit.co/rspm/admin/appendix/cli/rspm_config_experimental_list.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                        https://docs.posit.co/rspm/admin/appendix/cli/rspm_config_experimental_show.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                      https://docs.posit.co/rspm/admin/appendix/cli/rspm_config_log.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                          https://docs.posit.co/rspm/admin/appendix/cli/rspm_create.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                           https://docs.posit.co/rspm/admin/appendix/cli/rspm_create_blocklist-rule.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                     https://docs.posit.co/rspm/admin/appendix/cli/rspm_create_bulk-blocklist-rules.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                            https://docs.posit.co/rspm/admin/appendix/cli/rspm_create_bulk-metadata.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                              https://docs.posit.co/rspm/admin/appendix/cli/rspm_create_git-builder.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                 https://docs.posit.co/rspm/admin/appendix/cli/rspm_create_metadata.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                     https://docs.posit.co/rspm/admin/appendix/cli/rspm_create_repo.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                   https://docs.posit.co/rspm/admin/appendix/cli/rspm_create_source.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                    https://docs.posit.co/rspm/admin/appendix/cli/rspm_create_token.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                          https://docs.posit.co/rspm/admin/appendix/cli/rspm_delete.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                           https://docs.posit.co/rspm/admin/appendix/cli/rspm_delete_blocklist-rule.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                 https://docs.posit.co/rspm/admin/appendix/cli/rspm_delete_metadata.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                            https://docs.posit.co/rspm/admin/appendix/cli/rspm_edit.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                             https://docs.posit.co/rspm/admin/appendix/cli/rspm_edit_blocklist-rule.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                https://docs.posit.co/rspm/admin/appendix/cli/rspm_edit_git-builder.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                           https://docs.posit.co/rspm/admin/appendix/cli/rspm_edit_https-credential.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                       https://docs.posit.co/rspm/admin/appendix/cli/rspm_edit_repo.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                     https://docs.posit.co/rspm/admin/appendix/cli/rspm_edit_source.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                    https://docs.posit.co/rspm/admin/appendix/cli/rspm_edit_ssh-key.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                         https://docs.posit.co/rspm/admin/appendix/cli/rspm_encrypt.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                https://docs.posit.co/rspm/admin/appendix/cli/rspm_encrypt_generate.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                           https://docs.posit.co/rspm/admin/appendix/cli/rspm_evict.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                             https://docs.posit.co/rspm/admin/appendix/cli/rspm_hup.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                          https://docs.posit.co/rspm/admin/appendix/cli/rspm_import.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                         https://docs.posit.co/rspm/admin/appendix/cli/rspm_import_https-credential.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                  https://docs.posit.co/rspm/admin/appendix/cli/rspm_import_ssh-key.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                            https://docs.posit.co/rspm/admin/appendix/cli/rspm_info.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                            https://docs.posit.co/rspm/admin/appendix/cli/rspm_list.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                               https://docs.posit.co/rspm/admin/appendix/cli/rspm_list_bioconductor.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                            https://docs.posit.co/rspm/admin/appendix/cli/rspm_list_blocklist-rules.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                       https://docs.posit.co/rspm/admin/appendix/cli/rspm_list_cran.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                              https://docs.posit.co/rspm/admin/appendix/cli/rspm_list_distributions.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                               https://docs.posit.co/rspm/admin/appendix/cli/rspm_list_git-builders.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                 https://docs.posit.co/rspm/admin/appendix/cli/rspm_list_git-builds.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                            https://docs.posit.co/rspm/admin/appendix/cli/rspm_list_git-credentials.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                              https://docs.posit.co/rspm/admin/appendix/cli/rspm_list_license-types.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                   https://docs.posit.co/rspm/admin/appendix/cli/rspm_list_metadata.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                   https://docs.posit.co/rspm/admin/appendix/cli/rspm_list_packages.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                      https://docs.posit.co/rspm/admin/appendix/cli/rspm_list_repos.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                               https://docs.posit.co/rspm/admin/appendix/cli/rspm_list_requirements.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                    https://docs.posit.co/rspm/admin/appendix/cli/rspm_list_sources.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                   https://docs.posit.co/rspm/admin/appendix/cli/rspm_list_ssh-keys.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                     https://docs.posit.co/rspm/admin/appendix/cli/rspm_list_tokens.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                           https://docs.posit.co/rspm/admin/appendix/cli/rspm_login.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                       https://docs.posit.co/rspm/admin/appendix/cli/rspm_login_sso.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                            https://docs.posit.co/rspm/admin/appendix/cli/rspm_logs.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                         https://docs.posit.co/rspm/admin/appendix/cli/rspm_offline.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                          https://docs.posit.co/rspm/admin/appendix/cli/rspm_online.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                          https://docs.posit.co/rspm/admin/appendix/cli/rspm_remove.html 
#>                                                                                                               "2026-10-14 20:13:14 UTC" 
#>                                                                         https://docs.posit.co/rspm/admin/appendix/cli/rspm_reorder.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                           https://docs.posit.co/rspm/admin/appendix/cli/rspm_rerun.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                          https://docs.posit.co/rspm/admin/appendix/cli/rspm_revoke.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                    https://docs.posit.co/rspm/admin/appendix/cli/rspm_revoke_token.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                             https://docs.posit.co/rspm/admin/appendix/cli/rspm_run.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                       https://docs.posit.co/rspm/admin/appendix/cli/rspm_subscribe.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                            https://docs.posit.co/rspm/admin/appendix/cli/rspm_sync.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                            https://docs.posit.co/rspm/admin/appendix/cli/rspm_test.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                            https://docs.posit.co/rspm/admin/appendix/cli/rspm_test_blocklist-rules.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                     https://docs.posit.co/rspm/admin/appendix/cli/rspm_unsubscribe.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                          https://docs.posit.co/rspm/admin/appendix/cli/rspm_update.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                             https://docs.posit.co/rspm/admin/appendix/cli/rspm_url.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                      https://docs.posit.co/rspm/admin/appendix/cli/rspm_url_create.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                     https://docs.posit.co/rspm/admin/appendix/cli/rspm_url_explain.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                          https://docs.posit.co/rspm/admin/appendix/cli/rspm_verify.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                            https://docs.posit.co/rspm/admin/appendix/configuration.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                                  https://docs.posit.co/rspm/admin/appendix/crontab.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                               https://docs.posit.co/rspm/admin/appendix/encryption.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                    https://docs.posit.co/rspm/admin/appendix/experimental-features.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                            https://docs.posit.co/rspm/admin/appendix/minimal-root-use-installation.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                      https://docs.posit.co/rspm/admin/appendix/offline-cli/rspm-offline-downloader.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                  https://docs.posit.co/rspm/admin/appendix/offline-cli/rspm-offline-downloader_get.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                     https://docs.posit.co/rspm/admin/appendix/offline-cli/rspm-offline-downloader_get_bioconductor.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                             https://docs.posit.co/rspm/admin/appendix/offline-cli/rspm-offline-downloader_get_cran.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                          https://docs.posit.co/rspm/admin/appendix/offline-cli/rspm-offline-downloader_get_openvsx.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                             https://docs.posit.co/rspm/admin/appendix/offline-cli/rspm-offline-downloader_get_pypi.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                            https://docs.posit.co/rspm/admin/appendix/offline-cli/rspm-offline-downloader_get_vulns.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                             https://docs.posit.co/rspm/admin/appendix/offline-cli/rspm-offline-downloader_validate.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                https://docs.posit.co/rspm/admin/appendix/offline-cli/rspm-offline-downloader_validate_bioconductor.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                        https://docs.posit.co/rspm/admin/appendix/offline-cli/rspm-offline-downloader_validate_cran.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                     https://docs.posit.co/rspm/admin/appendix/offline-cli/rspm-offline-downloader_validate_openvsx.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                        https://docs.posit.co/rspm/admin/appendix/offline-cli/rspm-offline-downloader_validate_pypi.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                        https://docs.posit.co/rspm/admin/appendix/package-ecosystem.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                       https://docs.posit.co/rspm/admin/appendix/r-python-detection.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                       https://docs.posit.co/rspm/admin/appendix/repo-request-types.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                         https://docs.posit.co/rspm/admin/appendix/requirements-txt.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                           https://docs.posit.co/rspm/admin/appendix/source-details.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                          https://docs.posit.co/rspm/admin/appendix/storage-classes.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                              https://docs.posit.co/rspm/admin/appendix/system-dependency-detection.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                  https://docs.posit.co/rspm/admin/appendix/system-dependency-detection/ 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                           https://docs.posit.co/rspm/admin/authentication/entra-id.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                              https://docs.posit.co/rspm/admin/authentication/index.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                               https://docs.posit.co/rspm/admin/authentication/okta.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                     https://docs.posit.co/rspm/admin/authentication/openid-connect.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                      https://docs.posit.co/rspm/admin/authentication/scope-mapping.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                               https://docs.posit.co/rspm/admin/authentication/token-authentication.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                                 https://docs.posit.co/rspm/admin/building-packages.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                                     https://docs.posit.co/rspm/admin/building-packages/ 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                                  https://docs.posit.co/rspm/admin/chronicle/events.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                                   https://docs.posit.co/rspm/admin/chronicle/index.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                                           https://docs.posit.co/rspm/admin/connect.html 
#>                                                                                                               "2026-10-14 20:13:15 UTC" 
#>                                                                                          https://docs.posit.co/rspm/admin/database.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                         https://docs.posit.co/rspm/admin/file-storage/file-storage.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                  https://docs.posit.co/rspm/admin/file-storage/nfs.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                   https://docs.posit.co/rspm/admin/file-storage/s3.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                     https://docs.posit.co/rspm/admin/getting-started/configuration.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                         https://docs.posit.co/rspm/admin/getting-started/configuration/ 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                               https://docs.posit.co/rspm/admin/getting-started/faq.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                           https://docs.posit.co/rspm/admin/getting-started/installation 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                          https://docs.posit.co/rspm/admin/getting-started/installation/ 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                https://docs.posit.co/rspm/admin/getting-started/installation/index.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                              https://docs.posit.co/rspm/admin/getting-started/installation/installation-kubernetes.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                  https://docs.posit.co/rspm/admin/getting-started/installation/installation-kubernetes/ 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                            https://docs.posit.co/rspm/admin/getting-started/installation/installation-multi-server.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                       https://docs.posit.co/rspm/admin/getting-started/installation/installing-ppm.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                      https://docs.posit.co/rspm/admin/getting-started/requirements.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                          https://docs.posit.co/rspm/admin/getting-started/requirements/ 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                            https://docs.posit.co/rspm/admin/getting-started/sizing.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                             https://docs.posit.co/rspm/admin/getting-started/user-guide 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                                https://docs.posit.co/rspm/admin/ha.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                                    https://docs.posit.co/rspm/admin/ha/ 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                             https://docs.posit.co/rspm/admin/index.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                          https://docs.posit.co/rspm/admin/integrations/artifactory.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                  https://docs.posit.co/rspm/admin/integrations/aws.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                https://docs.posit.co/rspm/admin/integrations/azure.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                      https://docs.posit.co/rspm/admin/integrations/nexusrepository.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                   https://docs.posit.co/rspm/admin/integrations/openid-connect-identity-federation.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                            https://docs.posit.co/rspm/admin/integrations/snowflake.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                         https://docs.posit.co/rspm/admin/licensing.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                             https://docs.posit.co/rspm/admin/licensing/ 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                           https://docs.posit.co/rspm/admin/logging.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                           https://docs.posit.co/rspm/admin/managing-change-control.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                 https://docs.posit.co/rspm/admin/metadata/metadata.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                         https://docs.posit.co/rspm/admin/migration.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                             https://docs.posit.co/rspm/admin/networking 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                        https://docs.posit.co/rspm/admin/networking.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                            https://docs.posit.co/rspm/admin/networking/ 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                               https://docs.posit.co/rspm/admin/operational-metrics.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                   https://docs.posit.co/rspm/admin/operational-metrics/ 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                   https://docs.posit.co/rspm/admin/plugins/plugins.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                https://docs.posit.co/rspm/admin/process-management.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                                             https://docs.posit.co/rspm/admin/proxy.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                     https://docs.posit.co/rspm/admin/python-packaging/curated-pypi.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                     https://docs.posit.co/rspm/admin/python-packaging/local-python.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                           https://docs.posit.co/rspm/admin/python-packaging/python.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                  https://docs.posit.co/rspm/admin/r-packaging/curated-bioconductor.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                                                          https://docs.posit.co/rspm/admin/r-packaging/curated-cran.html 
#>                                                                                                               "2026-10-14 20:13:16 UTC" 
#>                                        https://docs.posit.co/rspm/admin/reference-architectures/aws/reference-architecture-aws-k8s.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                            https://docs.posit.co/rspm/admin/reference-architectures/aws/reference-architecture-aws-k8s/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                         https://docs.posit.co/rspm/admin/reference-architectures/aws/reference-architecture-aws-lb.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                             https://docs.posit.co/rspm/admin/reference-architectures/aws/reference-architecture-aws-lb/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                              https://docs.posit.co/rspm/admin/reference-architectures/aws/reference-architecture-aws-single-server.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                  https://docs.posit.co/rspm/admin/reference-architectures/aws/reference-architecture-aws-single-server/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                    https://docs.posit.co/rspm/admin/reference-architectures/azure/reference-architecture-azure-k8s.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                        https://docs.posit.co/rspm/admin/reference-architectures/azure/reference-architecture-azure-k8s/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                     https://docs.posit.co/rspm/admin/reference-architectures/azure/reference-architecture-azure-lb.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                         https://docs.posit.co/rspm/admin/reference-architectures/azure/reference-architecture-azure-lb/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                          https://docs.posit.co/rspm/admin/reference-architectures/azure/reference-architecture-azure-single-server.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                              https://docs.posit.co/rspm/admin/reference-architectures/azure/reference-architecture-azure-single-server/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                       https://docs.posit.co/rspm/admin/reference-architectures/faq.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                     https://docs.posit.co/rspm/admin/reference-architectures/generic-architectures.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                         https://docs.posit.co/rspm/admin/reference-architectures/generic-architectures/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                  https://docs.posit.co/rspm/admin/reference-architectures/overview.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                      https://docs.posit.co/rspm/admin/reference-architectures/overview/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                 https://docs.posit.co/rspm/admin/remote-publishing.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                      https://docs.posit.co/rspm/admin/repositories.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                        https://docs.posit.co/rspm/admin/rstudio-server/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                  https://docs.posit.co/rspm/admin/security/package.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                      https://docs.posit.co/rspm/admin/security/package/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                   https://docs.posit.co/rspm/admin/security/server.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                       https://docs.posit.co/rspm/admin/security/server/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                 https://docs.posit.co/rspm/admin/server-management.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                  https://docs.posit.co/rspm/admin/serving-binaries.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                           https://docs.posit.co/rspm/admin/swagger.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                         https://docs.posit.co/rspm/admin/upgrading.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                             https://docs.posit.co/rspm/admin/upgrading/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                           https://docs.posit.co/rspm/admin/vsx-packaging/local-vsx.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                             https://docs.posit.co/rspm/admin/vsx-packaging/openvsx.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                         https://docs.posit.co/rspm/admin/workbench.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                                   https://docs.posit.co/rspm/index.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                                    https://docs.posit.co/rspm/licenses/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                          https://docs.posit.co/rspm/licenses/index.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                                https://docs.posit.co/rspm/news-offline/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                                        https://docs.posit.co/rspm/news/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                           https://docs.posit.co/rspm/news/offline-downloader/index.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                        https://docs.posit.co/rspm/news/package-manager/ 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                              https://docs.posit.co/rspm/news/package-manager/index.html 
#>                                                                                                               "2026-10-14 20:13:17 UTC" 
#>                                                                                                        https://docs.posit.co/rspm/user/ 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                                     https://docs.posit.co/rspm/user/authentication.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                                   https://docs.posit.co/rspm/user/configure-python.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                                        https://docs.posit.co/rspm/user/configure-r.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                                      https://docs.posit.co/rspm/user/configure-vsx.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                                       https://docs.posit.co/rspm/user/get-repo-url.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                                    https://docs.posit.co/rspm/user/getting-started.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                                              https://docs.posit.co/rspm/user/index.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                  https://docs.posit.co/rspm/user/packages/customizing-package-tabs.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                     https://docs.posit.co/rspm/user/packages/package-changelog-tab.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                      https://docs.posit.co/rspm/user/packages/package-overview-tab.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                https://docs.posit.co/rspm/user/packages/reviewing-package-activity.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                    https://docs.posit.co/rspm/user/packages/searching-for-packages.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                    https://docs.posit.co/rspm/user/packages/viewing-other-versions.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                           https://docs.posit.co/rspm/user/packages/viewing-package-version-history.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                           https://docs.posit.co/rspm/user/packages/viewing-package.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                                            https://docs.posit.co/rspm/user/plugins.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                                  https://docs.posit.co/rspm/user/r-binary-packages.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                                              https://docs.posit.co/rspm/user/setup.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                                          https://docs.posit.co/rspm/user/whats-new.html 
#>                                                                                                               "2026-10-14 20:13:18 UTC" 
#>                                                                                                     https://docs.posit.co/shiny-server/ 
#>                                                                                                               "2026-05-28 23:55:50 UTC" 
#>                                                                                                     https://docs.posit.co/shinyapps.io/ 
#>                                                                                                               "2026-09-18 22:29:51 UTC" 
#>                                                                                                      https://docs.posit.co/support.html 
#>                                                                                                               "2026-10-15 18:37:18 UTC" 
#>                                                                                                          https://docs.posit.co/support/ 
#>                                                                                                               "2026-10-15 18:37:18 UTC" 
#>                                                                                               https://docs.posit.co/supported-versions/ 
#>                                                                                                               "2026-10-15 18:37:18 UTC" 
#>                                                                                   https://docs.posit.co/supported-versions/connect.html 
#>                                                                                                               "2026-10-15 18:37:18 UTC" 
#>                                                                                     https://docs.posit.co/supported-versions/index.html 
#>                                                                                                               "2026-10-15 18:37:18 UTC" 
#>                                                                           https://docs.posit.co/supported-versions/package-manager.html 
#>                                                                                                               "2026-10-15 18:37:18 UTC" 
#>                                                                                   https://docs.posit.co/supported-versions/rstudio.html 
#>                                                                                                               "2026-10-15 18:37:18 UTC" 
#>                                                                                                  https://docs.posit.co/troubleshooting/ 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                                                      https://docs.posit.co/troubleshooting/articles/accessing-pins.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                https://docs.posit.co/troubleshooting/articles/automatically-remove-zombie-processes-from-workbench.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                              https://docs.posit.co/troubleshooting/articles/backtracing-r-code-for-troubleshooting.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                                                 https://docs.posit.co/troubleshooting/articles/c-stack-usage-limit.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                                                https://docs.posit.co/troubleshooting/articles/capturing-a-har-file.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#> https://docs.posit.co/troubleshooting/articles/configure-posit-workbench-saml-authentication-to-support-case-insensitive-usernames.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                                          https://docs.posit.co/troubleshooting/articles/configuring-local-launcher.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                                 https://docs.posit.co/troubleshooting/articles/connect-in-kubernetes-restart-issue.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                                    https://docs.posit.co/troubleshooting/articles/connection-refused-error-message.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                                    https://docs.posit.co/troubleshooting/articles/database-is-locked-error-message.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                     https://docs.posit.co/troubleshooting/articles/deploying-nfs-for-high-availability-deployments.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                           https://docs.posit.co/troubleshooting/articles/downloading-packages-from-package-manager.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                                          https://docs.posit.co/troubleshooting/articles/error-reading-launcher-key.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>            https://docs.posit.co/troubleshooting/articles/error-when-deploying-from-posit-workbench-to-posit-connect-using-aws-nlb.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                           https://docs.posit.co/troubleshooting/articles/how-to-export-a-network-request-log-from-the-r-studio-ide.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                       https://docs.posit.co/troubleshooting/articles/how-to-mount-a-windows-smb-cifs-share-via-pam.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                    https://docs.posit.co/troubleshooting/articles/how-to-upload-files-larger-than-20-mb-to-support.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                        https://docs.posit.co/troubleshooting/articles/images-not-displaying-in-published-shiny-app.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                                             https://docs.posit.co/troubleshooting/articles/javascript-console-logs.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                      https://docs.posit.co/troubleshooting/articles/kubernetes-sessions-in-posit-workbench-stdin-errors-within-pod.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                     https://docs.posit.co/troubleshooting/articles/list-of-outbound-or-egress-network-destinations.html 
#>                                                                                                               "2026-10-15 18:29:16 UTC" 
#>                                    https://docs.posit.co/troubleshooting/articles/long-running-jupyter-sessions-on-posit-workbench.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                       https://docs.posit.co/troubleshooting/articles/metadata-is-missing-required-fields-errors-in-package-manager.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                             https://docs.posit.co/troubleshooting/articles/migrate-to-the-latest-marketplace-image.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                                             https://docs.posit.co/troubleshooting/articles/operation-not-permitted.html 
#>                                                                                                               "2026-09-03 19:14:17 UTC" 
#>                                     https://docs.posit.co/troubleshooting/articles/package-manager-wont-start-temp-directory-error.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                              https://docs.posit.co/troubleshooting/articles/pins-incompatible-with-connect-2024-05.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                        https://docs.posit.co/troubleshooting/articles/pprof-diagnostic-reports-for-package-manager.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                                https://docs.posit.co/troubleshooting/articles/publishing-from-workbench-to-connect.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                                    https://docs.posit.co/troubleshooting/articles/resolve-python-issue-in-positron.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                                https://docs.posit.co/troubleshooting/articles/resolving-conflicting-license-status.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                  https://docs.posit.co/troubleshooting/articles/resolving-connection-issues-in-workbench-due-to-http-proxy-changes.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>            https://docs.posit.co/troubleshooting/articles/resolving-content-errors-in-posit-connect-behind-f5-with-ssl-termination.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                     https://docs.posit.co/troubleshooting/articles/resolving-jupyter-lab-connection-issues-after-workbench-upgrade.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>            https://docs.posit.co/troubleshooting/articles/resolving-license-file-access-issues-due-to-parent-directory-permissions.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                                https://docs.posit.co/troubleshooting/articles/rotate-key-files-in-workbench-images.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                   https://docs.posit.co/troubleshooting/articles/run-shiny-applications-via-the-linux-command-line.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>             https://docs.posit.co/troubleshooting/articles/saml-protocol-response-cannot-be-sent-via-bindings-other-than-http-post.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                              https://docs.posit.co/troubleshooting/articles/sanitize-sensitive-data-in-diagnostics.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                           https://docs.posit.co/troubleshooting/articles/scheduled-reports-are-not-running-on-time.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                             https://docs.posit.co/troubleshooting/articles/session-heartbeat-failure-apple-blossom.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                               https://docs.posit.co/troubleshooting/articles/ssl-verification-errors-installing-vs-code-extensions.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                                         https://docs.posit.co/troubleshooting/articles/support-ticket-for-new-user.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                https://docs.posit.co/troubleshooting/articles/tax-exempt-accounts-for-shinyapps-io-and-posit-cloud.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>        https://docs.posit.co/troubleshooting/articles/the-terminal-in-the-r-studio-ide-is-missing-characters-or-behaving-strangely.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                              https://docs.posit.co/troubleshooting/articles/the-workbench-jobs-feature-is-not-configured-correctly.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                                                 https://docs.posit.co/troubleshooting/articles/too-many-open-files.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                       https://docs.posit.co/troubleshooting/articles/troubleshooting-license-key-activation-errors.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                          https://docs.posit.co/troubleshooting/articles/troubleshooting-postgres-connection-issues.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                         https://docs.posit.co/troubleshooting/articles/troubleshooting-quarto-memory-limit-errors-in-posit-connect.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                      https://docs.posit.co/troubleshooting/articles/troubleshooting-r-studio-ide-connection-errors.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                           https://docs.posit.co/troubleshooting/articles/troubleshooting-r-studio-rendering-errors.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                                  https://docs.posit.co/troubleshooting/articles/understanding-setup-chunk-behavior.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>         https://docs.posit.co/troubleshooting/articles/user-guide-for-troubleshooting-pam-authentication-issues-in-posit-workbench.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                                              https://docs.posit.co/troubleshooting/articles/using-r-studio-server-in-windows-wsl-2.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
#>                https://docs.posit.co/troubleshooting/articles/validating-the-launcher-sessions-callback-address-in-posit-workbench.html 
#>                                                                                                               "2026-09-03 19:14:18 UTC" 
# }
```
