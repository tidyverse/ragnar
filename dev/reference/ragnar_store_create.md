# Create and connect to a vector store

Create and connect to a vector store

## Usage

``` r
ragnar_store_create(
  location = ":memory:",
  embed = embed_ollama(),
  ...,
  embedding_size = ncol(embed("foo")),
  overwrite = FALSE,
  extra_cols = NULL,
  name = NULL,
  title = NULL,
  version = 2
)

ragnar_store_connect(location, ..., read_only = TRUE)
```

## Arguments

- location:

  filepath, or `:memory:`. Location can also be a database name
  specified with `md:dbname`, in this case the database will be created
  in MotherDuck after a connection is established.

- embed:

  A function that is called with a character vector and returns a matrix
  of embeddings. Note this function will be serialized and then
  deserialized in new R sessions, so it cannot reference to any objects
  in the global or parent environments. Make sure to namespace all
  function calls with `::`. If additional R objects must be available in
  the function, you can optionally supply a `carrier::crate()` with
  packaged data. It can also be `NULL` for stores that don't need to
  embed their texts, for example, if only using FTS algorithms such as
  [`ragnar_retrieve_bm25()`](https://ragnar.tidyverse.org/dev/reference/ragnar_retrieve_bm25.md).

- ...:

  unused; must be empty.

- embedding_size:

  integer

- overwrite:

  logical, what to do if `location` already exists

- extra_cols:

  A zero row data frame used to specify additional columns that should
  be added to the store. Such columns can be used for adding additional
  context when retrieving. See the examples for more information.
  [`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html)
  is used to consistently perform type checks and casts when inserting
  with
  [`ragnar_store_insert()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_insert.md).

- name:

  A unique name for the store. Must match the `^[a-zA-Z0-9_-]+$` regex.
  Used by
  [`ragnar_register_tool_retrieve()`](https://ragnar.tidyverse.org/dev/reference/ragnar_register_tool_retrieve.md)
  for registering tools.

- title:

  A title for the store, used by
  [`ragnar_register_tool_retrieve()`](https://ragnar.tidyverse.org/dev/reference/ragnar_register_tool_retrieve.md)
  when the store is registered with an
  [ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html)
  object.

- version:

  integer. The version of the store to create. See details.

- read_only:

  logical, whether the returned connection can be used to modify the
  store.

## Value

a `RagnarStore` object

## Details

### Store versions

**Version 2 – documents with chunk ranges** (default)

With `version = 2`, ragnar stores each document once and records the
start and end positions of its chunks. This provides strong support for
overlapping chunk ranges with de-overlapping at retrieval, and generally
allows retrieving arbitrary ranges from source documents, but does not
support modifying chunks directly before insertion. Chunks can be
augmented via the `context` field and with additional fields passed to
`extra_cols`. The easiest way to prepare `chunks` for `version = 2` is
with
[`read_as_markdown()`](https://ragnar.tidyverse.org/dev/reference/read_as_markdown.md)
and
[`markdown_chunk()`](https://ragnar.tidyverse.org/dev/reference/markdown_chunk.md).

**Version 1 – flat chunks**

With `version = 1`, ragnar keeps all chunks in a single table. This lets
you easily modify chunk text before insertion. However, dynamic
rechunking (de-overlapping) or extracting arbitrary ranges from source
documents is not supported, since the original full documents are no
longer available. Chunks can be augmented by modifying the chunk text
directly (e.g., with `glue()`). Additionally, if you intend to call
[`ragnar_store_update()`](https://ragnar.tidyverse.org/dev/reference/ragnar_store_insert.md),
it is your responsibility to provide
`rlang::hash(original_full_document)` with each chunk. The easiest way
to prepare `chunks` for `version = 1` is with
[`ragnar_read()`](https://ragnar.tidyverse.org/dev/reference/ragnar_read.md)
and
[`ragnar_chunk()`](https://ragnar.tidyverse.org/dev/reference/ragnar_chunk.md).

## Examples

``` r
# A store with a dummy embedding
store <- ragnar_store_create(
  embed = \(x) matrix(stats::runif(10), nrow = length(x), ncol = 10),
  version = 1
)
ragnar_store_insert(store, data.frame(text = "hello"))

# A store with a schema. When inserting into this store, users need to
# provide an `area` column.
store <- ragnar_store_create(
  embed = \(x) matrix(stats::runif(10), nrow = length(x), ncol = 10),
  extra_cols = data.frame(area = character()),
  version = 1
)
ragnar_store_insert(store, data.frame(text = "hello", area = "rag"))

# If you already have a data.frame with chunks that will be inserted into
# the store, you can quickly create a suitable store with `vec_ptype()`:
chunks <- data.frame(text = letters, area = "rag")
store <- ragnar_store_create(
  embed = \(x) matrix(stats::runif(10), nrow = length(x), ncol = 10),
  extra_cols = vctrs::vec_ptype(chunks),
  version = 1
)
ragnar_store_insert(store, chunks)

# version = 2 (the default) has support for deoverlapping
store <- ragnar_store_create(
  # if embed = NULL, then only bm25 search is used (not vss)
  embed = NULL
)
doc <- MarkdownDocument(
  paste0(letters, collapse = ""),
  origin = "/some/where"
)
chunks <- markdown_chunk(doc, target_size = 3, target_overlap = 2 / 3)
chunks$context <- substring(chunks$text, 1, 1)
chunks
#> # @document@origin: /some/where
#> # A tibble:         24 × 4
#>    start   end context text 
#>    <int> <int> <chr>   <chr>
#>  1     1     3 a       abc  
#>  2     2     4 b       bcd  
#>  3     3     5 c       cde  
#>  4     4     6 d       def  
#>  5     5     7 e       efg  
#>  6     6     8 f       fgh  
#>  7     7     9 g       ghi  
#>  8     8    10 h       hij  
#>  9     9    11 i       ijk  
#> 10    10    12 j       jkl  
#> # ℹ 14 more rows
ragnar_store_insert(store, chunks)
ragnar_store_build_index(store)

ragnar_retrieve(store, "abc bcd xyz", deoverlap = FALSE)
#> # A tibble: 3 × 8
#>   origin      doc_id chunk_id start   end  bm25 context text 
#>   <chr>        <int>    <int> <int> <int> <dbl> <chr>   <chr>
#> 1 /some/where      1        1     1     3  1.22 a       abc  
#> 2 /some/where      1        2     2     4  1.22 b       bcd  
#> 3 /some/where      1       24    24    26  1.22 x       xyz  
ragnar_retrieve(store, "abc bcd xyz", deoverlap = TRUE)
#> # A tibble: 2 × 8
#>   origin      doc_id chunk_id  start   end bm25      context text 
#>   <chr>        <int> <list>    <int> <int> <list>    <chr>   <chr>
#> 1 /some/where      1 <int [2]>     1     4 <dbl [2]> a       abcd 
#> 2 /some/where      1 <int [1]>    24    26 <dbl [1]> x       xyz  
```
