# ragnar: RAG Tools for R

## Overview

`ragnar` is an R package that helps implement Retrieval-Augmented Generation (RAG) workflows. It focuses on providing a complete solution with sensible defaults, while still giving the knowledgeable user precise control over all the steps. We don't believe that you can fully automate the creation of a good RAG, so it's important that ragnar is not a black box; you need to be able understand what it's doing by inspecting the output from intermediate steps.

## Key steps

### 1. Document Processing

We assume the user provides a directory of markdown or HTML files. In the long term, we should provide tools to help the user create this directory, e.g. with [web scraping](https://github.com/r-lib/httr2/pull/584) or from markdown. We should also eventually provide tools to help simplify the HTML using (e.g) readibilty to find the document contents or a pandoc AST walker to strip extraneous attributes.

### 2. Text Chunking

Next we need to divide each document into multiple chunks. I think we'll want to default to something that preserves some of the semantics of the document (especially with headers), although this is possibly not as important if we expect the user to do context augmentation (up next).

Strategies:

- Token-based splitting: Uses tokenization to create chunks with consistent token counts, better for LLM context windows
- Paragraph splitting: Maintains paragraph boundaries for better semantic coherence, with variable chunk sizes
- HTML-aware splitting: Respects HTML structure when splitting, preserving hierarchical relationships and semantic meaning
- Recursive splitting: Starts with large chunks and recursively splits on different delimiters (e.g., "\n\n", "\n", ".", " "), preserving semantic structure where possible

Will need some way to control chunk length and overlap. There's plenty of python code that already does this that we can copy

### 3. (Optionally) Context augmentation

Idea is from [Introducing Contextual Retrieval](https://www.anthropic.com/news/contextual-retrieval) research. Add needed context to the chunk with a prompt like this:

```
<document>
{{WHOLE_DOCUMENT}}
</document>
Here is the chunk we want to situate within the whole document
<chunk>
{{CHUNK_CONTENT}}
</chunk>
Please give a short succinct context to situate this chunk within the overall document for the purposes of improving search retrieval of the chunk. Answer only with the succinct context and nothing else.
```

(Would probably want to put the whole document in a separate message or system prompt so we can use caching more effectively.)

### 4. Embedding

Next need to compute an embedding for each chunk. Goal is for ellmer to provide access to embeddings from popular LLM providers.

Maybe it would be nice to have a self-contained implementation in an R package, but I suspect that this will be expensive enough that we'll only want to do later (but I might be wrong).

### 5. Storage

Now we need to store all this info in a way that's easy to search. I think the pragmatic solution here is to use duckdb.

```sql
-- Chunks table
CREATE TABLE chunks (
    chunk_id INTEGER PRIMARY KEY,
    file_path TEXT,
    file_hash TEXT,
    chunk_index INTEGER,
    content TEXT,
    embedding INTEGER[128],
    token_count INTEGER,
);
```

### 6. Retrieval

Finally need to provide a method to retrieve related chunks based on cosine similarity of embeddings and BM25 ranking and the user query. Need to specify either number of results or (better?) some distance threshold.

* https://duckdb.org/docs/stable/core_extensions/vss: index for vector similarity search
* https://duckdb.org/docs/stable/core_extensions/full_text_search

(Do we also need tools that summarise the conversation history to add more context to the user query?)

### 7. (Optional) Re-ranking

Seems like common method is to return a large number of chunks, since it's a cheap operation, and then use a specialised re-ranking to pick the (say) top 20. (e.g. <https://docs.voyageai.com/reference/reranker-api>)

### 8. Prompt generation

Put the selected chunks into the prompt in some way that the LLM likes/can understand.

Do best matching chunks go at top or bottom of the list? Where do you put the original prompt?

May also need tools for state management of chat history - what do you do with RAG context after its been used? Need to do some research on what other people do.

### Usage

```r
library(ragnar)

ragnar_update(
  db = "myproject.duckdb",
  documents = ragnar_read_html("path/to/docs"),
  chunk = split_semantic(chunk_size = 500, chunk_overlap = 50),
  embedding = embed_openai(model = "text-embedding-3-small"),
)

# Nice to have some iterator API especially for experimenting
# giving you the ability to understand what's actually going on.
for(chunk in ragnar_chunk(documents)) {
  ...
}

chunks <- ragnar_retrieve(
  db = "myproject.duckdb",
  query = "foo")

chunks <- ragnar_arrange(chunks, rerank_voyager(model = "rerank-2", top_k = 10))

chunks <- rangar_slice_max(chunks, n = 10, max_tokens = 2000)
# Need to provide some helper to inject into prompt?
```
