# ragnar: Retrieval-Augmented Generation (RAG) Workflows

Provides tools for implementing Retrieval-Augmented Generation (RAG)
workflows with Large Language Models (LLM). Includes functions for
document processing, text chunking, embedding generation, storage
management, and content retrieval. Supports various document types and
embedding providers ('Ollama', 'OpenAI'), with 'DuckDB' as the default
storage backend. Integrates with the 'ellmer' package to equip chat
objects with retrieval capabilities. Designed to offer both sensible
defaults and customization options with transparent access to
intermediate outputs. For a review of retrieval-augmented generation
methods, see Gao et al. (2023) "Retrieval-Augmented Generation for Large
Language Models: A Survey"
[doi:10.48550/arXiv.2312.10997](https://doi.org/10.48550/arXiv.2312.10997)
.

## See also

Useful links:

- <https://ragnar.tidyverse.org/>

- <https://github.com/tidyverse/ragnar>

- Report bugs at <https://github.com/tidyverse/ragnar/issues>

## Author

**Maintainer**: Tomasz Kalinowski <tomasz@posit.co>

Authors:

- Daniel Falbel <daniel@posit.co>

Other contributors:

- Posit Software, PBC ([ROR](https://ror.org/03wc8by49)) \[copyright
  holder, funder\]
