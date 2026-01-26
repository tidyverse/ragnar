# Serve a Ragnar store over MCP

Launches an MCP server (via
[`mcptools::mcp_server()`](https://posit-dev.github.io/mcptools/reference/server.html))
that exposes a retrieval tool backed by a Ragnar store. This lets
MCP-enabled clients (e.g., Codex CLI, Claude Code) call into your store
to retrieve relevant excerpts.

## Usage

``` r
mcp_serve_store(
  store,
  store_description = "the knowledge store",
  ...,
  name = NULL,
  title = NULL,
  extra_tools = NULL
)
```

## Arguments

- store:

  A `RagnarStore` object or a file path to a Ragnar DuckDB store. If a
  character path is supplied, it is opened with
  [`ragnar_store_connect()`](https://ragnar.tidyverse.org/reference/ragnar_store_create.md).

- store_description:

  Optional string used in the tool description presented to clients.

- ...:

  arguments passed on to
  [`ragnar_retrieve()`](https://ragnar.tidyverse.org/reference/ragnar_retrieve.md).

- name, title:

  Optional tool function name and title. By default, `store@name` and
  `store@title` will be used if present. The tool `name` must be a valid
  R function name and should be unique with the tools registered with
  the [ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html)
  object. `title` is used for user-friendly display.

- extra_tools:

  Optional additional tools (list of
  [`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html)
  objects) to serve alongside the retrieval tool.

## Value

This function blocks the current R process by running an MCP server. It
is intended for non-interactive use. Called primarily for side-effects.

## Details

To use this function with [Codex
CLI](https://developers.openai.com/codex/cli/), add something like this
to `~/.codex/config.toml`

    [mcp_servers.quartohelp]
    command = "Rscript"
    args = [
      "-e",
      "ragnar::mcp_serve_store('/path/to/ragnar.store', top_k=10)"
    ]

You can confirm the agent can search the ragnar store by inspecting the
output from the `/mcp` command, or by asking it "What tools do you have
available?".
