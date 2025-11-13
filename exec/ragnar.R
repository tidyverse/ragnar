#!/usr/bin/env Rapp
#| name: ragnar
#| title: Ragnar CLI
#| description: Build, update, and serve Ragnar knowledge stores.

Sys.setenv("UV_NO_PROGRESS" = "1")
options(
  # warn = 2,
  cli.progress_clear = FALSE,
  cli.progress_show_after = 0
)

attach(asNamespace("ragnar"), name = "namespace:ragnar", warn.conflicts = FALSE)

#| description: Path to the Ragnar store.
#| short: s
store <- ".ragnar.store"


switch(
  "help",

  #| title: Create a project store with files in the current working directory
  init = {
    #| title: Whether to overwrite an existing ragnar store.
    overwrite <- FALSE

    #| description: Name of the store
    name <- NA_character_

    #| description: Title of the store
    title <- NA_character_

    #| description: The store description
    description <- "the knowledge store"

    if (is.na(name)) {
      name <- sprintf("%s_knowledge_store", basename(getwd()))
      name <- gsub("[^[:alnum:]]+", "_", name)
    }

    if (is.na(title)) {
      title <- NULL
    }

    store <- ragnar_store_create(
      store,
      # embed = \(x) embed_ollama(x),
      embed = \(x) embed_openai(x),
      overwrite = overwrite,
      name = name,
      title = title
    )
    message("Created: ", store@location)

    #| description: files to ingest
    globs... <- c()

    #| short: -e
    #| description: |
    #|   If supplied, no files are ingested. By default, all text
    #|   files in the current working directory are indexed.
    empty <- FALSE

    if (!empty) {
      files <- resolve_project_files_to_ingest(globs...)
      ragnar_store_ingest(store, files)
      ragnar_store_build_index(store)
    }
  },

  #| title: Add content to a ragnar store from outside the current working directory.
  add = {
    #| description: pattern to filter files
    globs... <- c()

    #| short: 'n'
    n_workers <- NA_integer_
    if (is.na(n_workers)) {
      n_workers <- NULL
    }

    store <- ragnar_store_connect(store, read_only = FALSE)
    files <- resolve_files_to_ingest(globs...)
    ragnar_store_ingest(store, files, n_workers = n_workers)
  },

  #| title: Referesh the store contents.
  update = {
    store <- ragnar_store_connect(store, read_only = FALSE)
    docs <- DBI::dbGetQuery(store@con, "SELECT origin FROM documents")
    # TODO: we should (re)walk the project dir here to find new files.
    ragnar_store_ingest(store, docs$origin)
  },

  #| title: Serve a store retrieve tool over an stdio mcp server.
  mcp_serve = {
    store <- ragnar_store_connect(store)
    mcp_serve_store(store)
  },

  #| title: Build the store index.
  build_index = {
    store <- ragnar_store_connect(store, read_only = FALSE)
    ragnar::ragnar_store_build_index(store)
  },

  #| title: Print help.
  help = {
    # TODO: figure out a more elegant way to trigger emmission of default Rapp help.
    # maybe always add a 'help' command by default?
    app <- system.file("exec/ragnar.R", package = "ragnar")
    Rapp::run(app, "--help")
  }
)
