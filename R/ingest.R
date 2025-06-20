get_markdown_source_positions <- function(md, type = NULL) {
  lines <- md |> stri_split_lines() |> unlist() |> stri_trim_right()
  text <- lines |> stri_flatten("\n")

  if (text == "") {
    return(data_frame(tag = character(), start = integer(), end = integer()))
  }

  doc <- text |>
    commonmark::markdown_xml(
      sourcepos = TRUE,
      extensions = TRUE,
      footnotes = TRUE,
      normalize = TRUE
    ) |>
    enc2utf8() |>
    charToRaw() |>
    xml2::read_xml(encoding = "UTF-8")

  elements <- doc |> xml_find_all(xpath = "//*[@sourcepos]")

  df <- tibble::tibble(
    type = elements |> xml_name(),
    sourcepos = elements |> xml_attr("sourcepos")
  )

  if (length(type)) {
    df <- df[df$type %in% unique(c(type)), ]
  }

  # commonmark returns positions as line:byte-line:byte
  # e.g., 52:1-52:20
  pos <- df$sourcepos |>
    stri_split_charclass("[-:]", n = 4L, simplify = TRUE)
  storage.mode(pos) <- "integer"
  colnames(pos) <- c(
    "start_line",
    "start_line_byte",
    "end_line",
    "end_line_byte"
  )

  # lines |>
  #   stri_split_boundaries(type = "character") |>
  #   lapply(\(x) cumsum(stri_numbytes(x)))

  line_numbytes <- stri_numbytes(lines) + 1L # +1 for \n
  line_startbyte <- c(1L, 1L + drop_last(cumsum(line_numbytes)))

  # df$sourcepos <- pos
  start <- line_startbyte[pos[, "start_line"]] + pos[, "start_line_byte"] - 1L
  end <- line_startbyte[pos[, "end_line"]] + pos[, "end_line_byte"] - 1L

  # convert byte index to a char index
  char_byte_starts <-
    stri_split_boundaries(text, type = "character")[[1L]] |>
    stri_numbytes() |>
    cumsum()
  # char_byte_starts <- c(1L, char_byte_starts)

  df$start <- match(start, char_byte_starts) #- 1L
  df$end <- match(end, char_byte_starts) #- 1L

  # df |> filter(type == "heading") |> mutate(text = stri_sub(md, start, end))

  df
}

# md_positions <- get_markdown_source_positions(
#   md,
#   type = c("heading", "paragraph")
# )
#
#
# cbind(
#   md |> stri_split_lines1() |> stri_subset_regex("^#+ "),
#   md_positions |>
#     filter(type == "heading") |>
#     (\(p) stri_sub(md, p$start, p$end))()
# )

#' @importFrom vctrs vec_ptype
NULL

str_locate_boundary_starts1 <- function(string, type) {
  check_string(string)
  stri_locate_all_boundaries(
    string,
    type = type,
    locale = "@ss=standard"
  )[[1L]][, "start"]
}

ragnar_chunk_v2 <- function(
  md,
  target_size = 1600,
  target_overlap = .5,
  max_snap_dist = target_size / 8,
  pre_segment = NULL
) {
  md_len <- stri_length(md)

  md_positions <-
    get_markdown_source_positions(md, type = c("heading", "paragraph"))

  candidate_boundries <-
    bind_rows(
      md_positions |> select(type, position = start),
      tibble(
        type = "sentence",
        position = str_locate_boundary_starts1(md, "sentence")
      ),
      tibble(
        type = "line",
        position = stri_locate_all_fixed(md, "\n")[[1L]][, "start"]
      ),
      tibble(
        type = "word",
        position = str_locate_boundary_starts1(md, "line_break")
      )
    ) |>
    mutate(
      priority = match(
        type,
        c("heading", "paragraph", "sentence", "line", "word")
      )
    )

  chunk_targets <- tibble(
    start = (
      # 1L,
      unique(sort.int(as.integer(c(
        round(seq.int(
          from = 1,
          to = max(1, md_len - target_size + 1L),
          by = target_size * (1L - target_overlap)
        )),
        max(1, md_len - target_size + 1L)
      ))))
    ),
    # exclusive end (python style)
    end = pmin(md_len + 1L, start + target_size)
  )

  boundary_targets <- tibble(
    target = unique(sort(unlist(chunk_targets, use.names = FALSE)))
  ) |>
    mutate(
      snap_candidate_min = if_else(
        target == md_len + 1L,
        md_len + 1L,
        pmax(1L, target - max_snap_dist)
      ),
      snap_candidate_max = if_else(
        target == 1,
        1L,
        pmin(md_len, target + max_snap_dist)
      )
    )

  candidates <- candidate_boundries |>
    rename(candidate_position = position)

  snap_picks <- left_join(
    boundary_targets,
    candidates,
    join_by(
      snap_candidate_min <= candidate_position,
      snap_candidate_max >= candidate_position
    )
  ) |>
    mutate(snap_dist = abs(target - candidate_position)) |>
    slice_min(tibble(priority, snap_dist), by = target) |>
    select(target, candidate_position) |>
    mutate(candidate_position = coalesce(candidate_position, target))

  stopifnot(!anyNA(snap_picks))

  chunks <- chunk_targets |>
    mutate(
      across(c(start, end), \(pos) {
        snap_picks$candidate_position[match(pos, snap_picks$target)]
      }),
      end = end - 1L # convert to inclusive end
    ) |>
    # remove chunks that may have snapped to the same position
    arrange(start) |>
    slice_max(end, by = start) |>
    slice_min(start, by = end)

  # still need to handle NAs, backfills for missing segments
  if (anyNA(chunks)) {
    stop("NA encountered")
  }

  delta <- integer(md_len + 1L)
  add(delta[chunks$start]) <- 1L
  add(delta[chunks$end + 1]) <- -1L
  coverage <- cumsum(drop_last(delta))
  if (!isFALSE(any(coverage == 0))) {
    browser()
    rle(coverage)
    stop("uncovered chunks")
  }

  chunks <- chunks |> arrange(start)

  headings <- md_positions |>
    filter(type == "heading") |>
    mutate(
      heading_char_start_idx = start,
      heading_char_end_idx = end,
      heading_text = stri_sub(md, start, end),
      heading_level = stri_locate_first_regex(heading_text, "[^#]")[, "start"] -
        1L
    ) |>
    identity()

  chunks2 <-
    chunks |>
    mutate(
      chunk_idx = row_number(),
      chunk_char_start_idx = start,
      chunk_char_end_idx = end,
      chunk_text = stri_sub(md, start, end),
      .keep = 'none'
    ) |>
    tidyr::crossing(heading_level = 1:6)

  out <- left_join(
    chunks2,
    headings,
    join_by(
      heading_level,
      # TODO: for pathological inputs with an extremely long heading or
      # max_snap_dist==0, figure out what to do if the chunk starts in the
      # middle of a heading
      closest(chunk_char_start_idx > heading_char_start_idx)
    )
  ) |>
    select(starts_with("chunk_"), starts_with("heading_")) |>
    arrange(chunk_idx, heading_level) |>
    tidyr::nest(heading = starts_with("heading_"), .names_sep = "_") |>
    mutate(heading = lapply(heading, tidyr::drop_na)) |>
    rename(headings = heading) |>
    rename_with(\(nm) stri_replace_first_regex(nm, "^chunk_", ""))

  # out$headings[1:5]

  out

  # # |>
  # pull(heading) |>
  #   _[1:5]
  #
  # tidyr::drop_na(heading_start) |>
  #
  #   out |>
  #   # -> z
  #
  #   z$headings[1:10]
}


# Fast nearest-boundary search (adapted from findInterval.c)
# ------------------------------------------------------------------
# * target      : arbitrary order, integer vector of positions to snap
# * candidates  : sorted, unique integer positions of valid boundaries
# * max_snap_dist : maximum distance within which snapping may occur
#
# Returns an integer vector `snap` with one element per `target`.
snap_candidates_q <- quickr::quick(
  name = "snap_candidates_q",
  function(target, candidates, max_snap_dist) {
    declare({
      type(target = integer(nt))
      type(candidates = integer(nc))
      type(max_snap_dist = integer(1))
      type(snap = integer(nt))
    })

    nt <- length(target)
    nc <- length(candidates)
    snap <- integer(nt)

    ilo <- 1L # index of last hit (initially 1)

    for (i in 1:nt) {
      x <- target[i]

      ## 1. Exponential bracketing --------------------------------------------
      if (x < candidates[ilo]) {
        step <- 1L
        repeat {
          ihi <- ilo
          ilo <- ilo - step
          if (ilo <= 1L) {
            ilo <- 1L
            break
          }
          if (x >= candidates[ilo]) {
            break
          }
          step <- step * 2L
        }
      } else {
        step <- 1L
        ihi <- min(ilo + 1L, nc)
        while (ihi < nc && x >= candidates[ihi]) {
          ilo <- ihi
          step <- step * 2L
          ihi <- min(ilo + step, nc)
        }
      }

      ## 2. Binary search inside [ilo, ihi] -----------------------------------
      while (ihi - ilo > 1L) {
        mid <- (ilo + ihi) %/% 2L
        if (x < candidates[mid]) ihi <- mid else ilo <- mid
      }

      ## 3. Pick nearer candidate within max_snap_dist ------------------------
      left <- candidates[ilo]
      if (ilo < nc) {
        right <- candidates[ilo + 1L]
      } else {
        right <- candidates[nc]
      }
      ld <- abs(x - left)
      rd <- abs(x - right)

      if (ld <= max_snap_dist || rd <= max_snap_dist) {
        if (rd < ld) {
          snap[i] <- right
        } else {
          snap[i] <- left
        }
      } else {
        snap[i] <- x # keep original position
      }
    }

    snap
  }
)


nearest_within <- function(target, pos, max_dist) {
  pos <- sort(unique(pos))
  idx <- findInterval(
    target,
    pos,
    rightmost.closed = FALSE,
    all.inside = TRUE,
    checkSorted = FALSE
  )
  left <- pos[idx]
  right <- pos[pmin(idx + 1L, length(pos))]
  left_d <- abs(target - left)
  right_d <- abs(target - right)
  pick <- ifelse(right_d < left_d, right, left)
  pick[(left_d > max_dist) & (right_d > max_dist)] <- NA_integer_
  pick
}


nearest_within(seq(0, 100, by = 10), pos = c(23, 99), 4)

# get_headings <- function(md, positions, headings) {}

check_store_overwrite <- function(location, overwrite) {
  check_bool(overwrite)
  if (location == ":memory:") {
    return()
  }

  paths <- c(location, location.wal <- paste0(location, ".wal"))
  if (any(file.exists(paths))) {
    if (overwrite) {
      unlink(c(location, location.wal), force = TRUE)
    } else {
      stop("File already exists: ", location)
    }
  }
}

ragnar_store_create_v2 <- function(
  location = ":memory:",
  embed = embed_ollama(model = "snowflake-arctic-embed2:568m"),
  embedding_size = ncol(embed("foo")),
  overwrite = FALSE,
  ...,
  extra_cols = NULL,
  name = NULL,
  title = NULL
) {
  # browser()
  rlang::check_dots_empty()

  check_string(location)
  check_store_overwrite(location, overwrite)
  name <- name %||% unique_store_name()
  check_string(name)
  stopifnot(grepl("^[a-zA-Z0-9_-]+$", name))
  check_string(title, allow_null = TRUE)

  conn <- dbConnect(duckdb::duckdb(), dbdir = location, array = "matrix")

  if (is.null(embed)) {
    embedding_size <- NULL
  } else {
    check_number_whole(embedding_size, min = 0)
    embedding_size <- as.integer(embedding_size)

    if (!inherits(embed, "crate")) {
      environment(embed) <- baseenv()
      embed <- rlang::zap_srcref(embed)
    }
  }

  metadata <- data_frame(
    embedding_size = embedding_size,
    embed_func = blob::blob(serialize(embed, NULL)),
    name = name,
    title = title
  )
  dbWriteTable(conn, "metadata", metadata)

  # read back in embed, so any problems with an R function that doesn't
  # serialize correctly flush out early.
  metadata <- dbReadTable(conn, "metadata")
  embed <- unserialize(metadata$embed_func[[1L]])
  name <- metadata$name

  # attach function to externalptr, so we can retrieve it from just the connection.
  ptr <- conn@conn_ref
  attr(ptr, "embed_function") <- embed

  dbExecute(
    conn,
    glue(
      r"--(
      DROP VIEW IF EXISTS chunks CASCADE;
      DROP TABLE IF EXISTS documents CASCADE;
      DROP TABLE IF EXISTS embeddings CASCADE;
      DROP SEQUENCE IF EXISTS chunk_id_seq CASCADE;
      DROP SEQUENCE IF EXISTS doc_id_seq CASCADE;
      )--"
    )
  )
  dbExecute(
    conn,
    glue(
      r"--(

      CREATE SEQUENCE doc_id_seq START 1;
      CREATE SEQUENCE chunk_id_seq START 1; -- need a unique id for fts

      CREATE OR REPLACE TABLE documents (
        doc_id INTEGER DEFAULT nextval('doc_id_seq'), -- PRIMARY KEY
        -- hash AS (hash(text)),
        origin VARCHAR,                               -- UNIQUE
        text VARCHAR,
      );

      CREATE OR REPLACE TABLE embeddings (
        doc_id INTEGER,
        chunk_id INTEGER DEFAULT nextval('chunk_id_seq'),
        doc_chunk_idx INTEGER,
        doc_char_start_idx INTEGER,
        doc_char_end_idx INTEGER,
        headings VARCHAR,
        embedding FLOAT[{embedding_size}]
      );

      CREATE OR REPLACE VIEW chunks AS (
        SELECT
          d.origin,
          e.*,
          d.text[ e.doc_char_start_idx : e.doc_char_end_idx ] as text
        FROM
          documents d
        JOIN
          embeddings e
        USING
          (doc_id)
      );
      )--"
    )
  )

  DuckDBRagnarStore(
    embed = embed,
    # schema = schema,
    .con = conn
    # name = name,
    # title = title
  )
}

ragnar_store_build_index_v2 <- function(store, type = c("vss", "fts")) {
  if (S7_inherits(store, DuckDBRagnarStore)) {
    conn <- store@.con
  } else if (methods::is(store, "DBIConnection")) {
    conn <- store
  } else {
    stop("`store` must be a RagnarStore")
  }

  if ("vss" %in% type && !is.null(store@embed)) {
    # TODO: duckdb has support for three different distance metrics that can be
    # selected when building the index: l2sq, cosine, and ip. Expose these as options
    # in the R interface. https://duckdb.org/docs/stable/core_extensions/vss#usage
    dbExecute(conn, "INSTALL vss; LOAD vss;")
    dbExecute(
      conn,
      r"--(
      SET hnsw_enable_experimental_persistence = true;

      DROP INDEX IF EXISTS store_hnsw_cosine_index;
      DROP INDEX IF EXISTS store_hnsw_l2sq_index;
      DROP INDEX IF EXISTS store_hnsw_ip_index;

      CREATE INDEX store_hnsw_cosine_index ON embeddings USING HNSW (embedding) WITH (metric = 'cosine');
      CREATE INDEX store_hnsw_l2sq_index   ON embeddings USING HNSW (embedding) WITH (metric = 'l2sq');
      CREATE INDEX store_hnsw_ip_index     ON embeddings USING HNSW (embedding) WITH (metric = 'ip');
      )--"
    )
  }

  if ("fts" %in% type) {
    dbExecute(conn, "INSTALL fts; LOAD fts;")
    # fts index builder takes many options, e.g., stemmer, stopwords, etc.
    # Expose a way to pass along args. https://duckdb.org/docs/stable/core_extensions/full_text_search
    dbWithTransaction(conn, {
      dbExecute(
        conn,
        r"--(
        ALTER VIEW chunks RENAME TO chunks_view;

        CREATE TABLE chunks AS
        SELECT chunk_id, headings, text
        FROM chunks_view;
        )--"
      )
      dbExecute(
        conn,
        r"--(
        PRAGMA create_fts_index(
          'chunks',            -- input_table
          'chunk_id',          -- input_id
          'headings', 'text',  -- *input_values
          overwrite = 1
        );
        )--"
      )
      dbExecute(
        conn,
        r"--(
        DROP TABLE chunks;
        ALTER VIEW chunks_view RENAME TO chunks
        )--"
      )
    })
  }
  # )--"
  # )
  # dbExecute(
  #   conn,
  #   r"--(

  invisible(store)
}

# ragnar_store_from <- function(paths)

ragnar_store_ingest <- function(store, paths, ...) {
  # TODO: check document hash and perform update
  conn <- store@.con
  on.exit({
    duckdb::duckdb_unregister(conn, "_ragnar_insert_temp_view")
  })
  # cli::cli_progress_bar("Cleaning data", total = length(paths))
  # for (origin in paths) {
  for (i in seq_along(paths)) {
    origin <- paths[i]
    message(sprintf("[% 3i/%i] ingesting: %s", i, length(paths), origin))
    md <- read_as_markdown(origin, ...)
    already_present <- dbGetQuery(
      conn,
      "SELECT hash(text) as hash, doc_id FROM documents WHERE origin = ?",
      list(origin)
    )
    documents <- data_frame(origin = origin, text = md)
    if (nrow(already_present)) {
      duckdb::duckdb_register(
        conn,
        "_ragnar_insert_temp_view",
        documents,
        overwrite = TRUE
      )
      to_insert <- dbGetQuery(
        conn,
        "SELECT hash(text) AS hash FROM _ragnar_insert_temp_view"
      )
      if (identical(to_insert$hash, already_present$hash)) {
        next
      } else {
        dbWithTransaction(conn, {
          doc_id <- already_present$doc_id
          dbExecute(
            conn,
            glue(
              # TODO: use UPDATE statement instead of DELETE ???
              "
              DELETE FROM documents WHERE doc_id = {doc_id};
              DELETE FROM embeddings WHERE doc_id = {doc_id};
              "
            )
          )
        })
        documents$doc_id <- doc_id
      }
    }
    chunks <- ragnar_chunk_v2(md)
    embeddings <- chunks |>
      store@embed() |>
      mutate(
        headings = map_chr(headings, \(x) paste0(x$text, collapse = "\n")),
      ) |>
      # relocate(headings, .before = text) |>
      select(
        doc_chunk_idx = idx,
        doc_char_start_idx = char_start_idx,
        doc_char_end_idx = char_end_idx,
        headings,
        embedding
      )

    DBI::dbWithTransaction(conn, {
      dbAppendTable(conn, "documents", documents)
      # can optionally hand-write INSERT ... RETURNING doc_id
      embeddings$doc_id <- documents$doc_id %||%
        as.integer(dbGetQuery(
          conn,
          "SELECT currval('doc_id_seq') AS doc_id"
        ))
      dbAppendTable(conn, "embeddings", embeddings)
    })
    # cli::cli_progress_update()
  }
  # cli::cli_progress_done()
  invisible(store)
}

ragnar_store_connect_v2 <- function(location, read_only = TRUE) {
  conn <- dbConnect(
    duckdb::duckdb(),
    dbdir = location,
    read_only = read_only,
    array = "matrix"
  )
  dbExecute(conn, "INSTALL fts; INSTALL vss;")
  dbExecute(conn, "LOAD fts; LOAD vss;")
  metadata <- dbReadTable(conn, "metadata")
  embed <- unserialize(metadata$embed_func[[1L]])
  # schema <- unserialize(metadata$schema[[1L]])
  name <- metadata$name
  title <- metadata$title

  ptr <- conn@conn_ref
  attr(ptr, "embed_function") <- embed

  DuckDBRagnarStore(
    embed = embed,
    # schema = schema,
    .con = conn,
    name = name,
    title = metadata$title
  )
}

ragnar_retrieve_vss_v2 <- function(
  store,
  query,
  top_k = 3L,
  method = "cosine_distance"
) {
  conn <- store@.con
  retrieve_df <- data_frame(
    query = query,
    embedding = store@embed(query)
  )
  tmp_name <- "_ragnar_retrieve_query"
  on.exit(duckdb::duckdb_unregister(conn, tmp_name))
  duckdb::duckdb_register(conn, tmp_name, retrieve_df, overwrite = TRUE)

  top_k <- as.integer(top_k)
  method <- "cosine_distance"
  .[method_func, order_key_direction] <- method_to_info(method)
  dbGetQuery(
    conn,
    glue(
      "
      SELECT
        *,
        '{method}' as metric_name,
        {method_func}(
           embedding,
           (SELECT embedding FROM _ragnar_retrieve_query)
        ) as metric_value
       FROM chunks
       ORDER BY metric_value {order_key_direction}
       LIMIT {top_k};
       "
    )
  )
}


ragnar_retrieve_bm25_v2 <- function(store, text, top_k = 3L) {
  conn <- store@.con
  dbGetQuery(
    conn,
    glue(
      "
      SELECT
        *,
        'bm25' as metric_name,
        fts_main_chunks.match_bm25( chunk_id, ?) as metric_value
      FROM chunks
      WHERE metric_value IS NOT NULL
      ORDER BY metric_value
      LIMIT {top_k};
      "
    ),
    params = list(text)
  )
}

ragnar_store_from <- function(
  sources,
  embed = embed_ollama(model = "snowflake-arctic-embed2:568m"),
  location = ":memory:",
  ...
) {
  sources <- unique(sources)
  store <- ragnar_store_create_v2(embed = embed, location = location)
  ragnar_store_ingest(store, sources, ...)
  ragnar_store_build_index_v2(store)
  if (location != ":memory:") {
    dbDisconnect(store@.con)
    store <- ragnar_store_connect_v2(location, read_only = TRUE)
  }
  store
}


ragnar_deoverlap <- chunks_deoverlap <- function(store, chunks) {
  deoverlapped <- chunks |>
    group_by(doc_id) |>
    arrange(doc_chunk_idx, .by_group = TRUE) |>
    mutate(
      overlap_grp = cumsum(
        doc_char_start_idx > lag(doc_char_end_idx, default = -1L)
      )
    ) |>
    group_by(doc_id, overlap_grp) |>
    summarise(
      origin = first(origin),
      chunk_ids = list(chunk_id),
      doc_chunk_idxs = list(doc_chunk_idx),
      doc_char_start_idx = min(doc_char_start_idx),
      doc_char_end_idx = max(doc_char_end_idx),
      headings = first(headings),
      .groups = "drop"
    ) |>
    mutate(tmp_chunk_id = row_number())

  tmp_ <- deoverlapped |>
    select(doc_id, doc_char_start_idx, doc_char_end_idx, tmp_chunk_id)

  conn <- store@.con
  tmp_name <- "_ragnar_tmp_rechunk"
  on.exit(duckdb::duckdb_unregister(conn, tmp_name))
  duckdb::duckdb_register(conn, tmp_name, tmp_, overwrite = TRUE)

  new_text <- dbGetQuery(
    conn,
    glue(
      "
      SELECT
        c.tmp_chunk_id,
        d.text[ c.doc_char_start_idx : c.doc_char_end_idx ] AS text
      FROM {tmp_name} c
      JOIN documents d
      USING (doc_id)
      "
    )
  )

  deoverlapped <- deoverlapped |>
    full_join(
      new_text,
      by = join_by(tmp_chunk_id)
    )
  deoverlapped |>
    select(-overlap_grp, -tmp_chunk_id)
}

pluck_augmented_text <- function(chunks) {
  # c()
  # origin <- chunks$origin
  # headings <- chunks$headings |>
  #   stri_split_lines() |>
  #   lapply(\(x) {
  #     if (nzchar(x)) {
  #       paste0("> ", x, collapse = "\n")
  #     }
  #   })
  origin <- glue("excerpt from: {chunks$origin}")
  headings <- chunks$headings
  about <- as.character(map2(origin, headings, \(o, h) {
    lines <- unlist(stri_split_lines(c(o, h)))
    paste0("> ", lines[nzchar(lines)], collapse = "\n")
  }))
  text <- chunks$text
  glue(
    "
    {about}
    ---
    {text}
    "
  )
}

ragnar_augment <- function(chunks) {
  # TODO:
  # context <- format(chunks$headings... other cols) # as yaml?
  # paste(context, "\n" headings) # fences? quote metadata? or just dump it in context as json?
}


if (FALSE) {
  library(DBI)
  library(stringi)
  library(dplyr, warn.conflicts = FALSE)
  devtools::load_all()

  if (!file.exists("duckdb_docs.ragnar2.duckdb")) {
    paths <- ragnar_find_links("https://duckdb.org/sitemap.html")
    store <- ragnar_store_create_v2(
      location = "duckdb_docs.ragnar2.duckdb",
      embed = embed_ollama(model = "snowflake-arctic-embed2:568m")
    )
    ragnar_store_ingest(
      store,
      paths,
      html_extract_selectors = c(
        "main",
        "#main_content_wrap"
      ),
      html_zap_selectors = c(
        "header",
        "footer",
        ".sidenavigation",
        ".searchoverlay",
        "#sidebar"
      )
    )
    ragnar_store_build_index_v2(store)
    dbDisconnect(store@.con)
  }
  store <- ragnar_store_connect_v2(
    "duckdb_docs.ragnar2.duckdb",
    read_only = TRUE
  )

  show <- \(...) cat(..., sep = paste("\n\n*", strrep("~", 80), "*\n"))

  ragnar_retrieve_vss_v2(store, "create a table from json files", top_k = 10) |>
    ragnar_deoverlap(store = store)
  pull(text) |>
    show()
  ragnar_retrieve_bm25_v2(store, "json") |> pull(text) |> show()
  ragnar_retrieve_bm25_v2(store, "json", top_k = 10) |>
    ragnar_deoverlap(store = store)

  chunks <- bind_rows(
    ragnar_retrieve_vss_v2(store, "create a table from json files", top_k = 15),
    ragnar_retrieve_bm25_v2(store, "json", top_k = 15)
  ) |>
    ragnar_deoverlap(store = store)

  rag

  # |>

  origin = "https://quarto.org/docs/computations/r.html"
  md <- read_as_markdown(origin)
  ragnar_chunk_v2(md)
  ragnar_chunk_v2(md)$headings |> head()
  ragnar_chunk_v2(md)$headings |> tail()
}
