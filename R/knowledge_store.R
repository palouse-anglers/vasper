# Knowledge Store (RAG) Helpers ----
#
# Runtime-side helpers for the ragnar knowledge base. The store itself is a
# pre-built DuckDB file produced offline by data-raw/build_knowledge_store.R and
# committed as a deploy asset. At runtime the app only *reads* the store, so no
# Python/MarkItDown or web scraping happens on Posit Connect Cloud. Query
# embedding uses OPENAI_API_KEY, which is present in the Connect environment.

# Filesystem location of the committed ragnar store.
KNOWLEDGE_STORE_PATH <- file.path("data", "knowledge_store.duckdb")

# Number of passages returned per retrieval by default.
KNOWLEDGE_TOP_K <- 6L
KNOWLEDGE_TOP_K_MAX <- 12L

# Relevance guardrails. Lower cosine distance is better; higher bm25 is better.
KNOWLEDGE_MAX_COSINE_DISTANCE <- 0.72
KNOWLEDGE_MIN_BM25 <- 1.0

#' Connect to the pre-built knowledge store (read-only)
#'
#' The embedding function is serialized into the store at build time and revived
#' on connect, so vector retrieval works here without re-specifying `embed`.
#'
#' @param path Filepath to the ragnar DuckDB store.
#'
#' @return A `RagnarStore` object, or `NULL` if the store is missing/unopenable.
connect_knowledge_store <- function(path = KNOWLEDGE_STORE_PATH) {
  if (!file.exists(path)) {
    warning(
      sprintf(
        paste0(
          "Knowledge store not found at '%s'. Run ",
          "data-raw/build_knowledge_store.R to build it. The search_knowledge ",
          "tool will return an unavailable message until then."
        ),
        path
      ),
      call. = FALSE
    )
    return(NULL)
  }

  tryCatch(
    ragnar::ragnar_store_connect(path, read_only = TRUE),
    error = function(e) {
      warning(
        sprintf(
          "Failed to connect to knowledge store: %s",
          conditionMessage(e)
        ),
        call. = FALSE
      )
      NULL
    }
  )
}

#' Disconnect a knowledge store opened with connect_knowledge_store()
#'
#' @param store A `RagnarStore` object or `NULL`.
#'
#' @return Invisibly `TRUE`.
disconnect_knowledge_store <- function(store) {
  if (!is.null(store)) {
    tryCatch(
      DBI::dbDisconnect(store@con, shutdown = TRUE),
      error = function(e) NULL
    )
  }
  invisible(TRUE)
}

normalize_top_k <- function(top_k) {
  val <- suppressWarnings(as.integer(top_k))
  if (is.na(val) || val < 1L) {
    return(KNOWLEDGE_TOP_K)
  }
  min(val, KNOWLEDGE_TOP_K_MAX)
}

score_list_min <- function(x) {
  if (is.list(x)) {
    out <- vapply(
      x,
      function(v) {
        vv <- suppressWarnings(as.numeric(v))
        vv <- vv[is.finite(vv)]
        if (length(vv) == 0) NA_real_ else min(vv)
      },
      numeric(1)
    )
    return(out)
  }

  vv <- suppressWarnings(as.numeric(x))
  if (length(vv) == 0) {
    return(numeric())
  }
  vv[!is.finite(vv)] <- NA_real_
  vv
}

score_list_max <- function(x) {
  if (is.list(x)) {
    out <- vapply(
      x,
      function(v) {
        vv <- suppressWarnings(as.numeric(v))
        vv <- vv[is.finite(vv)]
        if (length(vv) == 0) NA_real_ else max(vv)
      },
      numeric(1)
    )
    return(out)
  }

  vv <- suppressWarnings(as.numeric(x))
  if (length(vv) == 0) {
    return(numeric())
  }
  vv[!is.finite(vv)] <- NA_real_
  vv
}

filter_relevant_knowledge <- function(results) {
  if (!is.data.frame(results) || nrow(results) == 0) {
    return(results)
  }

  best_cosine <- if ("cosine_distance" %in% names(results)) {
    score_list_min(results$cosine_distance)
  } else {
    rep(NA_real_, nrow(results))
  }

  best_bm25 <- if ("bm25" %in% names(results)) {
    score_list_max(results$bm25)
  } else {
    rep(NA_real_, nrow(results))
  }

  keep <-
    (!is.na(best_cosine) & best_cosine <= KNOWLEDGE_MAX_COSINE_DISTANCE) |
    (!is.na(best_bm25) & best_bm25 >= KNOWLEDGE_MIN_BM25)

  results[keep, , drop = FALSE]
}

build_no_match_message <- function(results, requested_top_k, used_top_k) {
  best_cosine <- NA_real_
  if (
    is.data.frame(results) &&
      nrow(results) > 0 &&
      "cosine_distance" %in% names(results)
  ) {
    cos_vals <- score_list_min(results$cosine_distance)
    cos_vals <- cos_vals[!is.na(cos_vals)]
    if (length(cos_vals) > 0) {
      best_cosine <- min(cos_vals)
    }
  }

  score_line <- if (!is.na(best_cosine)) {
    paste0(
      "Best semantic distance was ",
      format(round(best_cosine, 3), nsmall = 3),
      " (needs <= ",
      format(round(KNOWLEDGE_MAX_COSINE_DISTANCE, 3), nsmall = 3),
      ")."
    )
  } else {
    "No semantic score was available for this query."
  }

  cap_line <- if (!is.na(requested_top_k) && requested_top_k > used_top_k) {
    paste0(
      "Requested top_k=",
      requested_top_k,
      " was capped at ",
      used_top_k,
      "."
    )
  } else {
    ""
  }

  trimws(paste(
    "No strong matches found in the knowledge store for this query.",
    score_line,
    cap_line,
    "Try adding domain terms (for example: crop, soil property, location, or practice)."
  ))
}

#' Format retrieved chunks into a cite-able text block for the LLM
#'
#' @param results A tibble returned by `ragnar::ragnar_retrieve()`.
#'
#' @return A single character scalar with one labelled passage per chunk.
format_knowledge_results <- function(results) {
  if (!is.data.frame(results) || nrow(results) == 0) {
    return("No relevant passages found in the knowledge store.")
  }

  origins <- if ("origin" %in% names(results)) {
    as.character(results$origin)
  } else {
    rep(NA_character_, nrow(results))
  }
  texts <- as.character(results$text)

  blocks <- vapply(
    seq_len(nrow(results)),
    function(i) {
      origin <- origins[[i]]
      source <- if (!is.na(origin) && nzchar(origin)) {
        origin
      } else {
        "unknown source"
      }
      paste0(
        "<<<passage ",
        i,
        ">>>\n",
        "source: ",
        source,
        "\n\n",
        trimws(texts[[i]])
      )
    },
    character(1)
  )

  paste(blocks, collapse = "\n\n")
}

#' Retrieve passages from the knowledge store
#'
#' @param store A `RagnarStore` object or `NULL`.
#' @param query Search query string.
#' @param top_k Number of passages to retrieve.
#'
#' @return A character scalar: formatted passages, or a status message.
run_search_knowledge <- function(store, query, top_k = KNOWLEDGE_TOP_K) {
  if (is.null(store)) {
    return(
      "The knowledge store is not available in this session. No background reference material can be retrieved."
    )
  }

  query <- trimws(as.character(query))
  if (length(query) != 1 || !nzchar(query)) {
    return("Provide a single, non-empty search query.")
  }

  requested_top_k <- suppressWarnings(as.integer(top_k))
  used_top_k <- normalize_top_k(top_k)

  results <- ragnar::ragnar_retrieve(store, query, top_k = used_top_k)
  relevant <- filter_relevant_knowledge(results)

  if (!is.data.frame(relevant) || nrow(relevant) == 0) {
    return(build_no_match_message(results, requested_top_k, used_top_k))
  }

  format_knowledge_results(relevant)
}
