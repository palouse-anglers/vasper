# knowledge_store helper tests
#
# Run from project root with:
# Rscript tests/test_knowledge_store.R

library(testthat)
library(tibble)

source(file.path("R", "knowledge_store.R"))

describe("normalize_top_k", {
  test_that("caps top_k at configured max and defaults invalid values", {
    expect_equal(normalize_top_k(3), 3L)
    expect_equal(normalize_top_k(100), KNOWLEDGE_TOP_K_MAX)
    expect_equal(normalize_top_k(0), KNOWLEDGE_TOP_K)
    expect_equal(normalize_top_k(NA), KNOWLEDGE_TOP_K)
  })
})

describe("format_knowledge_results", {
  test_that("formats passages with source labels", {
    results <- tibble(
      origin = c("https://example.org/a", "https://example.org/b"),
      text = c("  First passage.  ", "Second passage.")
    )

    out <- format_knowledge_results(results)

    expect_length(out, 1)
    expect_match(out, "<<<passage 1>>>", fixed = TRUE)
    expect_match(out, "source: https://example.org/a", fixed = TRUE)
    expect_match(out, "First passage.", fixed = TRUE)
    expect_match(out, "<<<passage 2>>>", fixed = TRUE)
    expect_match(out, "source: https://example.org/b", fixed = TRUE)
  })

  test_that("handles missing or absent origin", {
    results <- tibble(
      origin = c(NA_character_, ""),
      text = c("No origin.", "Empty origin.")
    )

    out <- format_knowledge_results(results)

    expect_equal(lengths(regmatches(out, gregexpr("unknown source", out))), 2)
  })

  test_that("returns a message for empty results", {
    expect_match(
      format_knowledge_results(tibble(text = character())),
      "No relevant passages",
      fixed = TRUE
    )
    expect_match(
      format_knowledge_results(NULL),
      "No relevant passages",
      fixed = TRUE
    )
  })
})

describe("run_search_knowledge", {
  test_that("returns unavailable message when store is NULL", {
    expect_match(
      run_search_knowledge(NULL, "anything"),
      "not available",
      fixed = TRUE
    )
  })

  test_that("rejects empty queries", {
    fake_store <- structure(list(), class = "RagnarStore")
    expect_match(
      run_search_knowledge(fake_store, "   "),
      "non-empty",
      fixed = TRUE
    )
  })

  test_that("reports no strong matches when distances are weak", {
    store <- ragnar::ragnar_store_create(
      location = ":memory:",
      embed = \(x) matrix(rep(0, length(x) * 4), nrow = length(x), ncol = 4),
      version = 2
    )
    on.exit(DBI::dbDisconnect(store@con, shutdown = TRUE), add = TRUE)

    doc <- ragnar::MarkdownDocument(
      "alpha beta gamma delta epsilon zeta eta theta",
      origin = "https://example.org/alpha"
    )
    chunks <- ragnar::markdown_chunk(doc, target_size = 20)
    ragnar::ragnar_store_insert(store, chunks)
    ragnar::ragnar_store_build_index(store)

    out <- run_search_knowledge(store, "xyzabc123nonexistentterm", top_k = 100)
    expect_match(out, "No strong matches found", fixed = TRUE)
    expect_match(out, "capped at", fixed = TRUE)
  })
})

describe("connect_knowledge_store", {
  test_that("returns NULL and warns when the store file is missing", {
    expect_warning(
      store <- connect_knowledge_store(tempfile(fileext = ".duckdb")),
      "Knowledge store not found"
    )
    expect_null(store)
  })
})
