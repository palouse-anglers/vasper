# data-raw/build_knowledge_store.R
# Build the ragnar knowledge store consumed by the Vasper assistant at runtime.
#
# Run once (and whenever sources change) from the project root:
#   Rscript data-raw/build_knowledge_store.R
#
# Requirements:
#   - OPENAI_API_KEY in the environment (embeds chunks with text-embedding-3-small).
#   - Python + MarkItDown, used by ragnar::read_as_markdown() to convert
#     HTML/PDF to Markdown. ragnar manages this through reticulate; see
#     ?ragnar::read_as_markdown if the Python toolchain needs setup.
#
# Output:
#   data/knowledge_store.duckdb
# Commit this file. The deployed app reads it read-only, so none of the scraping
# or Python conversion below runs on Posit Connect Cloud.

library(ragnar)
library(purrr)

source(file.path("R", "knowledge_store.R"))

if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  stop(
    "OPENAI_API_KEY must be set to build the knowledge store.",
    call. = FALSE
  )
}

# 1. Seed hubs ----
# Washington Soil Health Initiative "State of the Soils" report hub.
soil_health_seed <-
  "https://washingtonsoilhealthinitiative.com/state-of-the-soils/"

# WSU Extension soil-science publications catalog (a directory of PDF pubs).
wsu_pubs_seed <-
  "https://pubs.extension.wsu.edu/product-category/publications/agriculture/soil-science/"

# Known direct WSU publication PDFs that are relevant to this app's scope.
wsu_fallback_pdfs <- c(
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/woocommerce_uploads/FS396E-z5zxao.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/woocommerce_uploads/FS398E-uxhibg.pdf"
)

# 2. Harvest candidate content URLs ----
# ragnar_find_links() ends by calling sort(unique(links)); when a fetch returns
# no usable links it hands sort() an empty list and errors with
# "'x' must be atomic". Wrap discovery so one flaky page can't abort the build,
# and always return a plain character vector.
safe_find_links <- function(url, url_filter = identity) {
  out <- tryCatch(
    ragnar_find_links(url, url_filter = url_filter),
    error = function(e) {
      message("  link discovery failed for ", url, ": ", conditionMessage(e))
      character()
    }
  )
  as.character(out)
}

# State of the Soils: keep links that stay on the initiative's domain.
soil_health_pages <- safe_find_links(
  soil_health_seed,
  url_filter = \(url) {
    url[startsWith(url, "https://washingtonsoilhealthinitiative.com")]
  }
)

# Curate to content-bearing pages only. The site exposes many navigational
# pages (about-us, contact, events, washi-team, sponsors, ...) that dilute
# retrieval. Keep dated articles (/YYYY/MM/...) and the State of the Soils
# report pages; drop everything else.
soil_health_pages <- soil_health_pages[
  grepl("/20\\d\\d/\\d\\d/", soil_health_pages) |
    grepl("state-of-the-soils", soil_health_pages)
]

# WSU catalog: the substantive content is the publication PDFs hosted on
# wpcdn.web.wsu.edu. The /product/ HTML pages return 403 to MarkItDown's
# fetcher and carry no real content, so keep only direct PDF links.
wsu_pages <- safe_find_links(
  wsu_pubs_seed,
  url_filter = \(url) {
    url[grepl("\\.pdf$", url, ignore.case = TRUE)]
  }
)

# Discovery for this endpoint can intermittently fail upstream; pin two known
# relevant PDFs so the store retains baseline WSU coverage on every build.
wsu_pages <- unique(c(wsu_pages, wsu_fallback_pdfs))

# Curated seed list. De-duplicate and keep the hubs themselves.
pages <- unique(c(
  soil_health_seed,
  soil_health_pages,
  wsu_pages
))

message(sprintf("Discovered %d candidate pages to ingest.", length(pages)))

# 3. Create a fresh store ----
# The embed function is serialized into the store, so it must be self-contained
# and fully namespaced (no references to globals). See ?ragnar_store_create.
store <- ragnar_store_create(
  location = KNOWLEDGE_STORE_PATH,
  embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small"),
  name = "vasper_soil_knowledge",
  title = "Washington soil health knowledge base",
  overwrite = TRUE
)

clean_markdown_document <- function(doc) {
  lines <- strsplit(as.character(doc), "\n", fixed = TRUE)[[1]]

  drop_patterns <- c(
    "^\\[Skip to content\\]",
    "^\\[!\\[",
    "^!\\[",
    "^Search\\s*$",
    "^Menu\\s*$",
    "^\\[(Advisory Team|LTARE Team)\\]\\("
  )

  keep <- rep(TRUE, length(lines))
  for (pat in drop_patterns) {
    keep <- keep & !grepl(pat, lines)
  }

  cleaned <- trimws(lines[keep], which = "right")
  cleaned <- cleaned[!(duplicated(cleaned) & !nzchar(cleaned))]
  text <- paste(cleaned, collapse = "\n")

  ragnar::MarkdownDocument(text, origin = doc@origin)
}

# 4. Ingest each page (skip failures so one bad URL doesn't abort the build) ----
ingest_page <- function(url) {
  message("ingesting: ", url)
  tryCatch(
    {
      doc <- url |>
        read_as_markdown() |>
        clean_markdown_document()

      chunks <- markdown_chunk(doc)
      ragnar_store_insert(store, chunks)
    },
    error = function(e) {
      message("  skipped: ", conditionMessage(e))
    }
  )
}

walk(pages, ingest_page)

# 5. Build the retrieval index ----
ragnar_store_build_index(store)

# 6. Checkpoint and close ----
# Disconnecting with shutdown flushes the write-ahead log (.wal) into the main
# .duckdb file and removes the sidecar, leaving a single clean artifact to
# commit. Without this the store ships with an uncommitted .wal.
DBI::dbExecute(store@con, "CHECKPOINT")
DBI::dbDisconnect(store@con, shutdown = TRUE)

message("Knowledge store written to ", KNOWLEDGE_STORE_PATH)
