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

# Optional discovery seeds on pubs.extension.wsu.edu.
#
# In some environments these category pages can return 403 or refuse
# connections to non-browser clients, which causes noisy discovery failures.
# Keep discovery opt-in and rely on curated direct PDFs by default.
wsu_pubs_seed <-
  "https://pubs.extension.wsu.edu/product-category/publications/agriculture/soil-science/"

wsu_category_seeds <- c(
  "https://pubs.extension.wsu.edu/product-category/publications/agriculture/soil-science/",
  "https://pubs.extension.wsu.edu/product-category/publications/agriculture/crops/",
  "https://pubs.extension.wsu.edu/product-category/publications/agriculture/field-crops/",
  "https://pubs.extension.wsu.edu/product-category/publications/agriculture/wheat/",
  "https://pubs.extension.wsu.edu/product-category/publications/agriculture/nutrient-management/",
  "https://pubs.extension.wsu.edu/product-category/publications/agriculture/conservation-tillage/"
)

# Known direct WSU publication PDFs that are relevant to this app's scope.
wsu_fallback_pdfs <- c(
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/woocommerce_uploads/FS396E-z5zxao.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/woocommerce_uploads/FS398E-uxhibg.pdf"
)

# Curated direct WSU PDFs discovered from successful prior builds. These avoid
# fragile link discovery on blocked endpoints while preserving broad agronomy
# coverage for planting windows, crop suitability context, and soil management.
wsu_curated_pdfs <- c(
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-3402-sku-PNW661.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-3699-sku-FS166E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-3723-sku-EM073E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4004-sku-FS047E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4597-sku-FS091E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4453-sku-PNW0170.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-3462-sku-PNW706.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-3536-sku-EB1987E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-3772-sku-PNW668.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-3776-sku-PNW703.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4376-sku-FS068E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4410-sku-TB45E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4695-sku-FS369E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-3474-sku-FS221E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-3702-sku-FS104E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4200-sku-PNW702.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4397-sku-EM4815.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4478-sku-FS336E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4479-sku-TB66E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4658-sku-FS358E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-3320-sku-TB41E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-3373-sku-PNW693.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-3485-sku-EB2010E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-3486-sku-FS117E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-3891-sku-FS119E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4269-sku-PNW544.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4448-sku-PNW721.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4559-sku-TB71E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4169-sku-FS170E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4286-sku-TB17E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4729-sku-FS374E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4731-sku-FS378E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4732-sku-FS379E.pdf",
  "https://wpcdn.web.wsu.edu/wp-ecommerce/uploads/sites/2/product-4746-sku-EM124E.pdf",
  "https://wpcdn.web.wsu.edu/extension/uploads/sites/30/home-gardener-guide-to-soils-EM063E.pdf"
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

  if (is.null(out)) {
    return(character())
  }

  if (is.list(out)) {
    out <- unlist(out, use.names = FALSE)
  }

  out <- as.character(out)
  out <- out[!is.na(out) & nzchar(out)]
  unique(out)
}

is_pdf_url <- function(url) {
  grepl("\\.pdf($|[?#])", url, ignore.case = TRUE)
}

is_trusted_wsu_pdf <- function(url) {
  is_pdf_url(url) &
    grepl(
      "(wpcdn\\.web\\.wsu\\.edu|pubs\\.extension\\.wsu\\.edu)",
      url,
      ignore.case = TRUE
    )
}

is_home_garden_source <- function(url) {
  grepl(
    "home-garden|home-gardener-guide|home-vegetable-gardening|growing-roses|home-lawns|growing-garlic",
    url,
    ignore.case = TRUE
  )
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

soil_health_pages <- soil_health_pages[
  !grepl(
    "/(page|paged)/|/tag/|/category/|newsletter|shop|events|about-us|contact|washi-team|sponsors|author/|wp-login",
    soil_health_pages
  )
]

# Optional discovery path for environments where pubs.extension.wsu.edu allows
# automated requests. Disabled by default to avoid repeated connection failures.
enable_wsu_discovery <- tolower(Sys.getenv(
  "ENABLE_WSU_DISCOVERY",
  "false"
)) %in%
  c(
    "1",
    "true",
    "yes"
  )

wsu_discovered_pdfs <- character()
if (enable_wsu_discovery) {
  wsu_pages <- safe_find_links(
    wsu_pubs_seed,
    url_filter = \(url) {
      url[grepl("pubs\\.extension\\.wsu\\.edu/product/", url) | is_pdf_url(url)]
    }
  )

  wsu_category_links <- unlist(
    lapply(
      wsu_category_seeds,
      function(seed) {
        safe_find_links(
          seed,
          url_filter = \(url) {
            url[
              grepl("pubs\\.extension\\.wsu\\.edu/product/", url) |
                is_pdf_url(url)
            ]
          }
        )
      }
    ),
    use.names = FALSE
  )

  wsu_product_pages <- unique(c(
    wsu_pages[grepl("pubs\\.extension\\.wsu\\.edu/product/", wsu_pages)],
    wsu_category_links[grepl(
      "pubs\\.extension\\.wsu\\.edu/product/",
      wsu_category_links
    )]
  ))

  wsu_product_pdfs <- unlist(
    lapply(
      wsu_product_pages,
      function(product_page) {
        safe_find_links(
          product_page,
          url_filter = \(url) {
            url[is_trusted_wsu_pdf(url)]
          }
        )
      }
    ),
    use.names = FALSE
  )

  wsu_discovered_pdfs <- unique(c(
    wsu_pages[is_trusted_wsu_pdf(wsu_pages)],
    wsu_category_links[is_trusted_wsu_pdf(wsu_category_links)],
    wsu_product_pdfs
  ))
} else {
  message(
    "WSU discovery disabled; using curated direct PDFs. Set ENABLE_WSU_DISCOVERY=true to enable."
  )
}

# Build final WSU list from stable curated PDFs + fallback + any discovered PDFs.
wsu_pages <- unique(c(wsu_curated_pdfs, wsu_fallback_pdfs, wsu_discovered_pdfs))
wsu_pages <- wsu_pages[!is_home_garden_source(wsu_pages)]

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
