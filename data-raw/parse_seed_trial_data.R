# data-raw/parse_seed_trial_data.R
# Parse NWGG / WSU 2023 Dayton and Walla Walla winter wheat variety trial PDFs
# into a clean CSV committed to data/seed_variety_trials.csv.
#
# Run once (and whenever new trial PDFs are added) from the project root:
#   Rscript data-raw/parse_seed_trial_data.R
#
# Requirements:
#   - ragnar::read_as_markdown() (uses Python + MarkItDown)
#   - PDFs are fetched from inetsgi.com at parse time.
#
# Output:
#   data/seed_variety_trials.csv

library(ragnar)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# ---- Source PDFs ----
# fa395e67: 2023 Dayton WSU HRW (Hard Red Winter)
# ae76dba6: 2023 Dayton WSU SWW (Soft White Winter, with some HRW noted)
# fe5c275e: 2023 Dayton WSU HRW + 2023 Walla Walla WSU HRW (two trials)
TRIAL_PDFS <- list(
  list(
    url = "https://inetsgi.com/customer/780/fa395e67.pdf",
    label = "2023 Dayton WSU HRW",
    source = "nwgg_dayton_hrw_2023",
    wheat_class_override = NULL
  ),
  list(
    url = "https://inetsgi.com/customer/780/ae76dba6.pdf",
    label = "2023 Dayton WSU SWW",
    source = "nwgg_dayton_sww_2023",
    # This trial is primarily SWW; HRW varieties are noted in the PDF
    # in red but colour cannot be extracted from text. NWGRGR labels it SWW.
    wheat_class_override = "SWW"
  ),
  list(
    url = "https://inetsgi.com/customer/780/fe5c275e.pdf",
    label = "2023 Dayton + Walla Walla WSU HRW",
    source = "nwgg_dayton_ww_hrw_2023",
    wheat_class_override = NULL
  )
)

OUTPUT_PATH <- file.path("data", "seed_variety_trials.csv")

# ---- Helpers ----

# Download a URL to a local temp file and return the path.
download_to_temp <- function(url) {
  ext <- tools::file_ext(url)
  if (!nzchar(ext)) {
    ext <- "pdf"
  }
  dest <- tempfile(fileext = paste0(".", ext))
  utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  dest
}

# Parse one markdown table row into a named character vector given column names.
parse_md_row <- function(row, col_names) {
  # Split on | and trim whitespace
  parts <- stringr::str_split(row, "\\|")[[1]]
  parts <- trimws(parts)
  # Drop leading/trailing empty cells from outer pipes
  parts <- parts[nzchar(parts)]
  # Pad or trim to match expected number of columns
  length(parts) <- length(col_names)
  stats::setNames(parts, col_names)
}

# Determine whether a row looks like a markdown separator line (---|---)
is_md_separator <- function(line) {
  grepl("^\\|?\\s*[-:]+", trimws(line))
}

# Extract the date portion from strings like "Planted: 10/6/2022"
extract_date_str <- function(txt, keyword) {
  m <- regmatches(
    txt,
    regexpr(paste0(keyword, ":\\s*(\\d{1,2}/\\d{1,2}/\\d{4})"), txt)
  )
  if (length(m) == 0 || !nzchar(m)) {
    return(NA_character_)
  }
  sub(paste0(keyword, ":\\s*"), "", m)
}

# ---- Parsers ----

# Parse a "clean" trial table where each column is a separate pipe-delimited cell.
# Handles headers: Variety | Yield | Test Weight | Protein | Height (In.) | Maturity
parse_clean_trial_table <- function(lines) {
  # Find header line
  header_idx <- which(grepl("Variety", lines) & grepl("Yield", lines))
  if (length(header_idx) == 0) {
    return(NULL)
  }
  header_idx <- header_idx[[1]]

  col_names <- c(
    "variety",
    "yield_bu_ac",
    "test_weight_lb_bu",
    "protein_pct",
    "height_in",
    "maturity_days"
  )

  data_lines <- lines[(header_idx + 2):length(lines)]
  data_lines <- data_lines[grepl("^\\|", data_lines)]
  data_lines <- data_lines[!is_md_separator(data_lines)]

  if (length(data_lines) == 0) {
    return(NULL)
  }

  rows <- lapply(data_lines, parse_md_row, col_names = col_names)
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  df
}

# Parse the "merged-column" trial table format from ae76dba6 where
# "Yield" and "Test Weight" share a cell, and "Height" and "Maturity" share a cell.
# Header:  Variety | Yield Test Weight | Protein | Height (In.) Maturity
# Row:     LCS Kamiak | 87 60.4 | 11.7 | 29 143
parse_merged_trial_table <- function(lines) {
  # Find header line with merged column names
  header_idx <- which(
    grepl("Variety", lines) &
      grepl("Yield", lines) &
      grepl("Test Weight", lines)
  )
  if (length(header_idx) == 0) {
    return(NULL)
  }
  header_idx <- header_idx[[1]]

  col_names_raw <- c(
    "variety",
    "yield_testweight",
    "protein_pct",
    "height_maturity"
  )

  data_lines <- lines[(header_idx + 2):length(lines)]

  # Some rows have their "maturity" value orphaned on the next line (PDF wrap).
  # Reconstitute: if a line is a bare number following a | line that's missing
  # its maturity, append it to the previous row.
  reconstituted <- character()
  i <- 1
  while (i <= length(data_lines)) {
    line <- data_lines[[i]]
    if (grepl("^\\|", line)) {
      # Check if next line is a bare number (orphaned maturity)
      if (
        i < length(data_lines) &&
          grepl("^\\s*\\d{3}\\s*$", data_lines[[i + 1]])
      ) {
        # Append the orphaned maturity to the height cell of this row
        orphan <- trimws(data_lines[[i + 1]])
        # Replace trailing | with " <orphan> |"
        line <- sub("\\|\\s*$", paste0(" ", orphan, " |"), line)
        i <- i + 1
      }
      reconstituted <- c(reconstituted, line)
    }
    i <- i + 1
  }

  data_lines <- reconstituted[!is_md_separator(reconstituted)]

  if (length(data_lines) == 0) {
    return(NULL)
  }

  rows <- lapply(data_lines, parse_md_row, col_names = col_names_raw)
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)

  # Split merged columns
  df <- df %>%
    mutate(
      yield_parts = str_match(yield_testweight, "^(\\S+)\\s+(\\S+)$"),
      yield_bu_ac = ifelse(
        !is.na(yield_parts[, 2]),
        yield_parts[, 2],
        NA_character_
      ),
      test_weight_lb_bu = ifelse(
        !is.na(yield_parts[, 3]),
        yield_parts[, 3],
        NA_character_
      ),

      hm_parts = str_match(trimws(height_maturity), "^(\\d+)\\s+(\\d+)$"),
      height_in = ifelse(!is.na(hm_parts[, 2]), hm_parts[, 2], NA_character_),
      maturity_days = ifelse(
        !is.na(hm_parts[, 3]),
        hm_parts[, 3],
        NA_character_
      )
    ) %>%
    select(
      variety,
      yield_bu_ac,
      test_weight_lb_bu,
      protein_pct,
      height_in,
      maturity_days
    )

  df
}

# ---- Wheel class detection for ae76dba6 ----
# In ae76dba6, "Red Varieties are Hard Red Winter" is noted; in the markdown
# output there is no color info, but we can identify known HRW varieties vs SWW.
# We'll mark wheat_class as from the trial label since we can't detect color.

# ---- Main ingestion function ----
ingest_trial_pdf <- function(
  pdf_url,
  trial_label,
  source_tag,
  wheat_class_override = NULL
) {
  message("Downloading: ", pdf_url)
  local_path <- download_to_temp(pdf_url)
  on.exit(unlink(local_path), add = TRUE)

  message("Parsing markdown...")
  doc <- ragnar::read_as_markdown(local_path)
  text <- as.character(doc)
  lines <- stringr::str_split(text, "\n")[[1]]

  # Find all trial section headers (lines containing "Winter Wheat Trial")
  trial_headers <- which(
    grepl("Winter Wheat Trial", lines, fixed = TRUE) &
      !grepl("^\\|", lines)
  )

  if (length(trial_headers) == 0) {
    warning("No trial headers found in: ", pdf_url)
    return(NULL)
  }

  all_trials <- list()

  for (h_idx in seq_along(trial_headers)) {
    start <- trial_headers[[h_idx]]
    end <- if (h_idx < length(trial_headers)) {
      trial_headers[[h_idx + 1]] - 1
    } else {
      length(lines)
    }

    section_lines <- lines[start:end]
    header_text <- section_lines[[1]]

    # Extract trial metadata from header region
    meta_text <- paste(
      section_lines[1:min(6, length(section_lines))],
      collapse = " "
    )
    planted <- extract_date_str(meta_text, "Planted")
    harvested <- extract_date_str(meta_text, "Harvested")

    # Determine wheat class and location from header text;
    # use the override if provided (e.g. for mixed SWW trials)
    wheat_class <- if (!is.null(wheat_class_override)) {
      wheat_class_override
    } else {
      dplyr::case_when(
        grepl("\\bHRW\\b", header_text) ~ "HRW",
        grepl("SWW|Soft White", header_text, ignore.case = TRUE) ~ "SWW",
        TRUE ~ "Mixed"
      )
    }

    location <- dplyr::case_when(
      grepl("Dayton", header_text) ~ "Dayton, WA",
      grepl("Walla Walla", header_text) ~ "Walla Walla, WA",
      TRUE ~ NA_character_
    )

    trial_name <- trimws(header_text)

    # Detect table type: merged vs clean
    header_line <- section_lines[which(
      grepl("Variety", section_lines) & grepl("Yield", section_lines)
    )]
    is_merged <- length(header_line) > 0 &&
      grepl("Yield Test Weight", header_line[[1]], fixed = TRUE)

    parsed_df <- if (is_merged) {
      parse_merged_trial_table(section_lines)
    } else {
      parse_clean_trial_table(section_lines)
    }

    if (is.null(parsed_df) || nrow(parsed_df) == 0) {
      message("  No rows parsed for: ", trial_name)
      next
    }

    parsed_df <- parsed_df %>%
      mutate(
        trial_name = trial_name,
        trial_year = 2023L,
        location = location,
        wheat_class = wheat_class,
        planted_date = planted,
        harvested_date = harvested,
        source = source_tag,
        source_url = pdf_url
      )

    all_trials <- c(all_trials, list(parsed_df))
    message("  Parsed ", nrow(parsed_df), " varieties from: ", trial_name)
  }

  if (length(all_trials) == 0) {
    return(NULL)
  }
  dplyr::bind_rows(all_trials)
}

# ---- Run ingestion ----
results <- lapply(TRIAL_PDFS, function(p) {
  tryCatch(
    ingest_trial_pdf(p$url, p$label, p$source, p$wheat_class_override),
    error = function(e) {
      message("ERROR processing ", p$url, ": ", conditionMessage(e))
      NULL
    }
  )
})

combined <- dplyr::bind_rows(results)

# ---- Tidy column types ----
combined <- combined %>%
  mutate(
    variety = trimws(variety),
    yield_bu_ac = suppressWarnings(as.numeric(yield_bu_ac)),
    test_weight_lb_bu = suppressWarnings(as.numeric(test_weight_lb_bu)),
    protein_pct = suppressWarnings(as.numeric(protein_pct)),
    height_in = suppressWarnings(as.integer(height_in)),
    maturity_days = suppressWarnings(as.integer(maturity_days)),
    trial_year = as.integer(trial_year)
  ) %>%
  filter(!is.na(variety) & nzchar(variety))

# Deduplicate: fa395e67 and the first table of fe5c275e are the same Dayton HRW trial.
combined <- combined %>%
  distinct(
    trial_name,
    location,
    variety,
    planted_date,
    harvested_date,
    .keep_all = TRUE
  )

message(sprintf(
  "Total varieties after dedup: %d across %d unique trials",
  nrow(combined),
  length(unique(combined$trial_name))
))

# ---- Write output ----
readr::write_csv(combined, OUTPUT_PATH)
message("Written to ", OUTPUT_PATH)
