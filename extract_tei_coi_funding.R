# Extract COI and funding statements from TEI files
# Selects only TEI files matching pilot2 DOIs
# Set use_metacheck <- TRUE before sourcing to also run metacheck for comparison

library(dplyr)
library(stringr)
library(xml2)

# ---- Settings ----
tei_dir <- "../pilot2_pdf_conversion/tei"
pilot2_csv <- "data/2026-04-03_Codingform_PilotingAssessment_pilot2.csv"
if (!exists("use_metacheck")) use_metacheck <- FALSE

if (use_metacheck) {
  if (!"metacheck" %in% rownames(installed.packages())) {
    remotes::install_github("scienceverse/metacheck", dependencies = TRUE)
  }
  library(metacheck)
}

# ---- 1. Load pilot2 DOIs ----
manual <- read.csv(pilot2_csv)
pilot2_dois <- manual[["X1.2..Article.DOI"]] |>
  sub("^https?://doi\\.org/", "", x = _) |>
  sub("(?i)^doi:\\s*", "", x = _, perl = TRUE) |>
  trimws() |>
  unique()
pilot2_dois <- pilot2_dois[pilot2_dois != "" & pilot2_dois != "test"]

message(length(pilot2_dois), " unique pilot2 DOIs loaded")

# ---- 2. Match DOIs to TEI files ----
all_tei <- list.files(tei_dir, pattern = "\\.tei\\.xml$", full.names = TRUE)

# TEI filenames encode DOIs: / → -, then _-_ separates from author info
# Extract DOI from filename by taking text before first `_-_` and converting - back to /
extract_doi_from_filename <- function(fname) {
  base <- basename(fname)
  doi_part <- sub("_-_.*", "", base)
  # The DOI prefix is always 10.XXXX - the first hyphen after the prefix is the /
  # e.g. 10.1080-03069885.2023.2214307 → 10.1080/03069885.2023.2214307
  sub("^(10\\.[0-9]{4,})-", "\\1/", doi_part)
}

tei_dois <- sapply(all_tei, extract_doi_from_filename, USE.NAMES = FALSE)
names(all_tei) <- tei_dois

# Match pilot2 DOIs to TEI files
matched_idx <- match(tolower(pilot2_dois), tolower(tei_dois))
matched_dois <- pilot2_dois[!is.na(matched_idx)]
matched_files <- all_tei[matched_idx[!is.na(matched_idx)]]
unmatched_dois <- pilot2_dois[is.na(matched_idx)]

message(length(matched_dois), " DOIs matched to TEI files, ",
        length(unmatched_dois), " unmatched")
if (length(unmatched_dois) > 0) {
  message("Unmatched DOIs: ", paste(head(unmatched_dois, 10), collapse = ", "),
          if (length(unmatched_dois) > 10) "..." else "")
}

# ---- 3. Metacheck extraction (optional) ----
extract_metacheck <- function(tei_file, doi) {
  paper <- tryCatch(metacheck::read_grobid(tei_file), error = function(e) {
    message("  metacheck read failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(paper)) {
    return(tibble(doi = doi,
                  mc_funding_found = NA, mc_funding_text = NA_character_,
                  mc_coi_found = NA, mc_coi_text = NA_character_))
  }

  # Funding
  funding_mod <- tryCatch(metacheck::module_run(paper, "funding_check"),
                          error = function(e) NULL)
  mc_funding_found <- FALSE
  mc_funding_text <- NA_character_
  if (!is.null(funding_mod)) {
    if (!is.null(funding_mod$summary_table)) {
      mc_funding_found <- any(funding_mod$summary_table$value %in% c(TRUE, "TRUE"), na.rm = TRUE)
    }
    if (!is.null(funding_mod$table) && nrow(funding_mod$table) > 0) {
      # Collapse all text columns into a single string
      text_cols <- sapply(funding_mod$table, is.character)
      texts <- funding_mod$table[, text_cols, drop = FALSE]
      mc_funding_text <- paste(unique(unlist(texts)), collapse = " || ")
      if (mc_funding_text == "") mc_funding_text <- NA_character_
      mc_funding_found <- TRUE
    }
  }

  # COI
  coi_mod <- tryCatch(metacheck::module_run(paper, "coi_check"),
                      error = function(e) NULL)
  mc_coi_found <- FALSE
  mc_coi_text <- NA_character_
  if (!is.null(coi_mod)) {
    if (!is.null(coi_mod$summary_table)) {
      mc_coi_found <- any(coi_mod$summary_table$value %in% c(TRUE, "TRUE"), na.rm = TRUE)
    }
    if (!is.null(coi_mod$table) && nrow(coi_mod$table) > 0) {
      text_cols <- sapply(coi_mod$table, is.character)
      texts <- coi_mod$table[, text_cols, drop = FALSE]
      mc_coi_text <- paste(unique(unlist(texts)), collapse = " || ")
      if (mc_coi_text == "") mc_coi_text <- NA_character_
      mc_coi_found <- TRUE
    }
  }

  tibble(doi = doi,
         mc_funding_found = mc_funding_found, mc_funding_text = mc_funding_text,
         mc_coi_found = mc_coi_found, mc_coi_text = mc_coi_text)
}

# ---- 4. Regex extraction from TEI XML ----

# Match heading against pattern, handling GROBID's spaced-out headings
# e.g. "CON F L IC T OF I N T ER E S T" or "AC K NOW L E DGM E N T S"
heading_matches <- function(heading, pattern, compact_pattern) {
  if (is.na(heading)) return(FALSE)
  str_detect(heading, pattern) ||
    str_detect(gsub("\\s+", "", heading), compact_pattern)
}

# Regex patterns — both spaced and compact versions
# Compact patterns match after whitespace removal (uppercase by convention, case-insensitive)
funding_heading_re <- regex("fund|grant|sponsor|acknowledg|financial.{0,5}support|author.{0,3}note", ignore_case = TRUE)
funding_compact_re <- regex("FUND|GRANT|SPONSOR|ACKNOWLEDG|FINANCIALSUPPORT|AUTHORNOTE", ignore_case = TRUE)
coi_heading_re <- regex("conflict.{0,10}interest|competing.{0,5}interest|disclosure|declaration.{0,15}interest", ignore_case = TRUE)
coi_compact_re <- regex("CONFLICTO?F?INTEREST|COMPETINGINTEREST|DISCLOSURE|DECLARATIONO?F?INTEREST", ignore_case = TRUE)
coi_body_re <- regex("conflict.{0,10}interest|competing.{0,5}interest|no.{0,5}conflict|no known conflict", ignore_case = TRUE)

# Trim trailing non-funding/non-COI boilerplate that GROBID often concatenates
trailing_section_re <- regex(
  paste0(
    "(Informed Consent|",
    "Ethics (Approval|Statement)|Ethical (Approval|Statement)|Ethical standard[.]|",
    "Data Availability|",
    "ORCID iD|About the author|Notes on contributor|",
    "Author Contribution|",
    "Appendix|SUPPORTING INFORMATION|Supplemental Material|",
    "This article is licensed under a Creative Commons)"
  ),
  ignore_case = TRUE
)

clean_extracted_text <- function(text) {
  if (is.na(text) || nchar(text) == 0) return(text)
  loc <- str_locate(text, trailing_section_re)
  if (!is.na(loc[1, "start"]) && loc[1, "start"] > 20) {
    # Only truncate at sentence/line boundaries to avoid cutting mid-sentence
    prefix <- substr(text, max(1, loc[1, "start"] - 2), loc[1, "start"] - 1)
    if (grepl("[.!?]\\s?$|\\n$", prefix) || loc[1, "start"] <= 2) {
      text <- substr(text, 1, loc[1, "start"] - 1) |> trimws()
    }
  }
  text
}

extract_regex <- function(tei_file, doi) {
  doc <- tryCatch({
    d <- read_xml(tei_file)
    xml_ns_strip(d)
    d
  }, error = function(e) {
    message("  XML parse failed: ", conditionMessage(e))
    NULL
  })

  rx_funding_text <- NA_character_
  rx_coi_text <- NA_character_

  if (!is.null(doc)) {
    # Collect leaf-level divs (no child divs) to avoid parent containers
    # that concatenate all children's text. Parent divs used as fallback.
    back_leaf_divs <- xml_find_all(doc, "//back//div[not(div)]")
    body_leaf_divs <- xml_find_all(doc, "//body//div[not(div)]")
    back_all_divs <- xml_find_all(doc, "//back//div")
    body_all_divs <- xml_find_all(doc, "//body//div")
    all_divs <- c(back_leaf_divs, body_leaf_divs)
    all_divs_incl_parents <- c(back_all_divs, body_all_divs)

    # --- Funding ---
    funding_parts <- character(0)
    funding_content_re <- regex("fund|grant|support|financ|award|fellowship|scholarship|sponsor", ignore_case = TRUE)

    # Structured funder elements in header
    funder_nodes <- xml_find_all(doc, "//titleStmt/funder")
    if (length(funder_nodes) > 0)
      funding_parts <- c(funding_parts, xml_text(funder_nodes) |> trimws())

    # Typed funding divs in back — prefer leaf children over parent container
    funding_parent_divs <- xml_find_all(doc, "//back//div[@type='funding']")
    for (fp_node in funding_parent_divs) {
      children <- xml_find_all(fp_node, "./div")
      if (length(children) > 0) {
        for (child in children) {
          child_text <- xml_text(child) |> trimws()
          if (nchar(child_text) > 0 && str_detect(child_text, funding_content_re))
            funding_parts <- c(funding_parts, clean_extracted_text(child_text))
        }
      } else {
        funding_parts <- c(funding_parts, clean_extracted_text(xml_text(fp_node) |> trimws()))
      }
    }

    # Grant numbers from listOrg
    grant_nodes <- xml_find_all(doc, "//listOrg[@type='funding']//idno[@type='grant-number']")
    if (length(grant_nodes) > 0)
      funding_parts <- c(funding_parts, paste("Grant:", xml_text(grant_nodes) |> trimws()))

    # Heading-based search across leaf divs (back + body)
    # For acknowledgment/author-note sections, verify text actually mentions funding
    for (node in all_divs) {
      heading_raw <- xml_text(xml_find_first(node, ".//head"))
      body_text <- xml_text(node) |> trimws()
      cleaned_body <- clean_extracted_text(body_text)
      if (heading_matches(heading_raw, funding_heading_re, funding_compact_re) &&
          !cleaned_body %in% funding_parts) {
        is_ack_heading <- !is.na(heading_raw) &&
          (str_detect(heading_raw, regex("acknowledg|author.{0,3}note", ignore_case = TRUE)) ||
           str_detect(gsub("\\s+", "", heading_raw), regex("ACKNOWLEDG|AUTHORNOTE", ignore_case = TRUE)))
        if (!is_ack_heading || str_detect(body_text, funding_content_re))
          funding_parts <- c(funding_parts, cleaned_body)
      }
    }

    # Last resort: scan ALL divs for explicit funding phrases (catches misplaced sections)
    if (length(funding_parts) == 0) {
      strong_funding_re <- regex("(this (work|study|research|project) was (funded|supported)|funded by|financial support .{0,20}(from|by)|supported by .{0,20}(grant|award|fellowship))", ignore_case = TRUE)
      for (node in all_divs) {
        body_text <- xml_text(node) |> trimws()
        if (str_detect(body_text, strong_funding_re))
          funding_parts <- c(funding_parts, clean_extracted_text(body_text))
      }
    }

    funding_parts <- unique(funding_parts[nchar(funding_parts) > 0])
    if (length(funding_parts) > 0)
      rx_funding_text <- paste(funding_parts, collapse = " || ")

    # --- COI ---
    coi_parts <- character(0)

    # Helper: for long text blocks, extract only COI-relevant sentences
    extract_coi_sentences <- function(text) {
      if (nchar(text) <= 500) return(text)
      sentences <- unlist(str_split(text, "(?<=\\.)\\s+"))
      coi_sentences <- sentences[str_detect(sentences, coi_body_re)]
      if (length(coi_sentences) > 0) paste(coi_sentences, collapse = " ") else text
    }

    # 1. Heading-based search on LEAF divs first (avoids parent containers)
    for (node in all_divs) {
      heading_raw <- xml_text(xml_find_first(node, ".//head"))
      body_text <- xml_text(node) |> trimws()
      if (heading_matches(heading_raw, coi_heading_re, coi_compact_re)) {
        heading_stripped <- gsub("\\s+", "", heading_raw %||% "")
        body_stripped <- gsub("\\s+", "", body_text)
        if (nchar(body_stripped) > nchar(heading_stripped)) {
          coi_parts <- c(coi_parts, extract_coi_sentences(body_text))
        } else {
          coi_parts <- c(coi_parts, "[COI heading found, statement text missing in TEI]")
        }
      }
    }

    # 1b. If leaf search failed entirely, try parent divs with sentence extraction
    if (length(coi_parts) == 0) {
      for (node in all_divs_incl_parents) {
        heading_raw <- xml_text(xml_find_first(node, ".//head"))
        body_text <- xml_text(node) |> trimws()
        if (heading_matches(heading_raw, coi_heading_re, coi_compact_re)) {
          extracted <- extract_coi_sentences(body_text)
          if (nchar(extracted) > 0 && nchar(extracted) <= nchar(body_text))
            coi_parts <- c(coi_parts, extracted)
        }
      }
    }

    # 2. Check inside funding divs for embedded COI text
    if (length(coi_parts) == 0) {
      for (node in xml_find_all(doc, "//back//div[@type='funding']")) {
        body_text <- xml_text(node) |> trimws()
        if (str_detect(body_text, coi_body_re)) {
          sentences <- unlist(str_split(body_text, "(?<=\\.)\\s+"))
          coi_sentences <- sentences[str_detect(sentences, coi_body_re)]
          coi_parts <- c(coi_parts, coi_sentences)
        }
      }
    }

    # 3. Fallback: body-text scan in annex/author-notes child divs
    if (length(coi_parts) == 0) {
      annex_divs <- xml_find_all(doc, "//back//div[@type='annex']//div | //back//div[@type='author-notes']//div")
      for (node in annex_divs) {
        body_text <- xml_text(node) |> trimws()
        if (str_detect(body_text, coi_body_re))
          coi_parts <- c(coi_parts, extract_coi_sentences(body_text))
      }
    }

    # 4. Last resort: scan ALL divs for strong COI phrases
    if (length(coi_parts) == 0) {
      strong_coi_re <- regex("(the authors? (have|has|declare|report) no (known )?conflict|no (potential )?conflict.{0,5}interest|we have no conflict)", ignore_case = TRUE)
      for (node in c(all_divs, all_divs_incl_parents)) {
        body_text <- xml_text(node) |> trimws()
        if (nchar(body_text) < 500 && str_detect(body_text, strong_coi_re))
          coi_parts <- c(coi_parts, body_text)
      }
    }

    coi_parts <- unique(coi_parts[nchar(coi_parts) > 0])
    coi_parts <- sapply(coi_parts, clean_extracted_text, USE.NAMES = FALSE)
    coi_parts <- coi_parts[nchar(coi_parts) > 0]
    if (length(coi_parts) > 0)
      rx_coi_text <- paste(coi_parts, collapse = " || ")
  }

  tibble(doi = doi,
         rx_funding_text = rx_funding_text,
         rx_coi_text = rx_coi_text)
}

# ---- 5. Run extraction on all matched files ----
message("\nExtracting from ", length(matched_files), " TEI files...",
        if (use_metacheck) " (with metacheck)" else "")

rx_results <- vector("list", length(matched_files))
if (use_metacheck) mc_results <- vector("list", length(matched_files))

for (i in seq_along(matched_files)) {
  f <- matched_files[i]
  d <- matched_dois[i]
  message(sprintf("[%d/%d] %s", i, length(matched_files), d))

  rx_results[[i]] <- extract_regex(f, d)
  if (use_metacheck) mc_results[[i]] <- extract_metacheck(f, d)
}

rx_df <- bind_rows(rx_results)

# ---- 6. Combine and save ----
if (use_metacheck) {
  mc_df <- bind_rows(mc_results)
  tei_extracted <- full_join(mc_df, rx_df, by = "doi")
} else {
  tei_extracted <- rx_df
}

# Add flags for easy comparison
tei_extracted <- tei_extracted |>
  mutate(
    rx_funding_found = !is.na(rx_funding_text),
    rx_coi_found = !is.na(rx_coi_text)
  )

output_file <- "data/tei_coi_funding_extracted.csv"
write.csv(tei_extracted, output_file, row.names = FALSE)
message("\nResults saved to ", output_file)

# ---- 7. Quick summary ----
message("\n--- Summary ---")
message("Total DOIs processed: ", nrow(tei_extracted))
message("Regex - funding found: ", sum(tei_extracted$rx_funding_found, na.rm = TRUE),
        " (", round(100 * mean(tei_extracted$rx_funding_found, na.rm = TRUE), 1), "%)")
message("Regex - COI found:     ", sum(tei_extracted$rx_coi_found, na.rm = TRUE),
        " (", round(100 * mean(tei_extracted$rx_coi_found, na.rm = TRUE), 1), "%)")
if (use_metacheck) {
  message("Metacheck - funding found: ", sum(tei_extracted$mc_funding_found, na.rm = TRUE),
          " (", round(100 * mean(tei_extracted$mc_funding_found, na.rm = TRUE), 1), "%)")
  message("Metacheck - COI found:     ", sum(tei_extracted$mc_coi_found, na.rm = TRUE),
          " (", round(100 * mean(tei_extracted$mc_coi_found, na.rm = TRUE), 1), "%)")
  message("\nAgreement (both find / both miss):")
  message("  Funding: ", sum(tei_extracted$mc_funding_found == tei_extracted$rx_funding_found, na.rm = TRUE),
          " / ", nrow(tei_extracted))
  message("  COI:     ", sum(tei_extracted$mc_coi_found == tei_extracted$rx_coi_found, na.rm = TRUE),
          " / ", nrow(tei_extracted))
}
