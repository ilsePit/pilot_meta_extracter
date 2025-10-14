# Quick Test Script for DOI Metadata Extraction
# This is a simplified version that works without TOP Factor data
# ============================================================================

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rcrossref, dplyr, countrycode)

# Test with one DOI first
test_doi <- "10.1177/1745691620950684"

message("Testing with DOI: ", test_doi)

# Get metadata from CrossRef
work <- rcrossref::cr_works(doi = test_doi)$data

# 1. Extract publication year
if (!is.null(work$published.print) && !is.na(work$published.print)) {
  year <- as.numeric(substr(work$published.print, 1, 4))
} else if (!is.null(work$published.online) && !is.na(work$published.online)) {
  year <- as.numeric(substr(work$published.online, 1, 4))
} else if (!is.null(work$issued) && !is.na(work$issued)) {
  year <- as.numeric(substr(work$issued, 1, 4))
} else {
  year <- NA
}

message("Publication Year: ", year)

# 2. Extract journal name
journal <- work$container.title
message("Journal Name: ", journal)

# 3. Extract ISSN
if (!is.null(work$issn) && !is.na(work$issn)) {
  issn_split <- strsplit(work$issn, ",")[[1]]
  issn <- trimws(issn_split[1])
} else {
  issn <- NA
}
message("ISSN: ", issn)

# 4. Try to get author information
message("\n--- Author Information ---")
if (!is.null(work$author) && length(work$author) > 0) {
  authors <- work$author[[1]]
  message("Number of authors: ", nrow(authors))

  # Check first author's affiliation
  if ("affiliation.name" %in% names(authors)) {
    message("First author: ", authors$given[1], " ", authors$family[1])

    if (!is.na(authors$affiliation.name[1])) {
      message("Affiliation: ", authors$affiliation.name[1])
    }
  }
}

message("\n========================================")
message("Now testing all four DOIs...")
message("========================================\n")

# All test DOIs
test_dois <- c(
  "10.1177/1745691620950684",
  "10.1111/bjso.12804",
  "10.1037/pspi0000504",
  "10.1027/1864-9335/a000535"
)

# Process each DOI
results_list <- list()

for (doi in test_dois) {
  message("Processing: ", doi)

  tryCatch({
    work <- rcrossref::cr_works(doi = doi)$data

    # Extract year
    if (!is.null(work$published.print) && !is.na(work$published.print)) {
      year <- as.numeric(substr(work$published.print, 1, 4))
    } else if (!is.null(work$published.online) && !is.na(work$published.online)) {
      year <- as.numeric(substr(work$published.online, 1, 4))
    } else if (!is.null(work$issued) && !is.na(work$issued)) {
      year <- as.numeric(substr(work$issued, 1, 4))
    } else if (!is.null(work$created) && !is.na(work$created)) {
      year <- as.numeric(substr(work$created, 1, 4))
    } else {
      year <- NA
    }

    # Extract journal and ISSN
    journal <- work$container.title
    if (!is.null(work$issn) && !is.na(work$issn)) {
      # ISSN might be comma-separated string
      issn_split <- strsplit(work$issn, ",")[[1]]
      issn <- trimws(issn_split[1])
    } else {
      issn <- NA
    }

    # Try to extract country
    country <- NA
    if (!is.null(work$author) && length(work$author) > 0) {
      authors <- work$author[[1]]
      if ("affiliation.name" %in% names(authors) && length(authors$affiliation.name) > 0) {
        # Get first non-NA affiliation
        aff_text <- NA
        for (aff in authors$affiliation.name) {
          if (!is.na(aff) && nchar(aff) > 0) {
            aff_text <- aff
            break
          }
        }

        if (!is.na(aff_text) && nchar(aff_text) > 0) {
          # Try multiple extraction methods

          # Method 1: Check last part after comma
          parts <- strsplit(aff_text, ",")[[1]]
          parts <- trimws(parts)
          if (length(parts) > 0) {
            last_part <- parts[length(parts)]
            country_match <- tryCatch({
              countrycode::countrycode(last_part, origin = "country.name", destination = "country.name")
            }, error = function(e) NA, warning = function(w) NA)
            if (!is.na(country_match)) {
              country <- country_match
            }
          }

          # Method 2: Look for country names anywhere in text
          if (is.na(country)) {
            countries <- countrycode::codelist$country.name.en
            for (country_name in countries) {
              if (grepl(country_name, aff_text, ignore.case = TRUE)) {
                country <- country_name
                break
              }
            }
          }

          # Method 3: Check for USA variations
          if (is.na(country)) {
            if (grepl("\\b(USA|U\\.S\\.A\\.|United States)\\b", aff_text, ignore.case = TRUE)) {
              country <- "United States"
            } else if (grepl("\\b(UK|U\\.K\\.|United Kingdom)\\b", aff_text, ignore.case = TRUE)) {
              country <- "United Kingdom"
            }
          }
        }
      }
    }

    results_list[[doi]] <- data.frame(
      doi = doi,
      year = year,
      journal = journal,
      issn = issn,
      country = country,
      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    message("  Error: ", e$message)
    results_list[[doi]] <- data.frame(
      doi = doi,
      year = NA,
      journal = NA,
      issn = NA,
      country = NA,
      stringsAsFactors = FALSE
    )
  })

  Sys.sleep(0.5)  # Be nice to the API
}

# Combine results
results_df <- bind_rows(results_list)

# Display results
message("\n========================================")
message("RESULTS")
message("========================================\n")
print(results_df)

# Save to CSV
write.csv(results_df, "doi_quick_test_results.csv", row.names = FALSE)
message("\nResults saved to: doi_quick_test_results.csv")

# Display detailed summary
message("\n========================================")
message("DETAILED SUMMARY")
message("========================================\n")

for (i in 1:nrow(results_df)) {
  message("DOI: ", results_df$doi[i])
  message("  Year: ", results_df$year[i])
  message("  Journal: ", results_df$journal[i])
  message("  ISSN: ", results_df$issn[i])
  message("  Country: ", results_df$country[i])
  message("")
}

message("\nNote: To add Impact Factor/SJR and TOP Factor data,")
message("please run the full script: doi_metadata_extractor.R")
message("and set up the TOP Factor data using: top_factor_manual_setup.R")
