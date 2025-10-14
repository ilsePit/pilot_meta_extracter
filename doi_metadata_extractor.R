# DOI Metadata Extraction Script
# This script retrieves publication metadata from DOIs including:
# - Publication year, journal name
# - Journal impact factor (or SJR as fallback)
# - Corresponding author country
# - TOP factor from COS data

# Required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rcrossref,    # For DOI metadata via CrossRef
  roadoi,       # Alternative DOI lookup
  sjrdata,      # For SJR rankings
  countrycode,  # For country code standardization
  httr,         # For HTTP requests
  jsonlite,     # For JSON parsing
  dplyr,        # Data manipulation
  stringr,      # String operations
  tidyr         # Data tidying
)

#' Extract basic metadata from DOI
#'
#' @param doi Character string of DOI
#' @return Named list with year, journal, and issn
get_basic_metadata <- function(doi) {
  tryCatch({
    # Use rcrossref to get metadata
    work <- rcrossref::cr_works(doi = doi)$data

    # Extract publication year
    if (!is.null(work$published.print)) {
      year <- as.numeric(work$published.print[[1]])
    } else if (!is.null(work$published.online)) {
      year <- as.numeric(work$published.online[[1]])
    } else if (!is.null(work$created)) {
      year <- as.numeric(substr(work$created, 1, 4))
    } else {
      year <- NA
    }

    # Extract journal name and ISSN
    journal <- work$container.title
    issn <- work$ISSN[[1]][1] # Take first ISSN if multiple

    list(
      year = year,
      journal = journal,
      issn = issn
    )
  }, error = function(e) {
    message("Error retrieving basic metadata for ", doi, ": ", e$message)
    list(year = NA, journal = NA, issn = NA)
  })
}

#' Get corresponding author country from affiliation
#'
#' @param doi Character string of DOI
#' @return Character string of country name
get_author_country <- function(doi) {
  tryCatch({
    work <- rcrossref::cr_works(doi = doi)$data

    # Try to get author information
    if (!is.null(work$author)) {
      authors <- work$author[[1]]

      # Look for corresponding author (usually first or has affiliation)
      # Check if affiliation exists
      if ("affiliation" %in% names(authors) && length(authors$affiliation) > 0) {
        # Get first author's affiliation (often corresponding author)
        affiliation <- authors$affiliation[[1]]

        if (is.data.frame(affiliation) && nrow(affiliation) > 0) {
          affiliation_text <- affiliation$name[1]
        } else if (is.list(affiliation) && length(affiliation) > 0) {
          affiliation_text <- affiliation[[1]]$name
        } else {
          return(NA)
        }

        # Try to extract country from affiliation string
        country <- extract_country_from_text(affiliation_text)
        return(country)
      }
    }

    return(NA)
  }, error = function(e) {
    message("Error retrieving author country for ", doi, ": ", e$message)
    return(NA)
  })
}

#' Extract country from affiliation text
#'
#' @param text Character string of affiliation
#' @return Character string of country name
extract_country_from_text <- function(text) {
  if (is.na(text) || text == "") return(NA)

  # Common patterns: country is often at the end after comma
  # Split by comma and check last parts
  parts <- strsplit(text, ",")[[1]]
  parts <- trimws(parts)

  # Try to match against country names
  countries <- countrycode::codelist$country.name.en

  # Check last 2 parts (city, country or state, country patterns)
  for (i in length(parts):max(1, length(parts)-1)) {
    part <- parts[i]

    # Direct match
    if (part %in% countries) {
      return(part)
    }

    # Partial match (case insensitive)
    match_idx <- which(tolower(countries) == tolower(part))
    if (length(match_idx) > 0) {
      return(countries[match_idx[1]])
    }

    # Try country code conversion
    country_attempt <- tryCatch({
      countrycode::countrycode(part, origin = "country.name", destination = "country.name")
    }, error = function(e) NA, warning = function(w) NA)

    if (!is.na(country_attempt)) {
      return(country_attempt)
    }
  }

  return(NA)
}

#' Get journal impact factor or SJR
#'
#' @param issn Character string of ISSN
#' @param year Numeric year of publication
#' @return Numeric value of IF or SJR
get_journal_metrics <- function(issn, year) {
  tryCatch({
    # Note: Impact Factor data requires subscription/API access
    # Using SJR as it's publicly available via sjrdata package

    # Load SJR data
    data("sjr_journals", package = "sjrdata")

    # Clean ISSN (remove hyphens for matching)
    issn_clean <- gsub("-", "", issn)

    # Try to find journal by ISSN for the specific year
    journal_data <- sjr_journals %>%
      filter(year == !!year) %>%
      filter(gsub("-", "", issn) == issn_clean)

    if (nrow(journal_data) > 0) {
      return(journal_data$sjr[1])
    }

    # If specific year not found, try most recent year
    journal_data <- sjr_journals %>%
      filter(gsub("-", "", issn) == issn_clean) %>%
      arrange(desc(year))

    if (nrow(journal_data) > 0) {
      message("Using SJR from year ", journal_data$year[1], " (requested year ", year, " not available)")
      return(journal_data$sjr[1])
    }

    return(NA)
  }, error = function(e) {
    message("Error retrieving journal metrics for ISSN ", issn, ": ", e$message)
    return(NA)
  })
}

#' Get TOP factor from COS data
#'
#' @param journal_name Character string of journal name
#' @param issn Character string of ISSN
#' @return Numeric TOP factor score
get_top_factor <- function(journal_name, issn) {
  tryCatch({
    # Download TOP Factor data from OSF
    # The data is available at https://osf.io/qatkz/
    # We'll try to access it via OSF API or direct download

    # Option 1: Try to load pre-downloaded data
    if (exists("top_factor_data")) {
      # Match by ISSN or journal name
      match <- top_factor_data %>%
        filter(tolower(Journal) == tolower(journal_name) |
                 ISSN == issn)

      if (nrow(match) > 0) {
        return(match$TOP_Factor[1])
      }
    }

    # Option 2: Download from OSF (requires one-time download)
    message("TOP Factor data not loaded. Please download from https://osf.io/qatkz/ and load into 'top_factor_data' variable")
    return(NA)

  }, error = function(e) {
    message("Error retrieving TOP factor: ", e$message)
    return(NA)
  })
}

#' Download and load TOP Factor data from OSF
#'
#' @return Data frame with TOP Factor data
download_top_factor_data <- function() {
  tryCatch({
    # OSF direct download link (you may need to update this)
    # The actual file URL from OSF project
    url <- "https://osf.io/download/qatkz/"

    # Try to download
    temp_file <- tempfile(fileext = ".csv")
    download.file(url, temp_file, mode = "wb")

    # Read the data
    top_data <- read.csv(temp_file)
    unlink(temp_file)

    return(top_data)
  }, error = function(e) {
    message("Could not automatically download TOP Factor data.")
    message("Please manually download from https://osf.io/qatkz/ and load it.")
    return(NULL)
  })
}

#' Main function to extract all metadata from DOI
#'
#' @param doi Character string of DOI
#' @return Data frame with all metadata
extract_doi_metadata <- function(doi) {
  message("Processing DOI: ", doi)

  # Get basic metadata
  basic <- get_basic_metadata(doi)

  # Get journal metrics
  if (!is.na(basic$issn) && !is.na(basic$year)) {
    metrics <- get_journal_metrics(basic$issn, basic$year)
  } else {
    metrics <- NA
  }

  # Get author country
  country <- get_author_country(doi)

  # Get TOP factor
  if (!is.na(basic$journal) || !is.na(basic$issn)) {
    top_factor <- get_top_factor(basic$journal, basic$issn)
  } else {
    top_factor <- NA
  }

  # Combine results
  result <- data.frame(
    doi = doi,
    publication_year = basic$year,
    journal_name = basic$journal,
    issn = basic$issn,
    impact_factor_or_sjr = metrics,
    corresponding_author_country = country,
    top_factor = top_factor,
    stringsAsFactors = FALSE
  )

  return(result)
}

#' Process multiple DOIs
#'
#' @param dois Character vector of DOIs
#' @return Data frame with metadata for all DOIs
process_multiple_dois <- function(dois) {
  results <- lapply(dois, extract_doi_metadata)
  combined <- bind_rows(results)
  return(combined)
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

# Test DOIs
test_dois <- c(
  "10.1177/1745691620950684",
  "10.1111/bjso.12804",
  "10.1037/pspi0000504",
  "10.1027/1864-9335/a000535"
)

# Try to download TOP Factor data
message("Attempting to download TOP Factor data...")
top_factor_data <- download_top_factor_data()

# Process all test DOIs
message("\n========================================")
message("Processing test DOIs...")
message("========================================\n")

results <- process_multiple_dois(test_dois)

# Display results
print(results)

# Save results to CSV
output_file <- "doi_metadata_results.csv"
write.csv(results, output_file, row.names = FALSE)
message("\nResults saved to: ", output_file)

# Display formatted results
message("\n========================================")
message("SUMMARY OF RESULTS")
message("========================================\n")

for (i in 1:nrow(results)) {
  message("DOI: ", results$doi[i])
  message("  Year: ", results$publication_year[i])
  message("  Journal: ", results$journal_name[i])
  message("  ISSN: ", results$issn[i])
  message("  Impact Factor/SJR: ", results$impact_factor_or_sjr[i])
  message("  Country: ", results$corresponding_author_country[i])
  message("  TOP Factor: ", results$top_factor[i])
  message("")
}
