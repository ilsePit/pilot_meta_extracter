# Enhanced DOI Metadata Extraction Script with Improved Country Detection
# ============================================================================
# Improvements:
# 1. Uses Nominatim (OpenStreetMap) geocoding for ambiguous locations
# 2. Better pattern matching for US states and cities
# 3. Checks multiple authors if first has no affiliation
# 4. University/institution geocoding database

# Required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rcrossref,       # For DOI metadata via CrossRef
  sjrdata,         # For SJR rankings
  countrycode,     # For country code standardization
  httr,            # For HTTP requests
  rjson,           # For JSON parsing
  dplyr,           # Data manipulation
  stringr,         # String operations
  tidygeocoder    # For geocoding with multiple services including Nominatim
)

# Load TOP Factor data if available
if (file.exists("top_factor_data.RData")) {
  load("top_factor_data.RData")
  message("TOP Factor data loaded successfully")
} else {
  message("TOP Factor data not found. Run download_top_factor.R first")
  top_factor_data <- NULL
}

# Load SJR data
data("sjr_journals", package = "sjrdata")

# US States database for better matching
us_states <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
  "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
  "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
  "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
  "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
  "New Hampshire", "New Jersey", "New Mexico", "New York",
  "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
  "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
  "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
  "West Virginia", "Wisconsin", "Wyoming", "District of Columbia", "DC"
)

# US State abbreviations
us_state_abbr <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID",
  "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
  "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK",
  "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
  "WI", "WY", "DC"
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

    # Extract journal name and ISSN
    journal <- work$container.title

    if (!is.null(work$issn) && !is.na(work$issn)) {
      # ISSN might be comma-separated string - store all
      issn_split <- strsplit(work$issn, ",")[[1]]
      issn_all <- trimws(issn_split)
      issn <- issn_all[1]  # Primary ISSN
    } else {
      issn <- NA
      issn_all <- NA
    }

    list(
      year = year,
      journal = journal,
      issn = issn,
      issn_all = issn_all
    )
  }, error = function(e) {
    message("Error retrieving basic metadata for ", doi, ": ", e$message)
    list(year = NA, journal = NA, issn = NA)
  })
}

#' Use geocoding to determine country from location
#'
#' @param location Character string of location (city, state, institution, etc.)
#' @return Character string of country name or NA
geocode_location <- function(location) {
  if (is.na(location) || location == "") return(NA)

  tryCatch({
    # Use tidygeocoder with Nominatim (free OSM service)
    # Rate limit: 1 request per second
    Sys.sleep(1.1)

    result <- tidygeocoder::geo(
      address = location,
      method = "osm",  # OpenStreetMap Nominatim
      full_results = TRUE,
      quiet = TRUE
    )

    if (!is.null(result) && nrow(result) > 0 && !is.na(result$lat[1])) {
      # Get country from result
      if ("country" %in% names(result) && !is.na(result$country[1])) {
        return(result$country[1])
      }

      # Try reverse geocoding with the coordinates
      Sys.sleep(1.1)
      reverse_result <- tidygeocoder::reverse_geo(
        lat = result$lat[1],
        long = result$long[1],
        method = "osm",
        full_results = TRUE,
        quiet = TRUE
      )

      if (!is.null(reverse_result) && "country" %in% names(reverse_result)) {
        return(reverse_result$country[1])
      }
    }

    return(NA)
  }, error = function(e) {
    return(NA)
  })
}

#' Extract country from affiliation text using multiple methods
#'
#' @param text Character string of affiliation
#' @param use_geocoding Logical, whether to use geocoding API (slower but more accurate)
#' @return Character string of country name
extract_country_from_text <- function(text, use_geocoding = TRUE) {
  if (is.na(text) || text == "") return(NA)

  # Method 1: Check for explicit country names
  countries <- countrycode::codelist$country.name.en

  # Method 1a: Check last part after comma
  parts <- strsplit(text, ",")[[1]]
  parts <- trimws(parts)

  if (length(parts) > 0) {
    last_part <- parts[length(parts)]

    # Direct country match
    country_match <- tryCatch({
      countrycode::countrycode(last_part, origin = "country.name", destination = "country.name")
    }, error = function(e) NA, warning = function(w) NA)

    if (!is.na(country_match)) {
      return(country_match)
    }
  }

  # Method 2: Look for country names anywhere in text
  for (country_name in countries) {
    # Use word boundaries to avoid partial matches
    if (grepl(paste0("\\b", country_name, "\\b"), text, ignore.case = TRUE)) {
      return(country_name)
    }
  }

  # Method 3: Check for USA/UK variations and US states
  if (grepl("\\b(USA|U\\.S\\.A\\.|United States)\\b", text, ignore.case = TRUE)) {
    return("United States")
  }

  # Check for US states or state abbreviations
  for (state in us_states) {
    if (grepl(paste0("\\b", state, "\\b"), text, ignore.case = TRUE)) {
      return("United States")
    }
  }

  # Check for state abbreviations (must be careful with these)
  for (abbr in us_state_abbr) {
    # Look for state abbreviation in typical formats: ", VA" or "VA 12345" etc.
    if (grepl(paste0(",\\s*", abbr, "\\b|\\s+", abbr, "\\s+\\d"), text)) {
      return("United States")
    }
  }

  if (grepl("\\b(UK|U\\.K\\.|United Kingdom|Great Britain)\\b", text, ignore.case = TRUE)) {
    return("United Kingdom")
  }

  # Method 4: Check for common UK institutions
  if (grepl("University.*(Oxford|Cambridge|London|Edinburgh|Manchester|Bristol|Birmingham)", text, ignore.case = TRUE)) {
    return("United Kingdom")
  }

  # Method 5: Extract institution/city name and use geocoding
  if (use_geocoding) {
    # Try to extract the last meaningful part (usually institution and location)
    if (length(parts) >= 2) {
      # Try geocoding the last 1-2 parts combined
      location_query <- paste(parts[max(1, length(parts)-1):length(parts)], collapse = ", ")
      country_from_geo <- geocode_location(location_query)
      if (!is.na(country_from_geo)) {
        return(country_from_geo)
      }
    }

    # Try geocoding the entire affiliation
    country_from_geo <- geocode_location(text)
    if (!is.na(country_from_geo)) {
      return(country_from_geo)
    }
  }

  return(NA)
}

#' Get corresponding author country from affiliation
#'
#' @param doi Character string of DOI
#' @param use_geocoding Logical, whether to use geocoding
#' @return Character string of country name
get_author_country <- function(doi, use_geocoding = TRUE) {
  tryCatch({
    work <- rcrossref::cr_works(doi = doi)$data

    # Try to get author information
    if (!is.null(work$author) && length(work$author) > 0) {
      authors <- work$author[[1]]

      # Look for affiliation.name column
      if ("affiliation.name" %in% names(authors) && length(authors$affiliation.name) > 0) {
        # Try multiple authors (not just first) in case first has no affiliation
        for (aff in authors$affiliation.name) {
          if (!is.na(aff) && nchar(aff) > 0) {
            country <- extract_country_from_text(aff, use_geocoding)
            if (!is.na(country)) {
              return(country)
            }
          }
        }
      }
    }

    return(NA)
  }, error = function(e) {
    message("  Error retrieving author country: ", e$message)
    return(NA)
  })
}

#' Get journal SJR for a specific year
#'
#' @param issn_all Character vector of ISSNs (can have multiple)
#' @param year Numeric year of publication
#' @return Numeric value of SJR
get_journal_sjr <- function(issn_all, year) {
  if (all(is.na(issn_all)) || is.na(year)) return(NA)

  tryCatch({
    # Try each ISSN
    for (issn in issn_all) {
      if (is.na(issn)) next

      # Clean ISSN (remove all non-numeric characters for matching)
      issn_clean <- gsub("[^0-9]", "", issn)

      # Try to find journal by ISSN for the specific year
      journal_data <- sjr_journals %>%
        filter(year == !!year) %>%
        filter(grepl(issn_clean, gsub("[^0-9,]", "", issn)))

      if (nrow(journal_data) > 0) {
        return(journal_data$sjr[1])
      }

      # If specific year not found, try closest year
      journal_data <- sjr_journals %>%
        filter(grepl(issn_clean, gsub("[^0-9,]", "", issn))) %>%
        mutate(year_diff = abs(year - !!year)) %>%
        arrange(year_diff)

      if (nrow(journal_data) > 0) {
        message("  Note: Using SJR from year ", journal_data$year[1],
                " (requested year ", year, " not available)")
        return(journal_data$sjr[1])
      }
    }

    return(NA)
  }, error = function(e) {
    message("  Error retrieving SJR: ", e$message)
    return(NA)
  })
}

#' Get TOP factor from COS data
#'
#' @param journal_name Character string of journal name
#' @param issn Character string of ISSN
#' @return Numeric TOP factor score
get_top_factor <- function(journal_name, issn) {
  if (is.null(top_factor_data)) return(NA)

  tryCatch({
    # Try matching by ISSN first (more reliable)
    if (!is.na(issn)) {
      issn_clean <- gsub("-", "", issn)

      match <- top_factor_data %>%
        filter(
          gsub("-", "", Issn) == issn_clean |
          gsub("-", "", Eissn) == issn_clean
        )

      if (nrow(match) > 0) {
        return(match$Total[1])
      }
    }

    # Try matching by journal name
    if (!is.na(journal_name)) {
      match <- top_factor_data %>%
        filter(tolower(Journal) == tolower(journal_name))

      if (nrow(match) > 0) {
        return(match$Total[1])
      }

      # Try partial match
      match <- top_factor_data %>%
        filter(grepl(journal_name, Journal, ignore.case = TRUE))

      if (nrow(match) > 0) {
        message("  Note: Matched TOP Factor via partial journal name match")
        return(match$Total[1])
      }
    }

    return(NA)
  }, error = function(e) {
    message("  Error retrieving TOP factor: ", e$message)
    return(NA)
  })
}

#' Main function to extract all metadata from DOI
#'
#' @param doi Character string of DOI
#' @param use_geocoding Logical, whether to use geocoding API
#' @return Data frame with all metadata
extract_doi_metadata <- function(doi, use_geocoding = TRUE) {
  message("\nProcessing DOI: ", doi)

  # Get basic metadata
  basic <- get_basic_metadata(doi)
  message("  Year: ", basic$year)
  message("  Journal: ", basic$journal)
  message("  ISSN: ", basic$issn)

  # Get journal SJR
  if (!all(is.na(basic$issn_all)) && !is.na(basic$year)) {
    sjr <- get_journal_sjr(basic$issn_all, basic$year)
    message("  SJR: ", ifelse(is.na(sjr), "Not found", sjr))
  } else {
    sjr <- NA
    message("  SJR: Cannot retrieve (missing ISSN or year)")
  }

  # Get author country
  if (use_geocoding) {
    message("  Extracting country (with geocoding - may take a moment)...")
  }
  country <- get_author_country(doi, use_geocoding)
  message("  Country: ", ifelse(is.na(country), "Not found", country))

  # Get TOP factor
  if (!is.na(basic$journal) || !is.na(basic$issn)) {
    top_factor <- get_top_factor(basic$journal, basic$issn)
    message("  TOP Factor: ", ifelse(is.na(top_factor), "Not found", top_factor))
  } else {
    top_factor <- NA
    message("  TOP Factor: Cannot retrieve (missing journal name/ISSN)")
  }

  # Combine results
  result <- data.frame(
    doi = doi,
    publication_year = basic$year,
    journal_name = basic$journal,
    issn = basic$issn,
    sjr = sjr,
    corresponding_author_country = country,
    top_factor = top_factor,
    stringsAsFactors = FALSE
  )

  return(result)
}

#' Process multiple DOIs
#'
#' @param dois Character vector of DOIs
#' @param use_geocoding Logical, whether to use geocoding
#' @return Data frame with metadata for all DOIs
process_multiple_dois <- function(dois, use_geocoding = TRUE) {
  results <- lapply(dois, function(doi) {
    result <- extract_doi_metadata(doi, use_geocoding)
    return(result)
  })
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

message("========================================")
message("Enhanced DOI Metadata Extraction")
message("Using Nominatim (OpenStreetMap) Geocoding")
message("========================================")
message("\nNOTE: Geocoding adds ~2 seconds per DOI due to API rate limits")
message("This significantly improves country detection accuracy.\n")

# Process all test DOIs with geocoding
results <- process_multiple_dois(test_dois, use_geocoding = TRUE)

# Display results table
message("\n========================================")
message("RESULTS TABLE")
message("========================================\n")
print(results)

# Save results to CSV
output_file <- "doi_metadata_enhanced_results.csv"
write.csv(results, output_file, row.names = FALSE)
message("\nResults saved to: ", output_file)

# Display detailed summary
message("\n========================================")
message("DETAILED SUMMARY")
message("========================================\n")

for (i in 1:nrow(results)) {
  message("DOI: ", results$doi[i])
  message("  Publication Year: ", results$publication_year[i])
  message("  Journal: ", results$journal_name[i])
  message("  ISSN: ", results$issn[i])
  message("  SJR: ", results$sjr[i])
  message("  Corresponding Author Country: ", results$corresponding_author_country[i])
  message("  TOP Factor: ", results$top_factor[i])
  message("")
}

message("========================================")
message("NOTES")
message("========================================")
message("- Uses OpenStreetMap Nominatim for geocoding (free)")
message("- SJR (Scimago Journal Rank) is used as proxy for Impact Factor")
message("- TOP Factor scores range from 0-29, measuring journal transparency")
message("- Country extraction tries multiple methods:")
message("  1. Direct country name matching")
message("  2. US state detection")
message("  3. UK institution recognition")
message("  4. Geocoding via Nominatim API")
message("- Nominatim rate limit: 1 request/second (adds ~2s per DOI)")
