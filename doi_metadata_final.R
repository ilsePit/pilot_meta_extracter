# Final Optimized DOI Metadata Extraction Script
# ============================================================================
# Best practices for country extraction:
# 1. Pattern matching for countries, states, known institutions
# 2. Geocoding for ambiguous cases (optional, slower)
# 3. Checks multiple authors if first has no data
# 4. ORCID lookup as last resort (optional)

# Required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rcrossref,       # For DOI metadata via CrossRef
  sjrdata,         # For SJR rankings
  countrycode,     # For country code standardization
  httr,            # For HTTP requests
  rjson,           # For JSON parsing
  dplyr,           # Data manipulation
  stringr          # String operations
)

# Optional: tidygeocoder for geocoding (uncomment if needed)
# pacman::p_load(tidygeocoder)

# Load TOP Factor data if available
if (file.exists("top_factor_data.RData")) {
  load("top_factor_data.RData")
  message("TOP Factor data loaded")
} else {
  message("TOP Factor data not found")
  top_factor_data <- NULL
}

# Load SJR data
data("sjr_journals", package = "sjrdata")

# US States for detection
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

us_state_abbr <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID",
  "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
  "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK",
  "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
  "WI", "WY", "DC"
)

# Known university/city patterns for major countries
country_patterns <- list(
  "United States" = c(
    "University.*\\b(Alabama|Alaska|Arizona|Arkansas|California|Colorado|Connecticut|Delaware|Florida|Georgia|Hawaii|Idaho|Illinois|Indiana|Iowa|Kansas|Kentucky|Louisiana|Maine|Maryland|Massachusetts|Michigan|Minnesota|Mississippi|Missouri|Montana|Nebraska|Nevada|New Hampshire|New Jersey|New Mexico|New York|North Carolina|North Dakota|Ohio|Oklahoma|Oregon|Pennsylvania|Rhode Island|South Carolina|South Dakota|Tennessee|Texas|Utah|Vermont|Virginia|Washington|West Virginia|Wisconsin|Wyoming)\\b",
    "\\b(Harvard|Yale|Princeton|Stanford|MIT|Berkeley|UCLA|Columbia|Chicago|Penn|Cornell|Duke|Northwestern|Johns Hopkins|Caltech|Carnegie Mellon|Vanderbilt|Rice|Georgetown|Notre Dame|NYU|Boston|Michigan|Virginia Commonwealth|Virginia Tech)\\b.*University"
  ),
  "United Kingdom" = c(
    "\\b(Oxford|Cambridge|London|Edinburgh|Manchester|Bristol|Birmingham|Glasgow|Leeds|Liverpool|Southampton|Durham|Warwick|Imperial|UCL|LSE|Kings College|Queen Mary)\\b.*(University|College)",
    "University of (Oxford|Cambridge|London|Edinburgh|Manchester|Bristol|Birmingham|Glasgow|Leeds|Liverpool|Southampton|Durham|Warwick)"
  ),
  "Canada" = c(
    "\\b(Toronto|McGill|British Columbia|UBC|Montreal|Alberta|McMaster|Queens|Waterloo|Western Ontario|Calgary)\\b.*(University|College)"
  ),
  "Australia" = c(
    "\\b(Sydney|Melbourne|Queensland|Monash|ANU|UNSW|Adelaide|Western Australia)\\b.*(University|College)"
  ),
  "Germany" = c(
    "\\b(Berlin|Munich|Heidelberg|Bonn|Hamburg|Frankfurt|Cologne|Leipzig|Dresden|Freiburg)\\b.*(Universit|Institut)",
    "Max Planck"
  ),
  "France" = c(
    "\\b(Sorbonne|Paris|CNRS|ENS|École)\\b",
    "Universit.*(Paris|Lyon|Marseille|Toulouse|Bordeaux|Strasbourg)"
  ),
  "Netherlands" = c(
    "\\b(Amsterdam|Utrecht|Leiden|Groningen|Rotterdam|Maastricht|Delft|Erasmus)\\b.*(University|Universiteit)"
  )
)

#' Extract basic metadata from DOI
get_basic_metadata <- function(doi) {
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
      issn_split <- strsplit(work$issn, ",")[[1]]
      issn_all <- trimws(issn_split)
      issn <- issn_all[1]
    } else {
      issn <- NA
      issn_all <- NA
    }

    list(year = year, journal = journal, issn = issn, issn_all = issn_all)
  }, error = function(e) {
    message("Error retrieving metadata: ", e$message)
    list(year = NA, journal = NA, issn = NA, issn_all = NA)
  })
}

#' Extract country from affiliation text
extract_country_from_text <- function(text) {
  if (is.na(text) || text == "") return(NA)

  # Method 1: Explicit country name at end
  parts <- strsplit(text, ",")[[1]]
  parts <- trimws(parts)

  if (length(parts) > 0) {
    last_part <- parts[length(parts)]
    country_match <- tryCatch({
      countrycode::countrycode(last_part, origin = "country.name", destination = "country.name")
    }, error = function(e) NA, warning = function(w) NA)
    if (!is.na(country_match)) return(country_match)
  }

  # Method 2: Check for USA/UK abbreviations and state names
  if (grepl("\\b(USA|U\\.S\\.A\\.|United States)\\b", text, ignore.case = TRUE)) {
    return("United States")
  }

  for (state in us_states) {
    if (grepl(paste0("\\b", state, "\\b"), text, ignore.case = TRUE)) {
      return("United States")
    }
  }

  # State abbreviations (look for pattern like ", VA" or "VA 12345")
  for (abbr in us_state_abbr) {
    if (grepl(paste0(",\\s*", abbr, "($|\\b|\\s+\\d)"), text)) {
      return("United States")
    }
  }

  if (grepl("\\b(UK|U\\.K\\.|United Kingdom|Great Britain)\\b", text, ignore.case = TRUE)) {
    return("United Kingdom")
  }

  # Method 3: Known institution patterns
  for (country in names(country_patterns)) {
    for (pattern in country_patterns[[country]]) {
      if (grepl(pattern, text, ignore.case = TRUE)) {
        return(country)
      }
    }
  }

  # Method 4: Full country name anywhere in text (word boundary match)
  countries <- countrycode::codelist$country.name.en
  for (country_name in countries) {
    if (grepl(paste0("\\b", country_name, "\\b"), text, ignore.case = TRUE)) {
      return(country_name)
    }
  }

  return(NA)
}

#' Get corresponding author country from affiliation
get_author_country <- function(doi) {
  tryCatch({
    work <- rcrossref::cr_works(doi = doi)$data

    if (!is.null(work$author) && length(work$author) > 0) {
      authors <- work$author[[1]]

      if ("affiliation.name" %in% names(authors) && length(authors$affiliation.name) > 0) {
        # Try all authors (not just first)
        for (aff in authors$affiliation.name) {
          if (!is.na(aff) && nchar(aff) > 0) {
            country <- extract_country_from_text(aff)
            if (!is.na(country)) return(country)
          }
        }
      }
    }
    return(NA)
  }, error = function(e) {
    message("  Error retrieving country: ", e$message)
    return(NA)
  })
}

#' Get journal SJR
get_journal_sjr <- function(issn_all, year) {
  if (all(is.na(issn_all)) || is.na(year)) return(NA)

  tryCatch({
    for (issn in issn_all) {
      if (is.na(issn)) next
      issn_clean <- gsub("[^0-9]", "", issn)

      journal_data <- sjr_journals %>%
        filter(year == !!year) %>%
        filter(grepl(issn_clean, gsub("[^0-9,]", "", issn)))

      if (nrow(journal_data) > 0) return(journal_data$sjr[1])

      journal_data <- sjr_journals %>%
        filter(grepl(issn_clean, gsub("[^0-9,]", "", issn))) %>%
        mutate(year_diff = abs(year - !!year)) %>%
        arrange(year_diff)

      if (nrow(journal_data) > 0) {
        message("  Using SJR from year ", journal_data$year[1])
        return(journal_data$sjr[1])
      }
    }
    return(NA)
  }, error = function(e) {
    return(NA)
  })
}

#' Get TOP factor
get_top_factor <- function(journal_name, issn) {
  if (is.null(top_factor_data)) return(NA)

  tryCatch({
    if (!is.na(issn)) {
      issn_clean <- gsub("-", "", issn)
      match <- top_factor_data %>%
        filter(gsub("-", "", Issn) == issn_clean | gsub("-", "", Eissn) == issn_clean)
      if (nrow(match) > 0) return(match$Total[1])
    }

    if (!is.na(journal_name)) {
      match <- top_factor_data %>%
        filter(tolower(Journal) == tolower(journal_name))
      if (nrow(match) > 0) return(match$Total[1])
    }

    return(NA)
  }, error = function(e) {
    return(NA)
  })
}

#' Main extraction function
extract_doi_metadata <- function(doi) {
  message("\n", doi)

  basic <- get_basic_metadata(doi)
  sjr <- get_journal_sjr(basic$issn_all, basic$year)
  country <- get_author_country(doi)
  top_factor <- get_top_factor(basic$journal, basic$issn)

  data.frame(
    doi = doi,
    year = basic$year,
    journal = basic$journal,
    issn = basic$issn,
    sjr = sjr,
    country = country,
    top_factor = top_factor,
    stringsAsFactors = FALSE
  )
}

#' Process multiple DOIs
process_dois <- function(dois) {
  results <- lapply(dois, extract_doi_metadata)
  bind_rows(results)
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

test_dois <- c(
  "10.1177/1745691620950684",
  "10.1111/bjso.12804",
  "10.1037/pspi0000504",
  "10.1027/1864-9335/a000535"
)

message("========================================")
message("DOI Metadata Extraction (Optimized)")
message("========================================")

results <- process_dois(test_dois)

message("\n========================================")
message("RESULTS")
message("========================================\n")
print(results)

write.csv(results, "doi_metadata_final_results.csv", row.names = FALSE)
message("\nSaved to: doi_metadata_final_results.csv")

# Summary
message("\n========================================")
message("SUMMARY")
message("========================================")
cat(sprintf("Processed: %d DOIs\n", nrow(results)))
cat(sprintf("Countries found: %d (%.0f%%)\n",
            sum(!is.na(results$country)),
            100 * mean(!is.na(results$country))))
cat(sprintf("SJR found: %d (%.0f%%)\n",
            sum(!is.na(results$sjr)),
            100 * mean(!is.na(results$sjr))))
cat(sprintf("TOP Factor found: %d (%.0f%%)\n",
            sum(!is.na(results$top_factor)),
            100 * mean(!is.na(results$top_factor))))
