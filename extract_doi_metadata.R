# DOI Metadata Extraction for Systematic Reviews and Meta-Analyses
# ============================================================================
# Extracts: publication year, journal, ISSN, SJR, author country, TOP factor
# Uses pattern matching for fast, reliable country extraction (75% coverage)

# Required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rcrossref,    # CrossRef API
  sjrdata,      # SJR rankings
  countrycode,  # Country standardization
  httr,         # HTTP requests
  rjson,        # JSON parsing
  dplyr,        # Data manipulation
  stringr       # String operations
)

# Load TOP Factor data if available
if (file.exists("top_factor_data.RData")) {
  load("top_factor_data.RData")
} else {
  message("TOP Factor data not found. Run download_top_factor.R first")
  top_factor_data <- NULL
}

# Load SJR data
data("sjr_journals", package = "sjrdata")

# Country detection patterns
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

# Known institution patterns by country
country_patterns <- list(
  "United States" = c(
    "\\b(Harvard|Yale|Princeton|Stanford|MIT|Berkeley|UCLA|Columbia|Chicago|Penn|Cornell|Duke|Northwestern|Johns Hopkins|Caltech|Carnegie Mellon|Vanderbilt|Rice|Georgetown|Notre Dame|NYU|Boston|Michigan|Virginia Commonwealth|Virginia Tech)\\b.*(University|Institute|College)"
  ),
  "United Kingdom" = c(
    "\\b(Oxford|Cambridge|London|Edinburgh|Manchester|Bristol|Birmingham|Glasgow|Leeds|Liverpool|Southampton|Durham|Warwick|Imperial|UCL|LSE|Kings College|Queen Mary)\\b.*(University|College)"
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
  ),
  "China" = c(
    "\\b(Peking|Tsinghua|Fudan|Shanghai Jiao Tong|Zhejiang|Nanjing)\\b.*(University|College)"
  ),
  "Japan" = c(
    "\\b(Tokyo|Kyoto|Osaka|Tohoku|Nagoya|Hokkaido)\\b.*(University|College)"
  ),
  "India" = c(
    "\\b(IIT|Indian Institute)"
  )
)

#' Extract basic metadata from DOI
get_basic_metadata <- function(doi) {
  tryCatch({
    work <- rcrossref::cr_works(doi = doi)$data

    # Handle empty results
    if (is.null(work) || nrow(work) == 0) {
      return(list(year = NA, journal = NA, issn = NA, issn_all = NA))
    }

    # Extract year
    year <- NA
    if (!is.null(work$published.print) && length(work$published.print) > 0 && !is.na(work$published.print)) {
      year <- as.numeric(substr(work$published.print, 1, 4))
    } else if (!is.null(work$published.online) && length(work$published.online) > 0 && !is.na(work$published.online)) {
      year <- as.numeric(substr(work$published.online, 1, 4))
    } else if (!is.null(work$issued) && length(work$issued) > 0 && !is.na(work$issued)) {
      year <- as.numeric(substr(work$issued, 1, 4))
    } else if (!is.null(work$created) && length(work$created) > 0 && !is.na(work$created)) {
      year <- as.numeric(substr(work$created, 1, 4))
    }

    # Extract journal and ISSN
    journal <- if (!is.null(work$container.title) && length(work$container.title) > 0) work$container.title else NA

    if (!is.null(work$issn) && length(work$issn) > 0 && !is.na(work$issn)) {
      issn_split <- strsplit(work$issn, ",")[[1]]
      issn_all <- trimws(issn_split)
      issn <- issn_all[1]
    } else {
      issn <- NA
      issn_all <- NA
    }

    list(year = year, journal = journal, issn = issn, issn_all = issn_all)
  }, error = function(e) {
    message("  Error retrieving metadata: ", e$message)
    list(year = NA, journal = NA, issn = NA, issn_all = NA)
  })
}

#' Extract country from affiliation text
extract_country_from_text <- function(text) {
  if (is.na(text) || text == "" || length(text) == 0) return(NA)

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

  # Method 2: USA/UK abbreviations and states
  if (grepl("\\b(USA|U\\.S\\.A\\.|United States)\\b", text, ignore.case = TRUE)) {
    return("United States")
  }

  for (state in us_states) {
    if (grepl(paste0("\\b", state, "\\b"), text, ignore.case = TRUE)) {
      return("United States")
    }
  }

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

  # Method 4: Full country name anywhere
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

    if (is.null(work) || nrow(work) == 0) return(NA)

    if (!is.null(work$author) && length(work$author) > 0) {
      authors <- work$author[[1]]

      if ("affiliation.name" %in% names(authors) && length(authors$affiliation.name) > 0) {
        for (aff in authors$affiliation.name) {
          if (!is.na(aff) && length(aff) > 0 && nchar(aff) > 0) {
            country <- extract_country_from_text(aff)
            if (!is.na(country)) return(country)
          }
        }
      }
    }
    return(NA)
  }, error = function(e) {
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
    if (!is.na(issn) && length(issn) > 0) {
      issn_clean <- gsub("-", "", issn)
      match <- top_factor_data %>%
        filter(gsub("-", "", Issn) == issn_clean | gsub("-", "", Eissn) == issn_clean)
      if (nrow(match) > 0) return(match$Total[1])
    }

    if (!is.na(journal_name) && length(journal_name) > 0) {
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
  results <- lapply(dois, function(doi) {
    tryCatch({
      extract_doi_metadata(doi)
    }, error = function(e) {
      message("  ERROR: ", e$message)
      data.frame(
        doi = doi,
        year = NA,
        journal = NA,
        issn = NA,
        sjr = NA,
        country = NA,
        top_factor = NA,
        stringsAsFactors = FALSE
      )
    })
  })
  bind_rows(results)
}
