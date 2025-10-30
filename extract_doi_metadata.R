# DOI Metadata Extraction ------------------------------------------------------
# Focuses on OpenAlex for all metadata, journal metrics, and location data.

if (!require("pacman")) install.packages("pacman")
if (!require("pak", quietly = TRUE)) install.packages("pak")
if (!require("sjrdata", quietly = TRUE)) pak::pak("ikashnitsky/sjrdata")
pacman::p_load(
  httr,
  countrycode,
  dplyr,
  sjrdata
)

# Optional TOP Factor data -----------------------------------------------------
if (file.exists("top_factor_data.RData")) {
  load("top_factor_data.RData")
} else {
  message("TOP Factor data not found. Run download_top_factor.R first")
  top_factor_data <- NULL
}

# Constants -------------------------------------------------------------------
OA_USER_AGENT <- "mailto:test@example.com"
data("sjr_journals", package = "sjrdata")

# Utility helpers -------------------------------------------------------------
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

safe_country_name <- function(iso2) {
  if (is.null(iso2) || length(iso2) == 0) return(NA_character_)
  tryCatch(
    countrycode::countrycode(toupper(iso2), origin = "iso2c", destination = "country.name"),
    error = function(e) NA_character_,
    warning = function(w) NA_character_
  )
}

as_numeric_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

is_generic_institution <- function(name) {
  if (is.null(name) || length(name) == 0) return(TRUE)
  normalized <- tolower(trimws(name))
  if (normalized == "") return(TRUE)
  if (grepl("(university|universit|college|institute|academy|hospital|centre for|center for)", normalized)) {
    return(FALSE)
  }
  grepl(
    "^(department|dept\\.|school|faculty|centre|center|division|unit|program|programme|clinic|laboratory|lab|research group|office)\\b",
    normalized
  )
}

format_location_label <- function(location) {
  if (is.null(location)) return(NA_character_)
  pieces <- Filter(
    function(x) !is.null(x) && length(x) > 0 && !is.na(x) && trimws(x) != "",
    c(
      location$display_name,
      location$city,
      location$region,
      location$country
    )
  )
  pieces <- unique(pieces)
  if (length(pieces) == 0) return(NA_character_)
  paste(pieces, collapse = ", ")
}

location_country <- function(location) {
  if (is.null(location)) return(NA_character_)
  value <- location$country %||% NA_character_
  if (is.na(value) || !nzchar(value)) return(NA_character_)
  value
}

# OpenAlex requests -----------------------------------------------------------
fetch_openalex_work <- function(doi) {
  url <- paste0("https://api.openalex.org/works/doi:", doi)
  response <- tryCatch(
    httr::GET(url, httr::add_headers(`User-Agent` = OA_USER_AGENT)),
    error = function(e) NULL
  )
  if (is.null(response) || httr::status_code(response) != 200) {
    warning(sprintf("OpenAlex work fetch failed for %s (status %s)", doi, httr::status_code(response %||% NA)))
    return(NULL)
  }
  tryCatch(httr::content(response, as = "parsed"), error = function(e) NULL)
}

normalize_author_id <- function(author_id) {
  if (is.null(author_id) || length(author_id) == 0 || is.na(author_id)) return(NA_character_)
  if (startsWith(author_id, "https://openalex.org/")) {
    sub("https://openalex.org/", "", author_id, fixed = TRUE)
  } else {
    author_id
  }
}

fetch_author_profile <- function(author_id) {
  author_id <- normalize_author_id(author_id)
  if (is.na(author_id)) return(NULL)
  url <- paste0("https://api.openalex.org/authors/", author_id)
  response <- tryCatch(
    httr::GET(url, httr::add_headers(`User-Agent` = OA_USER_AGENT)),
    error = function(e) NULL
  )
  if (is.null(response) || httr::status_code(response) != 200) return(NULL)
  tryCatch(httr::content(response, as = "parsed"), error = function(e) NULL)
}

# Location handling -----------------------------------------------------------
build_location_details <- function(inst, source) {
  if (is.null(inst)) return(NULL)
  geo <- inst$geo %||% list()
  iso2 <- inst$country_code %||% NA_character_
  list(
    id = inst$id %||% NA_character_,
    display_name = inst$display_name %||% NA_character_,
    type = inst$type %||% NA_character_,
    country_code = if (!is.null(iso2)) toupper(iso2) else NA_character_,
    country = safe_country_name(iso2),
    city = geo$city %||% NA_character_,
    region = geo$region %||% NA_character_,
    latitude = as_numeric_or_na(geo$latitude),
    longitude = as_numeric_or_na(geo$longitude),
    source = source
  )
}

select_institution_from_list <- function(institutions, source) {
  if (is.null(institutions) || length(institutions) == 0) return(NULL)
  for (inst in institutions) {
    name <- inst$display_name %||% ""
    if (is_generic_institution(name)) next
    location <- build_location_details(inst, source)
    if (!is.null(location)) return(location)
  }
  NULL
}

get_first_authorship <- function(authorships) {
  if (is.null(authorships) || length(authorships) == 0) return(NULL)
  for (authorship in authorships) {
    if ((authorship$author_position %||% NA_character_) == "first") return(authorship)
  }
  authorships[[1]]
}

get_last_authorship <- function(authorships) {
  if (is.null(authorships) || length(authorships) == 0) return(NULL)
  for (i in seq_along(authorships)) {
    idx <- length(authorships) - i + 1
    authorship <- authorships[[idx]]
    if ((authorship$author_position %||% NA_character_) == "last") return(authorship)
  }
  authorships[[length(authorships)]]
}

profile_institution_candidates <- function(profile) {
  candidates <- list()
  if (!is.null(profile$last_known_institution)) {
    candidates <- c(candidates, list(profile$last_known_institution))
  }
  if (!is.null(profile$last_known_institutions) && length(profile$last_known_institutions) > 0) {
    candidates <- c(candidates, profile$last_known_institutions)
  }
  if (!is.null(profile$affiliations) && length(profile$affiliations) > 0) {
    for (aff in profile$affiliations) {
      inst <- aff$institution %||% aff
      candidates <- c(candidates, list(inst))
    }
  }
  Filter(function(x) !is.null(x), candidates)
}

get_article_location_details <- function(work) {
  authorships <- work$authorships %||% list()
  first_authorship <- get_first_authorship(authorships)
  if (is.null(first_authorship)) return(NULL)
  select_institution_from_list(first_authorship$institutions, "article")
}

get_author_profile_location <- function(authorship) {
  if (is.null(authorship)) return(NULL)
  profile <- fetch_author_profile(authorship$author$id)
  if (is.null(profile)) return(NULL)
  candidates <- profile_institution_candidates(profile)
  for (inst in candidates) {
    name <- inst$display_name %||% ""
    if (is_generic_institution(name)) next
    location <- build_location_details(inst, "author-profile")
    if (!is.null(location)) return(location)
  }
  NULL
}

get_author_location_details <- function(work, article_location) {
  authorships <- work$authorships %||% list()
  first <- get_first_authorship(authorships)
  last <- get_last_authorship(authorships)

  location <- get_author_profile_location(first)
  if (is.null(location)) {
    # Avoid double-fetching if first and last are same person
    if (!identical(first, last)) {
      location <- get_author_profile_location(last)
    }
  }

  if (is.null(location)) {
    if (!is.null(article_location)) {
      article_location$source <- paste0(article_location$source, "|fallback")
      location <- article_location
    } else {
      location <- list(
        id = NA_character_,
        display_name = NA_character_,
        type = NA_character_,
        country_code = NA_character_,
        country = NA_character_,
        city = NA_character_,
        region = NA_character_,
        latitude = NA_real_,
        longitude = NA_real_,
        source = "missing"
      )
    }
  }
  location
}

# Metadata helpers ------------------------------------------------------------
get_basic_metadata <- function(work) {
  source <- work$primary_location$source %||% list()
  if (length(source) == 0) {
    source <- work$primary_location %||% list()
  }
  if (length(source) == 0) {
    source <- work$host_venue %||% list()
  }

  issn_candidates <- source$issn %||% character()
  if (length(issn_candidates) > 0 && is.list(issn_candidates)) {
    issn_candidates <- unlist(issn_candidates, use.names = FALSE)
  }
  issn_candidates <- issn_candidates[!is.na(issn_candidates) & nzchar(issn_candidates)]
  if (length(issn_candidates) == 0) {
    issn_candidates <- source$issn_l %||% character()
  }
  issn_candidates <- unique(issn_candidates)

  list(
    year = work$publication_year %||% NA_integer_,
    journal = source$display_name %||% NA_character_,
    issn_primary = if (length(issn_candidates) > 0) issn_candidates[1] else NA_character_,
    issn_all = issn_candidates
  )
}

get_journal_sjr <- function(issn_all, year) {
  if (is.null(issn_all) || length(issn_all) == 0 || is.na(year)) return(NA_real_)
  tryCatch({
    for (issn in issn_all) {
      if (is.na(issn)) next
      issn_clean <- gsub("[^0-9]", "", issn)
      if (issn_clean == "") next

      journal_data <- sjr_journals %>%
        filter(year == !!year) %>%
        filter(grepl(issn_clean, gsub("[^0-9,]", "", issn)))
      if (nrow(journal_data) > 0) return(journal_data$sjr[1])

      journal_data <- sjr_journals %>%
        filter(grepl(issn_clean, gsub("[^0-9,]", "", issn))) %>%
        mutate(year_diff = abs(year - !!year)) %>%
        arrange(year_diff)
      if (nrow(journal_data) > 0) return(journal_data$sjr[1])
    }
    NA_real_
  }, error = function(e) NA_real_)
}

get_top_factor <- function(journal_name, issn) {
  if (is.null(top_factor_data)) return(NA_real_)
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
    NA_real_
  }, error = function(e) NA_real_)
}

append_location_columns <- function(record, prefix, location) {
  record[[paste0(prefix, "_id")]] <- location$id
  record[[paste0(prefix, "_display_name")]] <- location$display_name
  record[[paste0(prefix, "_type")]] <- location$type
  record[[paste0(prefix, "_country_code")]] <- location$country_code
  record[[paste0(prefix, "_country")]] <- location$country
  record[[paste0(prefix, "_city")]] <- location$city
  record[[paste0(prefix, "_region")]] <- location$region
  record[[paste0(prefix, "_latitude")]] <- location$latitude
  record[[paste0(prefix, "_longitude")]] <- location$longitude
  record[[paste0(prefix, "_source")]] <- location$source
  record
}

# Public API ------------------------------------------------------------------
extract_doi_metadata <- function(doi, return_location_details = TRUE) {
  message("Processing ", doi)
  work <- fetch_openalex_work(doi)
  if (is.null(work)) {
    empty <- data.frame(
      doi = doi,
      year = NA_integer_,
      journal = NA_character_,
      issn = NA_character_,
      sjr = NA_real_,
      top_factor = NA_real_,
      article_location = NA_character_,
      author_location = NA_character_,
      primary_topic_display_name = NA_character_,
      primary_topic_id = NA_character_,
      stringsAsFactors = FALSE
    )
    empty$primary_topic <- I(list(NULL))
    return(empty)
  }

  basic <- get_basic_metadata(work)
  sjr <- get_journal_sjr(basic$issn_all, basic$year)
  top_factor <- get_top_factor(basic$journal, basic$issn_primary)
  article_location <- get_article_location_details(work)
  author_location <- get_author_location_details(work, article_location)
  article_country <- location_country(article_location)
  author_country <- location_country(author_location)
  article_label <- format_location_label(article_location)
  author_label <- format_location_label(author_location)

  record <- data.frame(
    doi = doi,
    year = basic$year,
    journal = basic$journal,
    issn = basic$issn_primary,
    sjr = sjr,
    top_factor = top_factor,
    article_location = article_country,
    author_location = author_country,
    primary_topic_display_name = work$primary_topic$display_name %||% NA_character_,
    primary_topic_id = work$primary_topic$id %||% NA_character_,
    stringsAsFactors = FALSE
  )
  record$primary_topic <- I(list(work$primary_topic %||% NULL))

  if (return_location_details) {
    record$article_location_label <- article_label
    record$author_location_label <- author_label

    if (is.null(article_location)) {
      article_location <- list(
        id = NA_character_,
        display_name = NA_character_,
        type = NA_character_,
        country_code = NA_character_,
        country = NA_character_,
        city = NA_character_,
        region = NA_character_,
        latitude = NA_real_,
        longitude = NA_real_,
        source = NA_character_
      )
    }
    if (is.null(author_location)) {
      author_location <- list(
        id = NA_character_,
        display_name = NA_character_,
        type = NA_character_,
        country_code = NA_character_,
        country = NA_character_,
        city = NA_character_,
        region = NA_character_,
        latitude = NA_real_,
        longitude = NA_real_,
        source = NA_character_
      )
    }
    record <- append_location_columns(record, "article_location_detail", article_location)
    record <- append_location_columns(record, "author_location_detail", author_location)
  }

  record
}

process_dois <- function(dois, return_location_details = TRUE) {
  results <- lapply(dois, function(doi) {
    tryCatch(
      extract_doi_metadata(doi, return_location_details = return_location_details),
      error = function(e) {
        warning(sprintf("Extraction failed for %s: %s", doi, e$message))
        fallback <- data.frame(
          doi = doi,
          year = NA_integer_,
          journal = NA_character_,
          issn = NA_character_,
          sjr = NA_real_,
          top_factor = NA_real_,
          article_location = NA_character_,
          author_location = NA_character_,
          primary_topic_display_name = NA_character_,
          primary_topic_id = NA_character_,
          stringsAsFactors = FALSE
        )
        fallback$primary_topic <- I(list(NULL))
        fallback
      }
    )
  })
  bind_rows(results)
}
