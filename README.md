# DOI Metadata Extractor

Minimal R scripts for extracting publication metadata from DOIs with complete reliance on OpenAlex for article details, institution locations, and author affiliations.

## What you get

For each DOI, `process_dois()` returns:
- Publication year and journal title (from OpenAlex host venue metadata)
- ISSN (primary plus optional vector of alternates)
- Scimago Journal Rank (matched on ISSN/year via `sjrdata`)
- TOP Factor (if `top_factor_data.RData` has been downloaded)
- The OpenAlex `primary_topic` field (full object, plus display name and id helpers)
- Two country columns:
  - `article_location` – country of the article-level institution (first author affiliation)
  - `author_location` – country of the author-profile institution (first, then last author fallback)
- Optional detailed columns describing both locations (`return_location_details = TRUE`)

## Quick start

```r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr, countrycode, dplyr, sjrdata)

source("download_top_factor.R")   # one-time setup (optional but recommended)
source("extract_doi_metadata.R")

my_dois <- c(
  "10.1037/amp0001385",
  "10.1037/met0000351",
  "10.1146/annurev-psych-020821-114157"
)

results <- process_dois(my_dois, return_location_details = FALSE)
write.csv(results, "results.csv", row.names = FALSE)
```

## Suggested manual test

After sourcing `extract_doi_metadata.R`, run:

```r
test_dois <- c(
  "10.1037/amp0001385",
  "10.1037/met0000351",
  "10.3389/fpsyg.2022.896741"
)

process_dois(test_dois, return_location_details = TRUE)
```

Expect both `article_location` and `author_location` to be populated with country names, with `author_location_detail_source` showing `author-profile` when profile data was available; `article_location_label` and `author_location_label` (present when `return_location_details = TRUE`) contain the full institution strings.

## How it works

1. **OpenAlex Works API** supplies publication metadata, host venue, location-rich authorship information, and the primary topic.
2. **Institution choice** – the first author’s article-level institution is treated as the canonical article location; generic department names are skipped.
3. **Author affiliation** – the first author’s profile is queried for current or most recent institutions, backed off to the last author, then to article metadata when necessary.
4. **Metrics** – SJR values are pulled from `sjrdata`, while TOP Factor scores are matched against the optional download file.

## Repository layout

- `extract_doi_metadata.R` – core extraction and helper routines.
- `download_top_factor.R` – helper for retrieving the TOP Factor dataset (creates `top_factor_data.RData`).
- `top_factor_manual_setup.R` – fallback instructions if automated download fails.

## Requirements

```r
httr        # OpenAlex queries
countrycode # ISO-to-name conversion
dplyr       # light data manipulation
sjrdata     # Scimago Journal Rank reference data
```

## Citation

Please cite all data sources you rely on:
- **OpenAlex**: https://openalex.org/
- **Scimago Journal Rank**: https://www.scimagojr.com/
- **Center for Open Science (TOP Factor)**: https://www.cos.io/initiatives/top-guidelines

## License

MIT License – free for academic and commercial use.
