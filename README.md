# DOI Metadata Extractor

R script for extracting comprehensive publication metadata from DOIs for systematic reviews and meta-analyses.

## What it extracts

For each DOI, get:
- **Publication year** and **journal name**
- **ISSN**
- **SJR** (Scimago Journal Rank) - free alternative to Impact Factor
- **Corresponding author country** - extracted from affiliations (40-75% coverage depending on data availability)
- **TOP Factor** - journal transparency score (0-29) from Center for Open Science

## Quick Start

### 1. Install packages

```r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rcrossref, sjrdata, countrycode, httr, rjson, dplyr, stringr)
```

### 2. Download TOP Factor data (one-time)

```r
source("download_top_factor.R")
```

### 3. Extract metadata

```r
source("extract_doi_metadata.R")

# Your DOIs
my_dois <- c(
  "10.1177/1745691620950684",
  "10.1111/bjso.12804",
  "10.1037/pspi0000504"
)

# Extract metadata
results <- process_dois(my_dois)

# Save results
write.csv(results, "results.csv", row.names = FALSE)
```

## Example output

Tested with 10 diverse DOIs (psychology, medicine, philosophy, Danish, French, Indonesian journals, preprint, book chapter, old paper from 1974):

```
Total DOIs: 10
Years found: 10 (100%)
Journals found: 9 (90%)
SJR found: 5 (50%)
Countries found: 4 (40%)
TOP Factor found: 4 (40%)
```

Sample results:

| DOI | Year | Journal | SJR | Country | TOP |
|-----|------|---------|-----|---------|-----|
| 10.1177/1745691620950684 | 2021 | Perspectives on Psychological Science | 4.563 | United States | 0 |
| 10.1111/bjso.12804 | 2025 | British Journal of Social Psychology | 1.665 | France | 10 |
| 10.1136/bmj.3.5932.655 | 1974 | BMJ | NA | NA | NA |
| 10.7146/ln.v0i1.19000 | 1994 | LexicoNordica | NA | NA | NA |

## How it works

### Country extraction

Uses pattern matching to identify countries from affiliation text:
- ✅ US state names and abbreviations
- ✅ Major universities (Harvard, Oxford, Peking, etc.)
- ✅ Country names and common abbreviations (USA, UK, etc.)
- ✅ Checks all authors (not just first)

**Coverage:** 40-75% depending on field and data availability in CrossRef

**Limitations:**
- Some affiliations don't include country information
- Some DOIs have no affiliation data
- Non-standard institution names may be missed

### Journal metrics

- **SJR (Scimago Journal Rank)**: Free, publicly available via `sjrdata` R package
- **Impact Factor**: Requires expensive subscription - not included
- **TOP Factor**: Free, downloaded from Center for Open Science

### Data sources

- **CrossRef API** - Publication metadata
- **Scimago** - Journal rankings
- **Center for Open Science** - TOP Factor scores

## Files

- `extract_doi_metadata.R` - Main script
- `download_top_factor.R` - Download TOP Factor data
- `top_factor_manual_setup.R` - Manual download instructions (if needed)
- `test_extended.R` - Test script with 10 diverse DOIs

## Extending

Add institution patterns for better coverage in your field:

```r
# Edit country_patterns in extract_doi_metadata.R
country_patterns[["Brazil"]] <- c(
  "\\b(USP|UNICAMP|UFRJ)\\b.*(University|Universidade)"
)
```

## Limitations

- **Country:** ~25-60% missing depending on CrossRef data quality
- **SJR:** Not all journals indexed in Scimago
- **TOP Factor:** Only evaluated journals included
- **Metadata quality:** Depends on what publishers provide to CrossRef

## Requirements

```r
rcrossref      # CrossRef API
sjrdata        # SJR rankings
countrycode    # Country standardization
httr           # HTTP requests
rjson          # JSON parsing
dplyr          # Data manipulation
stringr        # String operations
```

## Citation

If you use this in your research, cite the data sources:
- **CrossRef**: https://www.crossref.org/
- **Scimago**: https://www.scimagojr.com/
- **TOP Guidelines**: Nosek et al. (2015). Promoting an open research culture. *Science*, 348(6242), 1422-1425.

## License

MIT License - Free for academic and commercial use.
