# DOI Metadata Extraction Scripts

Comprehensive R scripts for extracting publication metadata from DOIs, including journal metrics and transparency scores.

## Features

The scripts extract the following information for each DOI:

1. **Publication Year** - Year the article was published
2. **Journal Name** - Full journal title
3. **ISSN** - International Standard Serial Number
4. **SJR (Scimago Journal Rank)** - Journal impact metric (proxy for Impact Factor)
5. **Corresponding Author Country** - Extracted from first author's affiliation
6. **TOP Factor** - Transparency and Openness Promotion score (0-29) from COS

## Files

### Main Scripts

- **[doi_metadata_complete.R](doi_metadata_complete.R)** - Complete extraction script with all features
- **[doi_quick_test.R](doi_quick_test.R)** - Quick test script (without SJR/TOP Factor)
- **[download_top_factor.R](download_top_factor.R)** - Download TOP Factor data from OSF

### Helper Scripts

- **[doi_metadata_extractor.R](doi_metadata_extractor.R)** - Original full-featured version
- **[top_factor_manual_setup.R](top_factor_manual_setup.R)** - Manual TOP Factor data setup

## Quick Start

### 1. Install Required Packages

```r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rcrossref, sjrdata, countrycode, httr, rjson, dplyr, stringr)
```

### 2. Download TOP Factor Data (One-Time Setup)

```r
source("download_top_factor.R")
```

This downloads the TOP Factor dataset from the Center for Open Science (COS) OSF repository.

### 3. Run the Complete Extraction

```r
source("doi_metadata_complete.R")
```

Or process your own DOIs:

```r
source("doi_metadata_complete.R")

# Your DOIs
my_dois <- c(
  "10.xxxx/xxxxx",
  "10.yyyy/yyyyy"
)

# Process them
results <- process_multiple_dois(my_dois)

# Save results
write.csv(results, "my_doi_results.csv", row.names = FALSE)
```

## Test Results

The script was tested with four DOIs:

| DOI | Year | Journal | SJR | Country | TOP Factor |
|-----|------|---------|-----|---------|------------|
| 10.1177/1745691620950684 | 2021 | Perspectives on Psychological Science | 4.563 | NA | 0 |
| 10.1111/bjso.12804 | 2025 | British Journal of Social Psychology | 1.665 | France | 10 |
| 10.1037/pspi0000504 | 2025 | Journal of Personality and Social Psychology | 3.865 | NA | 20 |
| 10.1027/1864-9335/a000535 | 2023 | Social Psychology | 0.668 | United States | 11 |

## Data Sources

1. **Basic Metadata (Year, Journal, ISSN)** - [CrossRef API](https://www.crossref.org/) via `rcrossref` package
2. **SJR Rankings** - [Scimago Journal Rank](https://www.scimagojr.com/) via `sjrdata` package
3. **TOP Factor** - [Center for Open Science](https://www.cos.io/initiatives/top-guidelines) via OSF (https://osf.io/qatkz/)
4. **Country Extraction** - Affiliation parsing with `countrycode` package

## Important Notes

### Impact Factor vs SJR

- **Impact Factor** (IF) from Clarivate/Web of Science requires paid subscription
- **SJR** (Scimago Journal Rank) is used as a free alternative
- SJR is calculated using Scopus data and correlates well with Impact Factor
- Both metrics measure journal citation impact

### TOP Factor Scoring

- TOP Factor scores range from **0 to 29**
- Measures journal policies on transparency and open science
- Higher scores indicate stronger commitments to:
  - Data citation and transparency
  - Code/materials sharing
  - Preregistration
  - Replication studies
  - Open access

### Country Extraction Limitations

- Based on first author's affiliation (often the corresponding author)
- Some affiliations don't include country information
- Extraction uses multiple methods:
  1. Country name at end of affiliation string
  2. Pattern matching for country names
  3. Recognition of USA/UK variations

### SJR Data Availability

- SJR data via `sjrdata` package is updated periodically
- Current data goes through 2024
- For 2025 publications, the script uses the closest available year (2024)
- Some journals may not be in the SJR database

## Troubleshooting

### TOP Factor Data Not Found

If you see "TOP Factor data not found", run:

```r
source("download_top_factor.R")
```

This creates `top_factor_data.RData` in your working directory.

### SJR Not Found

Some journals may not be indexed in Scimago. The script will return `NA` for these cases.

### Country Not Found

Many CrossRef records don't include affiliation data, or affiliations don't specify countries. The script returns `NA` when country cannot be determined.

### Rate Limiting

The script includes 0.5 second delays between API calls to respect CrossRef's rate limits. For large batches, consider increasing this delay.

## Customization

### Using Impact Factor Instead of SJR

If you have access to Clarivate/Web of Science, you can replace the SJR function with Impact Factor lookup. The script structure supports this modification.

### Adding More Metrics

The modular design allows easy addition of new metrics:

```r
get_new_metric <- function(doi) {
  # Your custom metric extraction
  return(metric_value)
}

# Add to extract_doi_metadata() function
new_metric <- get_new_metric(doi)
```

## Citation

If you use these scripts in your research, please cite:

- **CrossRef**: https://www.crossref.org/
- **Scimago Journal Rank**: https://www.scimagojr.com/
- **TOP Guidelines**: https://www.cos.io/initiatives/top-guidelines

## License

These scripts are provided as-is for academic and research purposes.

## Contact

For issues or questions, please refer to the package documentation:
- `rcrossref`: https://github.com/ropensci/rcrossref
- `sjrdata`: https://github.com/ikashnitsky/sjrdata
