source("extract_doi_metadata.R")

# Test DOIs - diverse set
test_dois <- c(
  # Original psychology papers
  "10.1177/1745691620950684",  # Perspectives on Psychological Science
  "10.1111/bjso.12804",         # British Journal of Social Psychology
  "10.1037/pspi0000504",        # Journal of Personality and Social Psychology
  "10.1027/1864-9335/a000535",  # Social Psychology
  # New diverse papers
  "10.3917/phimag.153.0008",    # French journal
  "10.7146/ln.v0i1.19000",      # Danish journal
  "10.1166/asl.2017.8790",      # Advanced Science Letters
  "10.21203/rs.3.rs-5261491/v1", # Preprint
  "10.1136/bmj.3.5932.655",     # BMJ (old paper)
  "10.4324/9781003398127-3"     # Book chapter
)

message("========================================")
message("Extended Test with ", length(test_dois), " DOIs")
message("========================================")

results <- process_dois(test_dois)

message("\n========================================")
message("RESULTS")
message("========================================\n")
print(results)

write.csv(results, "extended_test_results.csv", row.names = FALSE)

# Summary
message("\n========================================")
message("SUMMARY STATISTICS")
message("========================================")
cat(sprintf("Total DOIs: %d\n", nrow(results)))
cat(sprintf("Years found: %d (%.0f%%)\n",
            sum(!is.na(results$year)),
            100 * mean(!is.na(results$year))))
cat(sprintf("Journals found: %d (%.0f%%)\n",
            sum(!is.na(results$journal)),
            100 * mean(!is.na(results$journal))))
cat(sprintf("SJR found: %d (%.0f%%)\n",
            sum(!is.na(results$sjr)),
            100 * mean(!is.na(results$sjr))))
cat(sprintf("Countries found: %d (%.0f%%)\n",
            sum(!is.na(results$country)),
            100 * mean(!is.na(results$country))))
cat(sprintf("TOP Factor found: %d (%.0f%%)\n",
            sum(!is.na(results$top_factor)),
            100 * mean(!is.na(results$top_factor))))
