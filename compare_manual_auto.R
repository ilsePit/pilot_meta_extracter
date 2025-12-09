# combine automated and manual coding
library(stringr)


## read manual coding file
manual = read.csv("data/2025-12-09_Codingform_ PilotingAssessment_pilot.csv")

# make clean doi column
manual <- manual %>%
  mutate(
    doi = X1.2..Article.DOI |> 
      sub("^https?://doi\\.org/", "", x = _) |>
      sub("(?i)^doi:\\s*", "", x = _, perl = TRUE)
  )
# select dois with manual coding
dois = manual$doi
## remove "test" entries
dois = dois[dois != "test"]
## keep only unique entries
dois = unique(dois)

# extract automated coding
source("extract_doi_metadata.R")

auto <- process_dois(dois, return_location_details = FALSE)

# combine files 
comb = full_join(
  auto, 
  manual,
  by = "doi"
)

write.csv(comb, "auto-and-manual-combined.csv", row.names = FALSE)

## remove test rows
comb = comb %>%
  filter(doi != "test")

# compare funding info
comb_fund = 
  comb %>%
  select(doi, contains("fund"))

## add whether any statement or coding available at all (also includes "no funding" statements)
comb_fund <- comb_fund %>%
  mutate(
    auto_extracted = if_any(starts_with("funding"), ~ 
                              !is.na(.) & str_trim(.) != "" & !str_to_lower(.) %in% c("na", "n/a")
    ),
    manual_extracted = !is.na(.data[["X8.1..Does.the.article.include.a.funding.statement."]]) &
      str_trim(.data[["X8.1..Does.the.article.include.a.funding.statement."]]) != "" &
      !str_to_lower(.data[["X8.1..Does.the.article.include.a.funding.statement."]]) %in% c("na", "n/a")
  )

## rough estimate of overlap in extractions
comb_fund %>%
  group_by(auto_extracted, manual_extracted) %>%
  summarise(n = n(),
            perc = n/nrow(comb_fund))
# auto_extracted manual_extracted     n  perc
# <lgl>          <lgl>            <int> <dbl>
# 1 FALSE          FALSE               48 0.291
# 2 FALSE          TRUE                18 0.109
# 3 TRUE           FALSE               64 0.388
# 4 TRUE           TRUE                35 0.212

## inspect manually
fund_inspect = 
  comb_fund %>%
  filter(manual_extracted, auto_extracted)
## overlap but by no means exhaustive

# compare COI info
comb_coi = 
  comb %>%
  select(doi, contains("coi"), contains("conflict"))

# FIXME

