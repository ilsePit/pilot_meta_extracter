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

## remove test rows and select only manually confirmed relevant studies
comb = comb %>%
  filter(doi != "test") %>%
  # rough selection
  filter(str_detect(X1.3..Is.there.a.statement.in.the.article.that.there.was.a.pilot.study.conducted.,
                    "^Yes,"))

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
# auto_extracted manual_extracted     n   perc
# <lgl>          <lgl>            <int>  <dbl>
# 1 FALSE          FALSE               10 0.154 
# 2 FALSE          TRUE                18 0.277 
# 3 TRUE           FALSE                2 0.0308
# 4 TRUE           TRUE                35 0.538

## inspect manually
fund_inspect = 
  comb_fund %>%
  filter(manual_extracted, auto_extracted)
## added value auto is the dois and easily readable funding body names etc

# compare COI info
comb_coi = 
  comb %>%
  select(doi, contains("coi"), contains("conflict"))

## add whether any statement or coding available at all (also includes "no coi" statements)
comb_coi <- comb_coi %>%
  mutate(
    auto_extracted = if_any(starts_with("coi"), ~ 
                              !is.na(.) & str_trim(.) != "" & !str_to_lower(.) %in% c("na", "n/a")
    ),
    manual_extracted = !is.na(.data[["X8.2.1..If.there.is.a.conflict.of.interest.statement..what.is.the.statement."]]) &
      str_trim(.data[["X8.2.1..If.there.is.a.conflict.of.interest.statement..what.is.the.statement."]]) != "" &
      !str_to_lower(.data[["X8.2.1..If.there.is.a.conflict.of.interest.statement..what.is.the.statement."]]) %in% c("na", "n/a")
  )

## check amount of manual coding errors / missing
comb_coi %>%
  rename(manual_does_include_coi = "X8.2..Does.the.article.include.a.statement.indicating.whether.there.were.any.conflicts.of.interest.") %>%
  group_by(manual_does_include_coi,
           manual_extracted) %>%
  summarise(n = n(),
            perc = n/nrow(comb_fund))

# manual_does_include_coi                                           manual_extracted     n   perc
# <chr>                                                             <lgl>            <int>  <dbl>
# 1 No, there is no conflict of interest statement                    FALSE               21 0.323 
# 2 No, there is no conflict of interest statement                    TRUE                 2 0.0308
# 3 Yes, the statement says that there are one or more conflicts of … FALSE                6 0.0923
# 4 Yes, the statement says that there are one or more conflicts of … TRUE                 4 0.0615
# 5 Yes, the statement says that there is no conflict of interest     FALSE               15 0.231 
# 6 Yes, the statement says that there is no conflict of interest     TRUE                17 0.262

## rough estimate of overlap in extractions
comb_coi %>%
  group_by(auto_extracted, manual_extracted) %>%
  summarise(n = n(),
            perc = n/nrow(comb_coi))
# auto_extracted manual_extracted     n   perc
# <lgl>          <lgl>            <int>  <dbl>
# 1 FALSE          FALSE               38 0.585 
# 2 FALSE          TRUE                21 0.323 
# 3 TRUE           FALSE                4 0.0615
# 4 TRUE           TRUE                 2 0.0308

coi_inspect = 
  comb_coi %>%
  filter(manual_extracted, auto_extracted)
# where they agree there is complete overlap