library(readr)
library(magrittr)
library(dplyr)
library(stringr)
library(progress)

# PatientLevelPrediction::viewMultiplePlp("~/Downloads/CoagulopathyV2/CoagulopathyV2ResultsCCAE/")

CCAE <- read_csv("cov_ccae.csv")
MDCR <- read_csv("cov_mdcr.csv")

# filter covariate data
CCAE_filtered <- CCAE %>%
  filter(covariateValue != 0) %>%
  filter(str_detect(covariateName, 'drug_era group')) %>%
  select(conceptId, covariateName)
CCAE_filtered$covariateName <- str_trim(as.character(regmatches(CCAE_filtered$covariateName, gregexpr("(?<=:).*",CCAE_filtered$covariateName,perl=TRUE))),
                                        side = "left")
CCAE_filtered <-  CCAE_filtered %>%
  distinct()

# extract ancestors with separation=1
concept_ancestor <- read_tsv("~/Downloads/vocabulary_rxnorm/CONCEPT_ANCESTOR.csv")
concept_ancestor_sep <- concept_ancestor %>%
  filter((min_levels_of_separation == 1 & max_levels_of_separation == 1) |
         (min_levels_of_separation == 2 & max_levels_of_separation == 2) |
         (min_levels_of_separation == 3 & max_levels_of_separation == 3) |
         (min_levels_of_separation == 4 & max_levels_of_separation == 4) |
         (min_levels_of_separation == 5 & max_levels_of_separation == 5)) %>%
  mutate(separation = min_levels_of_separation) %>%
  select(-max_levels_of_separation, -min_levels_of_separation)

# (min_levels_of_separation == 6 & max_levels_of_separation == 6) |
#   (min_levels_of_separation == 7 & max_levels_of_separation == 7) |
#   (min_levels_of_separation == 8 & max_levels_of_separation == 8) |
#   (min_levels_of_separation == 9 & max_levels_of_separation == 9) |
#   (min_levels_of_separation == 10 & max_levels_of_separation == 10)

concept_reference <- read_tsv("~/Downloads/vocabulary_rxnorm/CONCEPT.csv",
                              col_types = cols_only('concept_id' = col_integer(), #col1 is integer
                                                    'concept_name' = 'c',           #col2 is character
                                                    'domain_id' = 'c',
                                                    'standard_concept' = 'c',
                                                    'invalid_reason' = 'c'),
                              quote = "")

concept_reference_clean <- concept_reference %>%
  select(concept_id, concept_name, domain_id) %>%
  filter(domain_id != "Metadata")

CCAE_filtered$roll_up_aggregate <- FALSE

pb <- progress_bar$new(total = 10)

# CAUTION MAGIG NUMBER
for(j in 1:10){
  CCAE_filtered[, paste0("roll_up_", j)] <- FALSE

  for(i in 1:nrow(CCAE_filtered)) {
    # for(j in 1:nrow(concept_ancestor_sep_1)) {
    # test <- concept_ancestor_sep_1 %>%
    #   filter(descendant_concept_id == CCAE_filtered$conceptId[i])
    test <- concept_ancestor_sep %>%
      filter(separation == j & descendant_concept_id == CCAE_filtered$conceptId[i])

    test <- left_join(test, concept_reference_clean, by = c("ancestor_concept_id"="concept_id"))

    if(any(test$ancestor_concept_id %in% CCAE_filtered$conceptId)){
      CCAE_filtered[i, paste0("roll_up_", j)] <- TRUE

      # set aggregate to true
      CCAE_filtered$roll_up_aggregate[i] <- TRUE
    }
    # setTxtProgressBar(pb,i)
    # }
  }
  pb$tick()
}

reduced_predictors <- CCAE_filtered %>%
  filter(roll_up_aggregate == FALSE) %>%
  select(conceptId, covariateName)

