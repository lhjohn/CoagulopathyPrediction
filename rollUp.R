library(readr)
library(magrittr)
library(dplyr)
library(stringr)

# PatientLevelPrediction::viewMultiplePlp("~/Downloads/CoagulopathyV2/CoagulopathyV2ResultsCCAE/")

CCAE <- read_csv("cov_ccae.csv")
MDCR <- read_csv("cov_mdcr.csv")

# CCAE_filtered <- CCAE %>%
#   filter(covariateValue != 0)
# MDCR_filtered <- MDCR %>%
#   filter(covariateValue != 0)
# 
# inner_join_filtered <- select(CCAE_filtered, c(covariateName, covariateId,
#                                                covariateValue)) %>%
#   inner_join(select(MDCR_filtered, c(covariateId, covariateValue)),
#              by = "covariateId")
# 
# inner_join <- CCAE %>%
#   inner_join(MDCR, by = "covariateId")

#------------------------------------------------------------------------------#

# library(EloRating)
# elo_data <- data.frame(Date = c("2020-04-10", "2021-04-10", "2021-04-11",
#                                 "2021-04-12", "2021-04-13", "2021-04-14"),
#                        winner = c("d", "k", "n", "k", "c", "n"),
#                        loser = c("w", "w", "z", "n", "g", "g"),
#                        Draw = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
# 
# seqcheck(winner = elo_data$winner, loser = elo_data$loser, Date = elo_data$Date,
#          draw = elo_data$Draw)
# 
# res <- elo.seq(winner = elo_data$winner, loser = elo_data$loser,
#                Date = elo_data$Date, draw = elo_data$Draw, runcheck = TRUE)
# summary(res)
# 
# extract_elo(res, extractdate = "2021-04-11")
# extract_elo(res)
# eloplot(res)
#------------------------------------------------------------------------------#
concept_ancestor <- read_tsv("~/Downloads/vocabulary_download_v5_{ef9e6dc9-3661-4899-9f4a-e37a5f48d186}_1618481290985/CONCEPT_ANCESTOR.csv")

CCAE_filtered <- CCAE %>%
  filter(covariateValue != 0) %>%
  filter(str_detect(covariateName, 'condition_era group')) %>%
  select(conceptId, covariateName)

# filter name field
CCAE_filtered$covariateName <- str_trim(as.character(regmatches(CCAE_filtered$covariateName, gregexpr("(?<=:).*",CCAE_filtered$covariateName,perl=TRUE))),
                                        side = "left")

CCAE_filtered <-  CCAE_filtered %>%
  distinct()

complete_hierarchy <- concept_ancestor %>%
  left_join(CCAE_filtered, by = c("descendant_concept_id" = "conceptId")) # %>%

#--------------------------- separation of 1 ----------------------------------#

zeroth_roll_up <- complete_hierarchy %>%
  filter(!is.na(covariateName)) %>%
  filter(min_levels_of_separation == 1 & max_levels_of_separation == 1)

zeroth_roll_up <- zeroth_roll_up %>%
  select(-min_levels_of_separation, -max_levels_of_separation)

joined_succ_temp <- zeroth_roll_up %>%
  left_join(zeroth_roll_up, by = c("descendant_concept_id" = "ancestor_concept_id")) %>%
  filter(!is.na(descendant_concept_id.y))

joined_all_temp <- left_join(zeroth_roll_up, zeroth_roll_up, by = c("descendant_concept_id" = "ancestor_concept_id"))

# Remove rows of concepts that were successfully rolled up. If there are multiple
# ancestors to a concept, only the matching one will be rolled up, the rest will
# be marked as rolledUp = TRUE to indicate that rolling up is possible, but not
# required anymore.
first_roll_up <- joined_all_temp %>%
  anti_join(joined_succ_temp, by = c("descendant_concept_id" = "descendant_concept_id.y", "ancestor_concept_id" = "descendant_concept_id"))

# mark as rolled up as described above
rolled_up <- first_roll_up$descendant_concept_id %in% first_roll_up$descendant_concept_id.y
first_roll_up$rolled_up <- rolled_up
#--------------------------- separation of 2 ----------------------------------#

complete_hierarchy_2 <- concept_ancestor %>%
  left_join(first_roll_up, by = c("descendant_concept_id" = "ancestor_concept_id")) # %>%


second_roll_up <- complete_hierarchy_2 %>%
  filter(!is.na(covariateName.x)) %>%
  filter(min_levels_of_separation == 1 & max_levels_of_separation == 1)

second_roll_up <- second_roll_up %>%
  select(-min_levels_of_separation, -max_levels_of_separation)

joined_succ_temp <- second_roll_up %>%
  left_join(second_roll_up, by = c("descendant_concept_id" = "ancestor_concept_id")) %>%
  filter(!is.na(descendant_concept_id.y))

joined_all_temp <- left_join(second_roll_up, second_roll_up, by = c("descendant_concept_id" = "ancestor_concept_id"))



