library(readr)
library(magrittr)
library(dplyr)

CCAE <- read_csv("cov_ccae.csv")
MDCR <- read_csv("cov_mdcr.csv")

CCAE_filtered <- CCAE %>%
  filter(covariateValue != 0)
MDCR_filtered <- MDCR %>%
  filter(covariateValue != 0)

inner_join_filtered <- select(CCAE_filtered, c(covariateName, covariateId, covariateValue)) %>%
  inner_join(select(MDCR_filtered, c(covariateId, covariateValue)), by = "covariateId")

inner_join <- CCAE %>%
  inner_join(MDCR, by = "covariateId")
