## code to prepare `DATASET` dataset goes here

# usethis::use_data("DATASET")
# usethis::use_data(pbc_mayo)


#### Packages --------------------------------

library(survival)
library(tidyverse)

#### The data --------------------------------

# Data set comes from the survival package
# Load Mayo Clinic Primary Biliary Cirrhosis Data
pbc_mayo <- survival::pbc

dplyr::glimpse(pbc_mayo)


#### Make factors/characters --------------------------------

# Convert some variables to factors based on the description here:
# https://www.mayo.edu/research/documents/pbchtml/doc-10027635

pbc_mayo <- pbc_mayo %>%
  mutate(sex = factor(sex,
                      levels = c("m", "f"),
                      labels = c("Male", "Female")),
         status = factor(status,
                         levels = c(0, 1, 2),
                         labels = c("Alive",
                                    "Liver transplant",
                                    "Dead")),
         trt = factor(trt,
                         levels = c(1, 2),
                         labels = c("D-penicillamine",
                                    "Placebo")),
         edema = factor(edema,
                        levels = c(0, 0.5, 1),
                        labels = c("No edema and no diuretic therapy for edema",
                                   "Edema present without diuretics, or edema resolved by diuretics",
                                   "Edema despite diuretic therapy")),
         stage = as.character(stage)) %>%
  mutate_at(.vars = vars(ascites, hepato, spiders),
            .funs = list(~ factor(.,
                                  levels = c(0, 1),
                                  labels = c("No", "Yes")))) %>%
  dplyr::rename(alk_phos = alk.phos)


pbc_mayo %>%
  dplyr::glimpse()






# This data is from the Mayo Clinic trial in primary biliary cirrhosis (PBC) of the liver conducted between 1974 and 1984. A total of 424 PBC patients, referred to Mayo Clinic during that ten-year interval, met eligibility criteria for the randomized placebo controlled trial of the drug D-penicillamine. The first 312 cases in the data set participated in the randomized trial and contain largely complete data. The additional 112 cases did not participate in the clinical trial, but consented to have basic measurements recorded and to be followed for survival. Six of those cases were lost to follow-up shortly after diagnosis, so the data here are on an additional 106 cases as well as the 312 randomized participants.
#
# A nearly identical data set found in appendix D of Fleming and Harrington; this version has fewer missing values.
#
# Therneau and P Grambsch (2000), Modeling Survival Data: Extending the Cox Model, Springer-Verlag, New York. ISBN: 0-387-98784-3.
