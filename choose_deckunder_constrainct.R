library(tidyverse)

Data_from_other_repo <- "../data/"
# Archetype based archetype
source("sources.R")
# DAte by or intervall 
# week intervall
# one ore more meta 

# multiple cards : max and min count for each
# Search for cards all or main or side
# meta cards number of lands / surveil ....

df_base <- readRDS(paste0(Data_from_other_repo,"data_meta_en_cours.rds"))

Archetype_choosen <- "Reanimator"
Archetype_based_choosen <- NULL
Archetype_reference_choosen <- NULL
week_min <- 0
# meta <- NULL


a <- df_base %>% 
  filter(
    Archetype %==% Archetype_choosen,
    Base_Archetype %==% Archetype_based_choosen,
    ReferenceArchetype_Archetype %==% Archetype_reference_choosen
  )






unique(a$Archetype)
