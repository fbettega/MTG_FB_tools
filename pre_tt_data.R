library(tidyverse)
library(lubridate)
Modern_result_df <- readRDS("../data/data_meta_en_cours.rds")


unlist_side_or_main_deck_winner <- function(df,cols_fun){
  not_colfuns <- ifelse(cols_fun == "Sideboard","Mainboard","Sideboard")
  
  Unnest_filter_table <- df %>%  
    # select(
    #   all_of(cols_fun),not_colfuns,Player,AnchorUri, Archetype,Base_Archetype
    # ) %>% 
    select(-all_of(not_colfuns)) %>% 
    unnest_longer(!!rlang::sym(cols_fun)) %>%
    unnest_wider(!!rlang::sym(cols_fun), names_sep = "_") %>% 
    mutate(Main_or_side = cols_fun) %>% 
    rename(Count = paste0(cols_fun, "_Count") ,CardName = paste0(cols_fun, "_CardName"))
  return(Unnest_filter_table)
}

Full_unnest_modern <- rbind(
  unlist_side_or_main_deck_winner(Modern_result_df,"Mainboard"),
  unlist_side_or_main_deck_winner(Modern_result_df,"Sideboard")
  ) %>% 
  mutate(
    Date = lubridate::date(Date)
    )

write_rds(Full_unnest_modern,"data/modern_unnest_data.rds")