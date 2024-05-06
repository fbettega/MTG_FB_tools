library(tidyverse)
loaded_base_data <- readRDS("../data/data_meta_en_cours.rds")
Modern_card_DB <- read.csv("../data/DBcarte_modern.csv") %>% 
  mutate(
    name_join = trimws(
      gsub("//.*$","",name)
    )
  ) 

source("sources.R")


unlist_side_or_main <- function(df,cols_fun){
  not_colfuns <- ifelse(cols_fun == "Sideboard","Mainboard","Sideboard")
  
  Unnest_filter_table <- df %>%  
    select(
      all_of(cols_fun),not_colfuns,Player,AnchorUri, Archetype,Base_Archetype
    ) %>% 
    select(-all_of(not_colfuns)) %>% 
    unnest_longer(!!rlang::sym(cols_fun)) %>%
    unnest_wider(!!rlang::sym(cols_fun), names_sep = "_") %>% 
    mutate(Main_or_side = cols_fun) %>% 
    rename(Count = paste0(cols_fun, "_Count") ,CardName = paste0(cols_fun, "_CardName"))
  return(Unnest_filter_table)
}

partial_join <- function(x, y, by_x, pattern_y){
  idx_x <- sapply(y[[pattern_y]], grep, x[[by_x]])
idx_y <- sapply(seq_along(idx_x), function(i) rep(i, length(idx_x[[i]])))

df <- dplyr::bind_cols(x[unlist(idx_x), , drop = F],
                       y[unlist(idx_y), , drop = F])
return(df)
}



total_list_of_deck <- rbind(
  unlist_side_or_main(loaded_base_data,"Mainboard"),
  unlist_side_or_main(loaded_base_data,"Sideboard")
                            )
  
modern_cards_most_count <- total_list_of_deck %>% 
  select(Count,CardName) %>% 
  filter(Count <= 4) %>% 
  group_by(CardName) %>% 
  filter(Count == max(Count)) %>% 
  distinct() %>% 
  ungroup()







modern_cards_most_count_direct_match <- modern_cards_most_count %>% 
  left_join(
     Modern_card_DB %>%
      select(name,name_join,rarity),
     by = c("CardName" = "name")
    ) %>% 
  drop_na() %>% 
  select(-name_join)

modern_cards_most_count_second_match <- modern_cards_most_count %>% 
  filter(CardName %notin% modern_cards_most_count_direct_match$CardName) %>% 
  left_join(
    Modern_card_DB %>%
      select(name,name_join,rarity),
    by = c("CardName" = "name_join")
    ) %>% 
  drop_na() %>% 
  select(-CardName) %>% 
  rename(CardName = name )



modern_cards_most_count_with_rarity <- rbind(
  modern_cards_most_count_direct_match,
  modern_cards_most_count_second_match
  )







lapply(unique(modern_cards_most_count_with_rarity$rarity), function(x){
  
  to_write_db <- modern_cards_most_count_with_rarity %>% 
    filter(rarity == x) %>%
    select(-rarity)  %>%
    mutate(cut = floor(cumsum(Count)/1400))
  
  lapply(unique(to_write_db$cut), function(y) { 
    to_write_db_cut <- to_write_db %>% 
      filter(cut == y) %>% 
      select(-cut)
  
  write_tsv(to_write_db_cut,paste0("modern_cards_by_rarity/",x,"_",y,".txt"),
            col_names = FALSE)
    
    })
  return(NULL)
})
