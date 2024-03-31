library(tidyverse)

deck_parser <- function(deck_path){
  
  deck <- read.delim(deck_path,header = FALSE ,blank.lines.skip = FALSE) %>%
    filter( !str_detect(.$V1,regex('deck|Sideboard', ignore_case = TRUE))) %>%
    mutate(sep_side = str_detect(.$V1,regex('^$', ignore_case = TRUE))) %>% 
    mutate(quantite = as.numeric(str_extract_all(.$V1,"^[:digit:]*\\S*" )),
           nom = tolower(str_extract(.$V1,"(?<=[:digit:]\\s).*") ), 
    ) %>% 
    select(-V1) 
}


deck_to_compare_list <- list.files("data/Common_cards/",full.names = TRUE)



Deck_list <- lapply(
  deck_to_compare_list, function(x) {
  res <- deck_parser(x) 
  res$sep_side[which(res$sep_side)[1]:nrow(res)] <- TRUE
  
  res_final <- res %>% mutate(deck = x) %>% drop_na()
  return(res_final)
  }
  
  ) %>% bind_rows()



Common_cards <- Deck_list %>%
  group_by(nom) %>% 
  summarise(
    min_common = min(quantite),
    deck = list(deck)
            ) %>% 
  rowwise() %>% 
  mutate(
    n_common = length(deck)
    ) %>% 
  filter(n_common > 1)





Deck_list_without_common <- Common_cards %>% 
  unnest_longer(deck) %>% 
  select(-n_common) %>% 
  right_join(Deck_list,by = c("nom" ="nom",
                              "deck" = "deck"))











