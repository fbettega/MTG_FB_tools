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
  
  res_final <- res %>% mutate(deck = str_remove(str_remove(x,"^data/Common_cards/"),"\\.txt$")) %>% drop_na()
  return(res_final)
  }
  
  ) %>% bind_rows()



Common_cards <- Deck_list %>%
  select(-sep_side) %>% 
  group_by(nom,deck) %>% 
  summarise(quantite = sum(quantite),.groups = "drop") %>% 
  group_by(nom) %>% 
  summarise(
    min_common = min(quantite),
    deck = list(sort(deck))
            ) %>% 
  rowwise() %>% 
  mutate(
    n_common = length(deck)
    ) %>% 
  filter(n_common > 1) #%>% 
  # unnest_longer(deck) %>% 
  # mutate(value = TRUE) %>% 
  # pivot_wider(names_from = deck,
  #             values_from = value,
  #              values_fill = FALSE
  #             )





common_list_of_cards_by_deck <- Common_cards %>% 
  mutate(deck_using_cards = paste0(deck,collapse = " : ")) %>% 
  select(-deck,-n_common ) %>% 
  group_by(deck_using_cards) %>% 
  group_split()


#Group by a 
# group split df and print
# remove cards from deck starting by side and print
# reflechir a visualisations avec image des cartes (surement a globaliser avec une fonction)


No_common_cards <- Deck_list %>% anti_join(Common_cards , by ="nom")


Common_cards_remove_from_side <- Common_cards %>% 
  unnest_longer(deck) %>% 
  select(-n_common) %>% 
  inner_join(Deck_list %>% filter(sep_side),by = c("nom" ="nom",
                              "deck" = "deck")) %>% 
  mutate(min_common2 = if_else(min_common <= quantite,0,min_common-quantite),
         quantite = if_else(min_common <= quantite,quantite-min_common,0)
         ) %>% 
  select(-min_common) %>% 
  rename(min_common = min_common2)




Common_cards_remove_from_main <- Common_cards %>%
  unnest_longer(deck) %>% 
  select(-n_common) %>% 
  left_join(Common_cards_remove_from_side %>%
              select(-sep_side,- quantite),
            by = c("nom","deck")
            ) %>% 
  mutate(min_common = if_else(is.na(min_common.y),min_common.x,min_common.y)) %>% 
  select(-min_common.x,-min_common.y) %>% 
  filter(min_common > 0) %>% 
  inner_join(Deck_list %>% filter(!sep_side),by = c("nom" ="nom",
                                                   "deck" = "deck")) %>% 
  mutate(quantite = quantite - min_common) %>% 
  filter(quantite > 0) %>% select(-min_common)



Not_common_list_of_cards <- rbind(rbind(
  No_common_cards,
  Common_cards_remove_from_main
  ),
  Common_cards_remove_from_side %>% select(-min_common) %>% filter(quantite > 0)
  ) %>%arrange(deck,sep_side) %>%
  group_split(deck)

# Reste a print proprement deck box  1 ,2 ... n
# common 1,2,3,... n

# + aide a la reconstruction zoo = box + common 2 et 4



# finalisé vizuel

  
    
  
  











