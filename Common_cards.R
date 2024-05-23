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


deck_list_base_path <- "data/Common_cards/"



deck_to_compare_list <- list.files(deck_list_base_path,full.names = TRUE)

Deck_list <- lapply(
  deck_to_compare_list, function(x) {
  res <- deck_parser(x) 
  res$sep_side[which(res$sep_side)[1]:nrow(res)] <- TRUE
  
  res_final <- res %>% mutate(deck = str_remove(str_remove(x,paste0("^",deck_list_base_path)),"\\.txt$")) %>% drop_na()
  return(res_final)
  }
  
  ) %>% bind_rows()





# dek_list_fun <- Deck_list
# previous_iter_common <- Common_cards
# problème sur saisie push
search_reamining <- function(dek_list_fun, previous_iter_common = NULL){
  
  
  
  if(is.null(previous_iter_common)){
    Common_cards <- dek_list_fun %>%
      select(-sep_side) %>% 
      group_by(nom,deck) %>% 
      summarise(quantite = sum(quantite),.groups = "drop") %>% 
      group_by(nom) %>% 
      summarise(
        # a = list(quantite),
        min_common = min(quantite),
        deck = list(sort(deck))
      ) %>% 
      rowwise() %>% 
      mutate(
        n_common = length(deck)
      ) %>% 
      filter(n_common > 1) %>%
      ungroup() %>% 
      mutate(grouping_column = paste0(deck)) %>% 
      unnest_longer(deck) %>% 
      select(-n_common)
    
    reamaining_common <- data.frame()
  } else {
    reamaining_common <-  dek_list_fun %>%
      select(-sep_side) %>% 
      group_by(nom,deck) %>% 
      left_join(previous_iter_common %>% 
                  group_by(nom,deck,min_common) %>% 
                  summarise(min_common = sum(min_common),.groups = "drop"),
                by = c("nom","deck")) %>% 
      drop_na(min_common) %>% 
      mutate(quantite = quantite - min_common) %>% 
      select(-min_common) %>% 
      filter(quantite > 0) %>% 
      group_by(nom) %>% 
      mutate(number_of_occurence = n()) %>% 
      filter(number_of_occurence > 1) %>% 
      select(-number_of_occurence)%>% 
      group_by(nom,deck) %>% 
      summarise(quantite = sum(quantite),.groups = "drop") %>% 
      group_by(nom) %>% 
      summarise(
        min_common = min(quantite),
        deck = list(sort(deck)),
        .groups = "drop"
      ) %>% 
      rowwise() %>% 
      mutate(
        n_common = length(deck)
      ) %>% 
      filter(n_common > 1)  %>% 
      ungroup() %>% 
      mutate(grouping_column = paste0(deck)) %>% 
      unnest_longer(deck) %>% 
      select(-n_common)
    
    
    Common_cards <- rbind(previous_iter_common,reamaining_common)
    
    
  }
  
  if (nrow(reamaining_common) > 0 | is.null(previous_iter_common)) {
    res <- search_reamining(dek_list_fun,Common_cards)
    
  } else{
    res <- Common_cards
  }
  return(res)
}

Common_cards <- search_reamining(Deck_list)







common_list_of_cards_by_deck <- Common_cards %>% 
  group_by(grouping_column) %>% 
  mutate(deck_using_cards = paste0(deck,collapse = " : ")) %>% 
  ungroup() %>% 
  select(-deck ,-grouping_column) %>% 
  group_by(deck_using_cards) %>% 
  distinct() %>% 
  group_split()


#Group by a 
# group split df and print
# remove cards from deck starting by side and print
# reflechir a visualisations avec image des cartes (surement a globaliser avec une fonction)


No_common_cards <- Deck_list %>% anti_join(Common_cards , by ="nom")


Common_cards_remove_from_side <- Common_cards %>% 
  unnest_longer(deck) %>% 
  inner_join(Deck_list %>% filter(sep_side),by = c("nom" ="nom",
                              "deck" = "deck")) %>% 
  mutate(min_common2 = if_else(min_common <= quantite,0,min_common-quantite),
         quantite = if_else(min_common <= quantite,quantite-min_common,0)
         ) %>% 
  select(-min_common) %>% 
  rename(min_common = min_common2)




Common_cards_remove_from_main <- Common_cards %>%
  # unnest_longer(deck) %>% 
  left_join(Common_cards_remove_from_side %>%
              select(-sep_side,- quantite),
            by = c("nom","deck","grouping_column")
            ) %>% 
  mutate(min_common = if_else(is.na(min_common.y),min_common.x,min_common.y)) %>% 
  select(-min_common.x,-min_common.y) %>% 
  filter(min_common > 0) %>% 
  inner_join(Deck_list %>% filter(!sep_side),by = c("nom" ="nom",
                                                   "deck" = "deck")) %>% 
  mutate(quantite = quantite - min_common) %>% 
  filter(quantite > 0) %>%
  select(-min_common,-grouping_column)



Not_common_list_of_cards <- rbind(
  rbind(
  No_common_cards,
  Common_cards_remove_from_main 
  ),
  Common_cards_remove_from_side %>% 
    select(-min_common,-grouping_column) %>% 
    filter(quantite > 0)
  ) %>% arrange(deck,sep_side) %>%
  group_split(deck)




unlink("Deck_box.txt")
write(paste0("Common cards : ","\n"),file= "Deck_box.txt",append=TRUE)
invisible(
  lapply(common_list_of_cards_by_deck, function(x){
    # print(unique(x$deck_using_cards))
    write(paste0(unique(x$deck_using_cards)),file= "Deck_box.txt",append=TRUE)
    write_tsv(x %>% select(-deck_using_cards),file = "Deck_box.txt",append=TRUE)
    write("\n",file="Deck_box.txt",append=TRUE)
  # 
  # x %>%  print(n = 100)  
}
))

write(paste0("Separate Deck : ","\n"),file= "Deck_box.txt",append=TRUE)
invisible(lapply(Not_common_list_of_cards, function(x){
  write(paste0(unique(x$deck)),file= "Deck_box.txt",append=TRUE)
  write(paste0("Maindeck :"),file= "Deck_box.txt",append=TRUE)
  write_tsv(x %>% filter(!sep_side ) %>% select(-deck, -sep_side),file = "Deck_box.txt",append=TRUE)
  write(paste0("Sideboard :"),file= "Deck_box.txt",append=TRUE)
  write_tsv(x %>% filter(sep_side ) %>% select(-deck, -sep_side),file = "Deck_box.txt",append=TRUE)
  write("\n",file="Deck_box.txt",append=TRUE)
  
  }
  ))




# Reste a print proprement deck box  1 ,2 ... n
# common 1,2,3,... n

# + aide a la reconstruction zoo = box + common 2 et 4



# finalisé vizuel

  
    
  
  











