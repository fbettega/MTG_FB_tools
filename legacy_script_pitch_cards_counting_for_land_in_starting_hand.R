library(DT)
library(shiny)
library(tidyverse)
library(datamods)
library(data.table)
library(flextable)
library(shinyscreenshot)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(DT::renderDataTable)
# deck_list_pathdeck_list_path,card_DB,list_formatcard_DB,list_format
source("sources.R")

deck_list_path <- "data/temur_joute_novembre.txt"

card_DB <- read.csv("../data/DBcarte_modern.csv")

list_format <- "txt"




a <- lapply(1:nrow(table_of_cards_test) , function(y) {
  compute_pitch_proba(y,table_of_cards = table_of_cards_test)
})



max_add <- 5
quantite <- 1

table_of_cards_test <- expand.grid(
  Draw = seq(7, 8 + 5),
  deck = 60,
  number_pitachabel = seq(23 - (15 - max_add),23 + max_add) ,
  land = 22,
  elem = 4
)




compute_proba_with_two_tables <- function(x,possible_combination,max_number_of_cards)  {
  if (x == 1) {
    
    res <- proba_compute(
      success = possible_combination[, x],
      hit = max_number_of_cards[, x],
      deck = max_number_of_cards$Deck,
      sample_size = max_number_of_cards$Hand_size,
      more_than = FALSE
    )
  } else if (x == 2) { # just because rowsums don't work if one dimmension table
    res <- proba_compute(
      success = possible_combination[, x],
      hit = max_number_of_cards[, x],
      deck = max_number_of_cards$Deck - max_number_of_cards[, 1:(x - 1)],
      sample_size = max_number_of_cards$Hand_size - possible_combination[, 1:(x - 1)],
      more_than = FALSE
    )
  } else {
    res <- proba_compute(
      success = possible_combination[, x],
      hit = max_number_of_cards[, x],
      deck = max_number_of_cards$Deck - rowSums(max_number_of_cards[, 1:(x - 1)]),
      sample_size = max_number_of_cards$Hand_size - rowSums(possible_combination[, 1:(x - 1)]),
      more_than = FALSE
    )
  }
  return(res)
}


compute_pitch_proba <- function(y,
                                table_of_cards,
                                start_hand_size = 7, 
                                min_max_number_of_land = c(2,4)){
  base_df_number_cards <- data.frame(
    Number_of_pitchable = table_of_cards$number_pitachabel[y],
    Number_of_elem = table_of_cards$elem[y],
    Number_of_land = table_of_cards$land[y],
    Hand_size = table_of_cards$Draw[y],
    Deck = table_of_cards$deck[y]
  )
  
  
  
  
  table_possible_comibnation_intial_hand <- expand.grid(
    Init_Post_side = 0:base_df_number_cards$Number_of_pitchable,
    Init_pitch_cards = 0:base_df_number_cards$Number_of_elem,
    Init_lands = min(min_max_number_of_land):max(min_max_number_of_land)
  ) %>% 
    mutate(
      other = start_hand_size - (Init_Post_side + Init_pitch_cards + Init_lands)
    ) %>% 
    filter(other >= 0) %>% 
    mutate(
      remaining_draw = base_df_number_cards$Hand_size - start_hand_size,
      initial_functionnal_hand = Init_pitch_cards >= 1 &
        (
          (Init_Post_side + Init_pitch_cards - 1)
          >= quantite
        ),
      missing_pitch = Init_pitch_cards - 1 ,
      missing_post_side =  (Init_Post_side + (Init_pitch_cards - if_else(missing_pitch < 0,0,1)
                                              
      ) ) - quantite
    ) %>%
    mutate(across(c(missing_pitch,missing_post_side),~abs(if_else(.>0,0,.)))) %>% 
    mutate(total_number_of_missing_cards = missing_pitch + missing_post_side) %>% 
    filter(total_number_of_missing_cards <= remaining_draw ) %>%  
    rownames_to_column() %>% 
    select(-other)
  
  
  
  
  table_possible_comibnation_compleate <- table_possible_comibnation_intial_hand %>% 
    filter(initial_functionnal_hand #  total_number_of_missing_cards == remaining_draw
    ) %>% 
    select(starts_with("Init_")) %>% 
    as.data.frame()
  
  
  
  Proba_A_and_B_functionnal_starting_hand <- map(
    seq_along(table_possible_comibnation_compleate),
    function(x) {
      compute_proba_with_two_tables(
        x,
        table_possible_comibnation_compleate,
        base_df_number_cards)
    }
  ) %>%
    setNames(colnames(table_possible_comibnation_compleate)) %>%
    bind_cols() %>%
    transmute(proba_start_hand = Reduce(`*`, .))
  
  
  if((base_df_number_cards$Hand_size - start_hand_size) > 0 ) {
    
    
    
    table_possible_comibnation_to_expand <- table_possible_comibnation_intial_hand %>% 
      filter(!initial_functionnal_hand & total_number_of_missing_cards != remaining_draw) 
    
    if(nrow(table_possible_comibnation_to_expand) > 0){
      
      table_possible_comibnation_after_expand  <- 
        rbind(
          table_possible_comibnation_to_expand %>%
            group_by(rowname) %>% 
            expand(missing_pitch = missing_pitch:remaining_draw,
                   missing_post_side = missing_post_side:remaining_draw,
                   missing_land = 0:remaining_draw
            ) %>% 
            inner_join(
              table_possible_comibnation_to_expand %>% 
                select(
                  -missing_pitch, 
                  -missing_post_side
                ),
              by = "rowname"
            ) %>% 
            filter(missing_pitch + missing_post_side + missing_land <= remaining_draw) %>% 
            ungroup(),
          table_possible_comibnation_intial_hand %>% 
            filter(!initial_functionnal_hand & total_number_of_missing_cards == remaining_draw) %>% 
            mutate(missing_land = 0)
        )
      
      
    }else{
      table_possible_comibnation_after_expand  <- table_possible_comibnation_intial_hand %>% 
        filter(!initial_functionnal_hand & total_number_of_missing_cards == remaining_draw) 
    }
    
    
    
    combination_init_hand_A_and_B <- table_possible_comibnation_after_expand %>% 
      select(starts_with("Init_")) %>% 
      as.data.frame() %>% 
      distinct() 
    
    
    
    proba_A_and_B_starting_hand <- 
      cbind(
        combination_init_hand_A_and_B,
        map(
          seq_along(combination_init_hand_A_and_B),
          function(x) {
            compute_proba_with_two_tables(
              x,
              combination_init_hand_A_and_B %>% distinct(),
              base_df_number_cards)
          }
        ) %>%
          setNames(colnames(combination_init_hand_A_and_B)) %>%
          bind_cols() %>%
          transmute(proba_start_hand = Reduce(`*`, .))
      ) %>% 
      right_join(
        table_possible_comibnation_after_expand,
        by = join_by(Init_Post_side, Init_pitch_cards, Init_lands)
      )
    
    base_df_number_cards_post_start_hand <- 
      data.frame(Number_of_pitchable = base_df_number_cards$Number_of_pitchable  - table_possible_comibnation_after_expand$Init_Post_side,
                 Number_of_elem = base_df_number_cards$Number_of_elem - table_possible_comibnation_after_expand$Init_pitch_cards,
                 Number_of_land = base_df_number_cards$Number_of_land - table_possible_comibnation_after_expand$Init_lands,
                 
                 Hand_size = base_df_number_cards$Hand_size - start_hand_size,
                 Deck = base_df_number_cards$Deck
      )
    combination_post_start_hand_A_and_B <- table_possible_comibnation_after_expand %>% 
      select(starts_with("missing_")) %>% 
      as.data.frame()
    
    
    proba_A_and_B_global <- 
      cbind(
        proba_A_and_B_starting_hand,
        map(
          seq_along(combination_post_start_hand_A_and_B),
          function(x) {
            compute_proba_with_two_tables(
              x,
              combination_post_start_hand_A_and_B,
              base_df_number_cards_post_start_hand)
          }
        ) %>%
          setNames(colnames(combination_post_start_hand_A_and_B)) %>%
          bind_cols() %>%
          transmute(proba_draw = Reduce(`*`, .))
      ) %>% 
      mutate(proba = proba_start_hand * proba_draw)
    
    res_A_and_B <- sum(proba_A_and_B_global$proba) +
      sum(Proba_A_and_B_functionnal_starting_hand$proba_start_hand)
  } else {
    res_A_and_B <- sum(Proba_A_and_B_functionnal_starting_hand$proba_start_hand)
    
  }
  
  
  
  
  
  
  
  base_df_number_cards_knowing_A <- base_df_number_cards %>% 
    mutate(Number_of_elem = Number_of_elem - 1,
           Hand_size = Hand_size - 1,
           Deck = Deck - 1
    ) %>% select(-Number_of_land )
  
  table_possible_comibnation_knowing_A <- expand.grid(
    Post_side = 0:base_df_number_cards_knowing_A$Number_of_pitchable,
    pitch_cards = 0:(base_df_number_cards_knowing_A$Number_of_elem)
  ) %>% 
    mutate(
      other = (base_df_number_cards_knowing_A$Hand_size) - (Post_side + pitch_cards)
    ) %>% 
    filter(other >= 0) %>% 
    filter((Post_side + pitch_cards) >= quantite) %>%
    select(-other)
  
  
  
  proba_B_knowing_A <- map(
    seq_along(table_possible_comibnation_knowing_A),
    function(x) {
      compute_proba_with_two_tables(x,
                                    table_possible_comibnation_knowing_A,
                                    base_df_number_cards_knowing_A
      )
    }
  ) %>%
    setNames(colnames(table_possible_comibnation_knowing_A)) %>%
    bind_cols() %>%
    transmute(proba = Reduce(`*`, .))
  
  return(list(A_and_B =  res_A_and_B,
              B_knowing_A = sum(proba_B_knowing_A$proba)
  ))
  
  
}
