

###############################################################################
################### Pitch evaluation ###########################################

proba_compute <- function(success, hit, deck, sample_size, more_than = FALSE) {
  if (more_than) {
    sum(dhyper(x = success:hit, m = hit, n = deck - hit, k = sample_size))
  } else {
    dhyper(x = success, m = hit, n = deck - hit, k = sample_size)
  }
}



Pitch_cards_evaluation <- function(deck_list_path, card_DB, list_format) {
  pitch_cards <- rbind(
    list(
      w = c("Scars of the Veteran", "Reverent Mantra", "Shining Shoal", "Force of Virtue", "Solitude"),
      u = c("Force of Will", "Misdirection", "Disrupting Shoal", "Force of Negation", "Subtlety"),
      b = c("Contagion", "Unmask", "Sickening Shoal", "Force of Despair", "Grief"),
      r = c("Pyrokinesis", "Cave-In", "Blazing Shoal", "Force of Rage", "Fury"),
      g = c("Bounty of the Hunt", "Vine Dryad", "Nourishing Shoal", "Force of Vigor", "Endurance")
    ) %>%
      bind_rows() %>%
      pivot_longer(everything(), values_to = "cards", names_to = "color") %>%
      mutate(number_pitch = 1),
    list(
      w = c("Sunscour"),
      u = c("Commandeer"),
      b = c("Soul Spike"),
      r = c("Fury of the Horde"),
      g = c("Allosaurus Rider")
    ) %>%
      bind_rows() %>%
      pivot_longer(everything(), values_to = "cards", names_to = "color") %>%
      mutate(number_pitch = 2)
  ) %>%
    mutate(cards = tolower(cards))
  
  
  
  
  ###################################
  # pour le legacy donc plus tard
  
  pitch_discard_land <- list(
    white = c("Abolish"),
    blue = c("Foil"),
    black = c("Outbreak"),
    red = c("Flameshot"),
    green = c("Snag")
  )
  
  
  
  
  
  if (list_format == "txt") {
    test_deck_list <- deck_parser(deck_list_path) %>%
      rename(nom = Card_name, sep_side = Side) %>%
      mutate(nom = tolower(nom))
    
    test_deck_list$sep_side[which(test_deck_list$sep_side)[1]:nrow(test_deck_list)] <- TRUE
    
    test_deck_list <- test_deck_list %>%
      drop_na()
  } else if (list_format == "csv") {
    test_deck_list <- read.csv(deck_list_path) %>% rename(nom = Card_name, sep_side = Side)
    
    # test_deck_list$sep_side[which(test_deck_list$sep_side)[1]:nrow(test_deck_list)] <- TRUE
    #
    # test_deck_list <- test_deck_list %>%
    #   drop_na()
  }
  
  
  
  identify_pitch_cards <- inner_join(pitch_cards, test_deck_list, by = c("cards" = "nom"))
  if (nrow(identify_pitch_cards) == 0) {
    print("no pitch_cards")
  } else {
    pitchable_cards <- card_DB %>%
      select(name, card_faces.colors, colors, matches("^colors\\d+$")) %>%
      mutate(
        colors0 = if_else(
          !is.na(card_faces.colors),
          card_faces.colors, colors
        ),
        .after = 1
      ) %>%
      mutate(name = tolower(name)) %>%
      select(
        -card_faces.colors,
        -colors
      ) %>%
      pivot_longer(starts_with("colors"), names_to = "colors") %>%
      drop_na(value) %>%
      select(-colors) %>%
      distinct() %>%
      mutate(val = 1) %>%
      pivot_wider(names_from = value, values_from = val, values_fill = 0) %>%
      mutate(across(-name, ~ . == 1)) %>%
      right_join(
        test_deck_list,
        by = c("name" = "nom")
      ) %>%
      drop_na(R)
    
    
    color_of_interest <- toupper(identify_pitch_cards[1, ]$color)
    
    number_of_pitachable <- cbind(pitchable_cards %>%
                                    # filter(!sep_side) %>%
                                    filter(!!rlang::sym(color_of_interest)) %>%
                                    group_by(sep_side) %>%
                                    summarise(sum = sum(quantite), .groups = "drop") %>%
                                    select(-sep_side), name = c("number_pitachabel", "max_add")) %>%
      pivot_wider(values_from = sum, names_from = name) %>%
      mutate(color = identify_pitch_cards[1, ]$color)
    
    
    
    pitch_cards_and_number_of_pitachable <- lapply(
      unique(
        identify_pitch_cards$color
      ), function(x) {
        cbind(pitchable_cards %>%
                filter(!!rlang::sym(toupper(x))) %>%
                group_by(sep_side) %>%
                summarise(sum = sum(quantite), .groups = "drop") %>%
                select(-sep_side), name = c("number_pitachabel", "max_add")) %>%
          pivot_wider(values_from = sum, names_from = name) %>%
          mutate(color = x)
      }
    ) %>%
      bind_rows() %>%
      right_join(
        identify_pitch_cards,
        by = "color"
      ) %>%
      distinct(cards, .keep_all = TRUE) %>% 
      group_by(number_pitachabel,max_add,
               # color,
               number_pitch,quantite) %>% 
      summarise(cards = paste0(cards,collapse = "/"),.groups = "drop") %>% 
      mutate(cards = paste0(cards, " : ", quantite))
    
    
    list_of_result_table <- lapply(
      1:nrow(pitch_cards_and_number_of_pitachable),
      function(x) {
        # cbind(Number_of_pitch_cards = pitch_cards_and_number_of_pitachable[1,]$number_pitachabel,
        generate_res_table(
          deck = sum(test_deck_list %>% filter(!sep_side) %>% pull(quantite)),
          number_pitachabel = pitch_cards_and_number_of_pitachable[x, ]$number_pitachabel,
          max_add = pitch_cards_and_number_of_pitachable[x, ]$max_add,
          quantite = pitch_cards_and_number_of_pitachable[x, ]$quantite,
          number_pitch = pitch_cards_and_number_of_pitachable[x, ]$number_pitch
        ) # )
      }
    ) %>%
      set_names(pitch_cards_and_number_of_pitachable$cards)
  }
  
  return(list_of_result_table)
}



generate_res_table <- function(
    deck,
    number_pitachabel,
    max_add,
    quantite,
    number_pitch) {
  
  
  table_result_base <- expand.grid(
    Draw = seq(7, 7 + 5),
    base_Post_side = seq(
      number_pitachabel - (15 - max_add),
      number_pitachabel + max_add
    ),
    base_pitch_cards = quantite 
  ) %>% 
    rownames_to_column() 
  
  
  table_result_expand <- 
    right_join(
      table_result_base,
      table_result_base %>% 
        group_by(rowname) %>% 
        expand(
          Post_side = 0:base_Post_side,
          pitch_cards = 0:base_pitch_cards
        ) , by = "rowname") %>%
    select(-rowname) %>% 
    mutate(
      other = Draw - (Post_side + pitch_cards)
    ) %>% 
    filter(other >= 0)
  
  
  
  table_result_A_and_B <- table_result_expand %>% 
    filter(pitch_cards > 0 & 
             ((Post_side + pitch_cards - 1) >= number_pitch)
    ) %>% 
    select(-other)  %>%
    rowwise() %>%
    # proba de toucher elem et pitch
    mutate(
      A_and_B =
        proba_compute(
          success = pitch_cards,
          hit = base_pitch_cards,
          deck = deck,
          sample_size = Draw,
          more_than = FALSE
        ) *
        proba_compute(
          success = Post_side,
          hit = base_Post_side ,
          deck = deck - base_pitch_cards,
          sample_size = Draw - pitch_cards,
          more_than = FALSE
        )
    ) %>% 
    group_by(Draw,base_Post_side,base_pitch_cards ) %>% 
    summarise(A_and_B = sum(A_and_B),.groups = "drop")
  
  
  table_result_B_knowing_A <- table_result_expand %>% 
    mutate(pitch_cards = pitch_cards  - 1 ) %>% 
    filter(
      pitch_cards >= 0 & 
        ((Post_side + pitch_cards) >= number_pitch)
    ) %>% 
    select(-other)  %>%
    rowwise() %>%
    mutate(
      B_knowing_A =
        proba_compute(
          success = pitch_cards,
          hit = base_pitch_cards - 1,
          deck = deck - 1,
          sample_size = Draw - 1,
          more_than = FALSE
        ) *
        proba_compute(
          success = Post_side,
          hit = base_Post_side ,
          deck = deck - base_pitch_cards ,
          sample_size = Draw - (pitch_cards + 1),
          more_than = FALSE
        )
    ) %>% 
    group_by(Draw,base_Post_side,base_pitch_cards ) %>% 
    summarise(B_knowing_A = sum(B_knowing_A),.groups = "drop")
  
  
  
  table_result_final <- inner_join(
    table_result_A_and_B,
    table_result_B_knowing_A,
    by = join_by(Draw, base_Post_side, base_pitch_cards)
  ) %>%
    ungroup() %>%
    mutate(proba = paste(round(A_and_B ,3) * 100, "/", round(B_knowing_A ,3) * 100)) %>%
    select(-A_and_B,-B_knowing_A) %>%
    mutate(Draw = paste0("Turn ", Draw - 6)) %>%
    pivot_wider(
      names_from = Draw,
      values_from = proba
    ) %>%
    mutate(Diff_post_side = base_Post_side  - number_pitachabel, .before = 1) %>%
    mutate(Number_of_pitch_cards = number_pitachabel + Diff_post_side, .before = 1) %>%
    select(-base_Post_side)
  
  
  return(table_result_final)
}








