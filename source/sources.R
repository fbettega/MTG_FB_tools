################################################################################
if_null_fixe_return_else_value <- function(value, fix_val) {
  if (is.null(value)) {
    res <- fix_val
  } else {
    res <- value
  }
  return(res)
}


################################################################################
`%notin%` <- negate(`%in%`)
################################################################################
# operator returning true if var NULL
`%==%` <- function(e1, e2) {
  if (is.null(e2)) {
    return(TRUE)
  } else {
    return(e1 == e2)
  }
}
`%inornull%` <- function(e1, e2) {
  if (is.null(e2)) {
    return(TRUE)
  } else {
    return(e1 %in% e2)
  }
}


################################################################################
auto_select_input_if_one_choice <- function(
    inputId, label, choices, multiple = TRUE) {
  if (length(choices) == 1) {
    selectInput(
      inputId = inputId,
      label = label,
      choices = c(NULL, choices),
      selected = choices,
      multiple = multiple
    )
  } else {
    selectInput(
      inputId = inputId,
      label = label,
      choices = c(NULL, choices),
      multiple = multiple
    )
  }
}











################################################################################
################### Function that find optimum fetch ###########################
Fetch_land_optimizer <- function(deck_list_path, card_DB, list_format) {
  df_land <- card_DB %>%
    filter(str_detect(tolower(type_line), "land"))

  if (list_format == "txt") {
    test_deck_list <- deck_parser(deck_list_path) %>%
      rename(nom = Card_name, sep_side = Side) %>%
      mutate(nom = tolower(nom))
  } else if (list_format == "csv") {
    test_deck_list <- read.csv(deck_list_path) %>% rename(nom = Card_name, sep_side = Side)
  }

  Fetch_land <- df_land %>%
    select(name, oracle_text, image_uris.large) %>%
    filter(str_detect(tolower(oracle_text), "^\\{t\\}, pay 1 life, sacrifice")) %>%
    mutate(fetchable_land = str_extract(
      tolower(oracle_text),
      "(?<=\\: search your library for .{1,2}\\s)(.+)(?=card, put it onto the battlefield, then shuffle\\.)"
    )) %>%
    separate_wider_delim(fetchable_land,
      " or ",
      names = c("A", "B"),
      too_few = c("align_start")
    ) %>%
    select(-oracle_text) %>%
    pivot_longer(-c(name, image_uris.large), names_to = "temp", values_to = "type_fetchable") %>%
    mutate(type_fetchable = trimws(type_fetchable, which = "both")) %>%
    select(-temp) %>%
    drop_na() %>%
    dplyr::rename(
      fetch_name = name,
      fetch_image = image_uris.large
    ) %>%
    mutate(fetch_name = tolower(fetch_name))



  df_of_fetchable_land <- df_land %>%
    mutate(
      type_line = tolower(type_line),
      name = tolower(name)
    ) %>%
    fuzzyjoin::fuzzy_left_join(Fetch_land,
      by = c("type_line" = "type_fetchable"),
      match_fun = str_detect
    ) %>%
    select(name, type_line, fetch_name, type_fetchable, image_uris.large, fetch_image) %>%
    filter(!is.na(fetch_name)) %>%
    distinct(name, fetch_name, .keep_all = TRUE)



  fetch_in_deck <- test_deck_list %>%
    filter(test_deck_list$nom %in% unique(Fetch_land$fetch_name))


  fetchable_land_in_deck <- right_join(
    df_of_fetchable_land,
    test_deck_list,
    by = c("name" = "nom")
  ) %>%
    drop_na() %>%
    # uncount(quantite) %>%
    select(-sep_side)

  number_of_fetch_in_deck <- sum(fetch_in_deck$quantite)
  vector_of_fetchable_unique_land <- unique(fetchable_land_in_deck$name)



  initial_table_of_fetch_opti <- fetchable_land_in_deck %>%
    group_by(fetch_name) %>%
    summarise(
      nb_fetchable = sum(quantite),
      land_fetch = list(name)
    ) %>%
    arrange(desc(nb_fetchable)) %>%
    rowwise() %>%
    mutate(
      not_fetch_land = list(vector_of_fetchable_unique_land[vector_of_fetchable_unique_land %notin% land_fetch])
      # not_fetch_land = land_fetch %not_in% unique(fetchable_land_in_deck$name)
    )

  fetch_opti <- initial_table_of_fetch_opti %>%
    select(fetch_name, nb_fetchable)

  land_opti <- initial_table_of_fetch_opti %>%
    select(fetch_name, not_fetch_land) %>%
    unnest(not_fetch_land)



  fetch_opti <- initial_table_of_fetch_opti %>%
    filter(nb_fetchable >= unique(.$nb_fetchable)[min(length(unique(.$nb_fetchable)), 3)]) %>%
    select(fetch_name, nb_fetchable)

  based_data_of_fetch <- as.data.frame(matrix(rep(0:4, length(fetch_opti$fetch_name)), ncol = length(fetch_opti$fetch_name)))

  colnames(based_data_of_fetch) <- fetch_opti$fetch_name

  based_data_of_fetch_combin <- tidyr::expand(
    based_data_of_fetch,
    !!!based_data_of_fetch
  ) %>%
    filter(rowSums(.) == number_of_fetch_in_deck)



  Res_best_fetch_for_number_of_fetchable <- cbind(based_data_of_fetch_combin,
    total_number_fetchable = rowSums(
      sweep(
        as.matrix(based_data_of_fetch_combin),
        MARGIN = 2,
        fetch_opti$nb_fetchable, `*`
      )
    )
  ) %>%
    rownames_to_column()

  Min_fetchable_df <- initial_table_of_fetch_opti %>%
    filter(nb_fetchable >= unique(.$nb_fetchable)[min(length(unique(.$nb_fetchable)), 3)]) %>%
    select(-not_fetch_land, -nb_fetchable) %>%
    unnest(land_fetch) %>%
    left_join(
      fetchable_land_in_deck %>%
        select(name, quantite) %>%
        distinct(),
      by = c("land_fetch" = "name")
    ) %>%
    complete(fetch_name, land_fetch,
      fill = list(quantite = 0)
    ) %>%
    full_join(
      based_data_of_fetch_combin %>%
        rownames_to_column() %>%
        pivot_longer(-rowname),
      by = c("fetch_name" = "name"),
      relationship = "many-to-many"
    )


  result_total_fetchable <-
    inner_join(
      Min_fetchable_df %>%
        mutate(res = quantite * value) %>%
        group_by(rowname, land_fetch) %>%
        summarise(test = sum(res), .groups = "drop_last") %>%
        summarise(
          min_number_of_fetchable = min(test),
          name_land_min = paste0(.$land_fetch[which.min(test)], collapse = " / ")
        ),
      Res_best_fetch_for_number_of_fetchable,
      by = "rowname"
    ) %>%
    relocate(total_number_fetchable, min_number_of_fetchable, name_land_min, .after = last_col())


  opti_number_of_fetchable <- result_total_fetchable$rowname[
    result_total_fetchable$total_number_fetchable == max(result_total_fetchable$total_number_fetchable)
  ]


  opti_min_number_of_fetchable <- result_total_fetchable$rowname[
    result_total_fetchable$min_number_of_fetchable == max(result_total_fetchable$min_number_of_fetchable)
  ]


  common_solution_id <- intersect(opti_min_number_of_fetchable, opti_number_of_fetchable)
  if (length(common_solution_id) > 1) {
    text_res <- "real optimum found"

    res <- result_total_fetchable %>%
      filter(rowname %in% common_solution_id) %>%
      select(where(~ is.numeric(.x) && sum(.x) > 0))
  } else {
    text_res <- "No real optimum found"

    res <- dplyr::bind_rows(
      res_number_of_fetchable = result_total_fetchable %>%
        filter(rowname %in% opti_number_of_fetchable) %>%
        select(where(~ is.numeric(.x) && sum(.x) > 0), name_land_min),
      res_min_number_of_fetchable = result_total_fetchable %>%
        filter(rowname %in% opti_min_number_of_fetchable) %>%
        select(where(~ is.numeric(.x) && sum(.x) > 0), name_land_min) %>%
        arrange(desc(total_number_fetchable))
    ) %>%
      replace(is.na(.), 0) %>%
      relocate(total_number_fetchable, min_number_of_fetchable, name_land_min, .after = last_col())
  }

  fetch_result_list_en_cours <- initial_table_of_fetch_opti %>%
    inner_join(fetch_in_deck, by = c("fetch_name" = "nom")) %>%
    select(-sep_side)





  res_base <-
    cbind(
      fetch_result_list_en_cours %>%
        select(fetch_name, quantite) %>%
        pivot_wider(names_from = fetch_name, values_from = quantite),
      total_number_fetchable = sum(fetch_result_list_en_cours$nb_fetchable * fetch_result_list_en_cours$quantite),
      fetch_result_list_en_cours %>%
        select(-not_fetch_land, -nb_fetchable) %>%
        unnest(land_fetch) %>%
        rename(quantite_fetch = quantite) %>%
        left_join(
          fetchable_land_in_deck %>%
            select(name, quantite) %>%
            distinct(),
          by = c("land_fetch" = "name")
        ) %>%
        complete(fetch_name, land_fetch,
          fill = list(
            quantite = 0
          )
        ) %>%
        group_by(fetch_name) %>%
        fill(quantite_fetch) %>%
        mutate(res = quantite * quantite_fetch) %>%
        group_by(land_fetch) %>%
        summarise(test = sum(res)) %>%
        summarise(
          min_number_of_fetchable = min(test),
          name_land_min = list(.$land_fetch[which.min(test)])
        )
    )





  return(
    list(
      result_base = res_base,
      text = text_res,
      result = res
    )
  )
}
################################################################################
`%notin%` <- negate(`%in%`)
################################################################################
# operator returning true if var NULL
`%==%` <- function(e1, e2) {
  if (is.null(e2)) {
    return(TRUE)
  } else {
    return(e1 == e2)
  }
}

################################################################################

auto_select_input_if_one_choice <- function(
    inputId, label, choices, multiple = TRUE) {
  if (length(choices) == 1) {
    selectInput(
      inputId = inputId,
      label = label,
      choices = c(NULL, choices),
      selected = choices,
      multiple = multiple
    )
  } else {
    selectInput(
      inputId = inputId,
      label = label,
      choices = c(NULL, choices),
      multiple = multiple
    )
  }
}

################################################################################



deck_parser <- function(deck_path) {
  deck <- read.delim(deck_path, header = FALSE, blank.lines.skip = FALSE) %>%
    filter(!str_detect(.$V1, regex("deck|Sideboard", ignore_case = TRUE))) %>%
    mutate(Side = str_detect(.$V1, regex("^$", ignore_case = TRUE))) %>%
    mutate(
      quantite = as.numeric(str_extract_all(.$V1, "^[:digit:]*\\S*")),
      Card_name = tolower(str_extract(.$V1, "(?<=[:digit:]\\s).*"))
    ) %>%
    select(-V1)
}

################################################################################

Decklist_parse_add_type_fun <- function(path, Modern_card_DB_fun = Modern_card_DB) {
  Modern_card_with_simplified_type <- Modern_card_DB_fun %>%
    mutate(
      simplified_type = ifelse(
        str_detect(type_line, "Creature"),
        "Creature",
        ifelse(
          str_detect(type_line, "Land"),
          "Land",
          ifelse(
            str_detect(type_line, "Enchantment"),
            "Enchantment",
            ifelse(
              str_detect(type_line, "Instant|Sorcery"),
              "Spell",
              ifelse(
                str_detect(type_line, "Artifact"),
                "Artifact",
                ifelse(
                  str_detect(type_line, "Planeswalker"),
                  "Planeswalker",
                  ifelse(
                    str_detect(type_line, "Battle"),
                    "Battle", type_line
                  )
                )
              )
            )
          )
        )
      ),
      name = tolower(name)
    ) %>%
    drop_na(simplified_type) %>%
    select(name, simplified_type)





  list_import_base <- deck_parser(
    path
  ) %>%
    left_join(
      Modern_card_with_simplified_type,
      by = c("Card_name" = "name")
    )

  Side_cut_point <- which(list_import_base$Side)[1]


  list_import_base$Side[Side_cut_point:nrow(list_import_base)] <- TRUE

  list_import <- list_import_base[-Side_cut_point, ]

  # list_import <- list(
  #   main_deck = list_import_base[
  #     1:which(list_import_base$Side)-1,] %>% select(-Side),
  #   sideboard = list_import_base[
  #     (which(list_import_base$Side)+1):
  #       nrow(list_import_base),] %>% select(-Side)
  # )

  return(list_import)
}

################################################################################

wrenn_curve <- function(number_of_fetch,
                        number_of_wrenn_fun,
                        deck = 60,
                        card_draw = 8){
  
  
  df_success_wrenn <- rbind(
    expand.grid(
      sucess_wrenn = 0:number_of_wrenn_fun,
      sucess_fetch = 1:number_of_fetch
    ),
    data.frame(sucess_wrenn = 0,
               sucess_fetch = 0)
  ) %>% 
    filter(rowSums(.)<= card_draw)
  #browser()
  
  result <- df_success_wrenn %>%
    rowwise() %>%  
    mutate(
      proba_wrenn = 
        proba_compute(
          success = sucess_wrenn,
          hit = number_of_wrenn_fun,
          deck = deck,
          sample_size = card_draw)
      ,
      draw =  card_draw - sucess_wrenn,
      deck-number_of_wrenn_fun,
      hit = number_of_fetch,
      proba_fetch = proba_compute(
        success = sucess_fetch,
        hit = number_of_fetch,
        deck = deck-number_of_wrenn_fun,
        sample_size = card_draw - sucess_wrenn
      ),
      proba = proba_wrenn * 
        proba_fetch
      
      
      
    )
  
  return(sum(result$proba))
}


Wrenn_fun_analysis <- function(deck_list_path ,
                               card_DB,
                               list_format){
  if (list_format == "txt") {
    test_deck_list <- deck_parser(deck_list_path) %>%
      rename(nom = Card_name, sep_side = Side) %>%
      mutate(nom = tolower(nom))
  } else if (list_format == "csv") {
    test_deck_list <- read.csv(deck_list_path) %>% rename(nom = Card_name, sep_side = Side)
  }
  
  
  
  
  
  Fetch_land <- card_DB %>%
    filter(str_detect(tolower(type_line),"land")) %>% 
    select(name,oracle_text,image_uris.large) %>% 
    filter(str_detect(tolower(oracle_text),"^\\{t\\}, pay 1 life, sacrifice")) %>% 
    mutate(fetchable_land = str_extract(tolower(oracle_text),
                                        "(?<=\\: search your library for .{1,2}\\s)(.+)(?=card, put it onto the battlefield, then shuffle\\.)")
    ) %>% separate_wider_delim(fetchable_land, 
                               " or ", 
                               names = c("A", "B"),
                               too_few = c("align_start")) %>% 
    select(-oracle_text) %>%
    pivot_longer(-c(name,image_uris.large),names_to = "temp",values_to =   "type_fetchable") %>% 
    mutate(type_fetchable = trimws(type_fetchable, which = "both")) %>% 
    select(-temp) %>% 
    drop_na() %>% 
    dplyr::rename (fetch_name = name,
                   fetch_image = image_uris.large) %>% 
    mutate(fetch_name = tolower(fetch_name))
  
  
  
  
  
  Number_of_fetch_in_decklist <- test_deck_list %>% 
    inner_join(Fetch_land %>% 
                 distinct(fetch_name),
               by = c("nom" = "fetch_name") ) 
  
  
  
  number_of_wrenn <- test_deck_list  %>% 
    filter(nom == "wrenn and six") %>% 
    pull(quantite) 
  
  
  data_plot_wrenn <- expand.grid(n_fetch = 7:20,
                                 draw = 8 + (0:5)) %>% 
    rowwise() %>% 
    mutate(proba = wrenn_curve(number_of_fetch = n_fetch,
                               card_draw = draw,
                               number_of_wrenn_fun = number_of_wrenn
                               ) #*100
    ) %>% 
    ungroup() %>%
    mutate(draw  = paste(
      "Draw naturel +",
      draw - 8
    )
    )
  
  
  
  res_table <- data_plot_wrenn %>% 
    filter(draw == "Draw naturel + 0",n_fetch > 6,n_fetch < 17) %>%
    select(-draw) %>% 
    mutate(proba = paste0(round(proba,2)," %")) 
  
  
  
  p <- ggplot(data_plot_wrenn) +
    geom_line(aes(x = n_fetch,y = proba,colour = draw)) +
    geom_vline(xintercept = sum(Number_of_fetch_in_decklist$quantite),
               linetype="dotted", 
               color = "blue", linewidth=1.5)
  
  
  return(list(
    table = res_table,
    plot = p
  ))
  
}

################################################################################

Check_deck_list_for_analysis <- function(deck_list_path = deck_list_path,
                                         card_DB = card_DB,
                                         list_format = list_format){
  if (list_format == "txt") {
    test_deck_list <- deck_parser(deck_list_path) %>%
      rename(nom = Card_name, sep_side = Side) %>%
      mutate(nom = tolower(nom))
  } else if (list_format == "csv") {
    test_deck_list <- read.csv(deck_list_path) %>% rename(nom = Card_name, sep_side = Side)
  }
  
  
  Fetch_land <- card_DB %>%
    filter(str_detect(tolower(type_line),"land")) %>% 
    select(name,oracle_text,image_uris.large) %>% 
    filter(str_detect(tolower(oracle_text),"^\\{t\\}, pay 1 life, sacrifice")) %>% 
    mutate(fetchable_land = str_extract(tolower(oracle_text),
                                        "(?<=\\: search your library for .{1,2}\\s)(.+)(?=card, put it onto the battlefield, then shuffle\\.)")
    ) %>% separate_wider_delim(fetchable_land, 
                               " or ", 
                               names = c("A", "B"),
                               too_few = c("align_start")) %>% 
    select(-oracle_text) %>%
    pivot_longer(-c(name,image_uris.large),names_to = "temp",values_to =   "type_fetchable") %>% 
    mutate(type_fetchable = trimws(type_fetchable, which = "both")) %>% 
    select(-temp) %>% 
    drop_na() %>% 
    dplyr::rename (fetch_name = name,
                   fetch_image = image_uris.large) %>% 
    mutate(fetch_name = tolower(fetch_name)) %>% 
    distinct(fetch_name)
  
  
  
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
  
  
  
  pitch_bool <- (
    test_deck_list %>% 
      inner_join(pitch_cards ,
                 by = c("nom" = "cards") ) %>% 
      nrow()
  ) > 0
  
  Fetch_bool <- (
    test_deck_list %>% 
      inner_join(Fetch_land %>% 
                   distinct(fetch_name),
                 by = c("nom" = "fetch_name") ) %>% 
      nrow()
  ) > 0
  
  Wrenn_bool <- (test_deck_list %>% 
                   filter(nom == "wrenn and six") %>% nrow()) > 0
  
  return(
    list(
      pitch = pitch_bool,
      fetch  = Fetch_bool,
      wrenn = Wrenn_bool
    )
  )
}
################################################################################

