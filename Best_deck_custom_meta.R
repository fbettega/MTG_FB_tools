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
source("source/sources.R")
source("source/pitch_eval_sources.R")

deck_list_path <- "data/temur_joute_novembre.txt"

card_DB <- read.csv("../data/DBcarte_modern.csv")

list_format <- "txt"


# A voir pour leyline
# land et couleur des spells





bool_analysis_to_do <- Check_deck_list_for_analysis(
  deck_list_path = deck_list_path,
  card_DB = card_DB,
  list_format = list_format
)

################################################################################
# les 2 chiffres sont proba de A et B / proba de B sachant A
if (bool_analysis_to_do$pitch) {
  pitch_cards <- Pitch_cards_evaluation(
    deck_list_path = deck_list_path,
    card_DB = card_DB,
    list_format = list_format
  )
}

################################################################################
if (bool_analysis_to_do$fetch) {
  result_opti <- Fetch_land_optimizer(
    deck_list_path = deck_list_path,
    card_DB = card_DB,
    list_format = list_format
  )
}

################################################################################
if (bool_analysis_to_do$wrenn) {
  wreen_res <- Wrenn_fun_analysis(
    deck_list_path = deck_list_path,
    card_DB = card_DB,
    list_format = list_format
  )
}
