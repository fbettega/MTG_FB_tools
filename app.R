library(DT)
library(shiny)
library(tidyverse)
library(datamods)
library(data.table)
library(flextable)
library(shinyscreenshot)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflicts_prefer(DT::renderDataTable)
# conflicted::conflicts_prefer(shiny::renderDataTable)

 source("sources.R")

# screenshotButton()
# TO DO
# fetch optimizer debug real optimum
# check if deck is opti


# Reste beaucoup a faire
# Reflechir poour les filter non-in qui s'auto reset


Data_from_other_repo <- "../data/"


# data_folder <- paste0(Data_from_other_repo,"data/")



Modern_card_DB <- read.csv("../data/DBcarte_modern.csv")
Modern_result_df <- readRDS("data/modern_unnest_data.rds")


# side_plan_from_entry_base <- readRDS(file.path(data_folder,"df_Side_table.rds"))


options(DT.options = list(
  pageLength = 50,
  lengthMenu = c(5, 50, 100, 1000), 
  language = list(search = 'Filter:')
)
)

headerCallback <- c(
  "function(thead, data, start, end, display){",
  "  var $ths = $(thead).find('th');",
  "  $ths.css({'vertical-align': 'bottom', 'white-space': 'nowrap'});",
  "  var betterCells = [];",
  "  $ths.each(function(){",
  "    var cell = $(this);",
  "    var newDiv = $('<div>', {height: 'auto', width: cell.height()});",
  "    var newInnerDiv = $('<div>', {text: cell.text()});",
  "    newDiv.css({margin: 'auto'});",
  "    newInnerDiv.css({",
  "      transform: 'rotate(180deg)',",
  "      'writing-mode': 'tb-rl',",
  "      'white-space': 'nowrap'",
  "    });",
  "    newDiv.append(newInnerDiv);",
  "    betterCells.push(newDiv);",
  "  });",
  "  $ths.each(function(i){",
  "    $(this).html(betterCells[i]);",
  "  });",
  "}"
)



options(DT.options = list(
  pageLength = 20,
  lengthMenu = c(5,20 , 50, 100, 1000), 
  language = list(search = 'Filter:')
)
)



ui <- navbarPage(
  "Dashboard",
  # Side entry
  tabPanel(
    "Fetch_optimizer",
    sidebarPanel(
      wellPanel(
        h2("Upload list"),
        fileInput("deck_list_fetch_opti", 
                  NULL,
                  buttonLabel = "Upload...",
                  accept = c(".csv", ".txt")
        ),
      )
    ),
    mainPanel(
      h2( textOutput("res_text_opti_fetch")),
      DTOutput('efetch_opti_res_base', width = "50%"),
      DTOutput('efetch_opti_res_number_of_fetchable', width = "50%")
      
    )
  ),
  tabPanel(
    "Side_from_data_entry_panel",
    sidebarPanel(
      wellPanel(
        shinyjs::useShinyjs(),
        uiOutput("Deck_select_Archetype"),
        uiOutput("Deck_select_based_Archetype"),
        uiOutput("Deck_select_reference_Archetype"),
        uiOutput("Deck_select_meta"),
        
        numericInput(
          inputId = "Number_of_cards_consider",
          label = "Nunber of cards",
          min = 1,
          value = 1
        ),
        
          # uiOutput("Deck_select_main_or_side"),
          # uiOutput("Deck_select_card_count"),
          # uiOutput("Deck_select_card_min_or_max"),
          # uiOutput("Deck_select_card_name"),
        
        
         uiOutput("card_search_for_output"),
        
        # lapply(1:input.Number_of_cards_consider,function(i){
        #   uiOutput(paste0("Deck_select_main_or_side",i))
        #   uiOutput(paste0("Deck_select_card_count",i))
        #   uiOutput(paste0("Deck_select_card_min_or_max",i))
        #   uiOutput(paste0("Deck_select_card_name",i))
        # }
        # ),

        
        h2("Choose list"),
        actionButton("Update_deck_select_filter", label = "Update filter" ),
        
        actionButton("Show_decklist_filter", label = "Show decklist" )
        
        
        
        # actionButton("browser", label ="browser" )#,
        # uiOutput("Deck_from_entry_Deck_list"),
        # uiOutput("screen_shot_buttom")
        
      )
    ),
    mainPanel(
      uiOutput("t1"),
      DTOutput('Deck_url_df')
      
    )
  ),
  id = "Tab_active_nav_bar"
)




server <- function(input, output, session) {
  
  # Change_in_deck_filter <- reactive({
  #   list(
  #     input$Deck_select_Archetype,
  #     input$Deck_select_based_Archetype,
  #     input$Deck_select_reference_Archetype,
  #     input$Deck_select_meta
  #     )
  # })
  


  filter_df_of_deck <-  eventReactive(
    input$Update_deck_select_filter,{
      df_based_filter <- Modern_result_df %>%
        filter(
          Archetype %inornull% input$Deck_select_Archetype,
          Base_Archetype %inornull% input$Deck_select_based_Archetype,
          ReferenceArchetype_Archetype %inornull% input$Deck_select_reference_Archetype,
          Meta %inornull% input$Deck_select_meta
          
        )

    id_of_cards_filter <- lapply(1:input$Number_of_cards_consider, function(i) {
      if(
        !is.null(input[[paste0("Deck_select_main_or_side",i)]])
        ){
      main_or_side <- if_else(
        input[[paste0("Deck_select_main_or_side",i)]] == "Main",
        "Mainboard",
        if_else(
          input[[paste0("Deck_select_main_or_side",i)]] == "Side",
          "Sideboard",
          input[[paste0("Deck_select_main_or_side",i)]]
        )
      )
        if(main_or_side == "Both") {
          main_or_side <- NULL
        }
      
      }else{
        main_or_side <- NULL
      }
     input[[paste0("Deck_select_card_name",i)]]
      if(
        !is.null(input[[paste0("Deck_select_card_min_or_max",i)]]) 
        ){
        if(
          is.na(input[[paste0("Deck_select_card_count",i)]])
          ){
          condition_en_cours <- "TRUE"
        }else{
          # browser()
          condition_en_cours <- paste0(
            "Count",
            if_else(input[[paste0("Deck_select_card_min_or_max",i)]],"<=",">="),
            input[[paste0("Deck_select_card_count",i)]])
          }
      }else{
        condition_en_cours <- "TRUE"
      }
     
     if(if_null_fixe_return_else_value(input[[paste0("Deck_select_card_name",i)]][[1]],"") == ""){
       Card_name_en_cours <- NULL
     }else{
       Card_name_en_cours <- input[[paste0("Deck_select_card_name",i)]][[1]]
     }
     
     if(
       !is.null(input[[paste0("Deck_select_card_min_or_max",i)]])  
     ){
       if(input[[paste0("Deck_select_card_min_or_max",i)]]){
 

         id_of_cards_filter_en_cours <-  df_based_filter %>% 
           filter( Main_or_side %==% main_or_side) %>% 
           group_by(id) %>% 
           mutate(not_the_cards = all(CardName != input[[paste0("Deck_select_card_name",i)]][[1]])) %>% 
           ungroup() %>% 
           mutate(Cards_but_max_qant = CardName == input[[paste0("Deck_select_card_name",i)]][[1]] & eval(rlang::parse_expr(condition_en_cours)) ,
                  condition_df = not_the_cards|Cards_but_max_qant) %>% 
           filter(condition_df) %>% 
         pull(id)

       }else{


         id_of_cards_filter_en_cours <- df_based_filter %>% 
           filter(
             CardName %==% Card_name_en_cours,
             Main_or_side %==% main_or_side
           ) %>%
           filter(
             eval(rlang::parse_expr(condition_en_cours))
           ) %>%
           pull(id)
       }
     } else {
     id_of_cards_filter_en_cours <- df_based_filter %>% 
        filter(
          eval(rlang::parse_expr(condition_en_cours))
        ) %>%
        pull(id)
     }
     
     return(id_of_cards_filter_en_cours)
    }
     
     )

      common_id <- Reduce(intersect, id_of_cards_filter)
      res_cards_filter_df <- df_based_filter %>% 
        filter(id %in% common_id)
      
      return(res_cards_filter_df)
      }
    )
    


observeEvent(#filter_df_of_deck()
  input$Update_deck_select_filter,{
    output$Deck_select_Archetype <- renderUI({
      selectInput(
        inputId = "Deck_select_Archetype",
        label = "Archetype",
        choices = c(#"",
          if_null_fixe_return_else_value(input$Deck_select_Archetype,""),
          unique(filter_df_of_deck()$Archetype)
          ),
        selected = input$Deck_select_Archetype,
        multiple = TRUE
      )
    })
    
    output$Deck_select_based_Archetype <- renderUI({
      selectInput(
        inputId = "Deck_select_based_Archetype",
        label = "Base Archetype",
        choices = c(if_null_fixe_return_else_value(input$Deck_select_based_Archetype,""),
                    unique(filter_df_of_deck()$Base_Archetype)
                    ),
        selected = input$Deck_select_based_Archetype,
        multiple = TRUE
      )
    })
    
    output$Deck_select_reference_Archetype <- renderUI({
      selectInput(
        inputId = "Deck_select_reference_Archetype",
        label = "Reference Archetype",
        choices = c(
          if_null_fixe_return_else_value(input$Deck_select_reference_Archetype,""),
          unique(filter_df_of_deck()$ReferenceArchetype_Archetype)
          ),
        selected = input$Deck_select_reference_Archetype,
        multiple = TRUE
      )
    })
    
    output$Deck_select_meta <- renderUI({
      selectInput(
        inputId = "Deck_select_meta",
        label = "Meta",
        choices = c(if_null_fixe_return_else_value(input$Deck_select_meta,""),
                    unique(filter_df_of_deck()$Meta)
                    ),
        selected = input$Deck_select_meta,
        multiple = TRUE
      )
    })
  }#,
  # ignoreInit = FALSE,
  # ignoreNULL = FALSE
  )


observeEvent(#input$Number_of_cards_consider,
  input$Update_deck_select_filter,{
  output$card_search_for_output <- renderUI({
    lapply(1:input$Number_of_cards_consider, function(i) {
      # if( i > 1) browser()
list(
  selectInput(
        inputId = paste0("Deck_select_card_name",i),
        label = paste0("Card name ",i),
        choices = c(
           if_null_fixe_return_else_value(
             input[[paste0("Deck_select_card_name",i)]],""),unique(filter_df_of_deck()$CardName)
        ),
         selected = input[[paste0("Deck_select_card_name",i)]],
         multiple = TRUE
        
      ),
  
      checkboxInput(
        inputId = paste0("Deck_select_card_min_or_max",i),
        label = paste0("Max or min count ",i),
        value = if_null_fixe_return_else_value(
          input[[paste0("Deck_select_card_min_or_max",i)]],
          FALSE)
        ),

      numericInput(
        inputId = paste0("Deck_select_card_count",i),
        label = paste0("Card count ",i),
        min = 0,
        max = max(c(
          filter_df_of_deck()$Count,
          if_null_fixe_return_else_value(
            input[[paste0("Deck_select_card_count",i)]],0
            )
          )),
        value = input[[paste0("Deck_select_card_count",i)]]
      ),

      selectInput(
        inputId = paste0("Deck_select_main_or_side",i),
        label = paste0("Main side or both ",i),
        choices = c("Both", "Main", "Side"),
        selected = input[[paste0("Deck_select_main_or_side",i)]],
        multiple = FALSE
      )
      )
    })
  })
}, ignoreInit = FALSE, ignoreNULL = FALSE
)





observeEvent( NULL,{
  Result_filter_deck  <- reactive({
  URL_deck_list_df <- 
    filter_df_of_deck() %>%
    # Modern_result_df %>% 
    distinct(id,.keep_all = TRUE) %>% 
    mutate(Player = paste0("<a href='",AnchorUri,"'>",Player,"</a>")) %>% 
    select(
      id,Tournament ,Meta , Week , Date , Result  , Wins , Losses , Draws ,
      Player , #AnchorUri , 
      Archetype , Color , Companion ,
      ReferenceArchetype_Archetype  , Base_Archetype 
      ) %>% 
    mutate(across(where(is.character),as.factor)) %>% 
    column_to_rownames(var = "id")
 })

output$Deck_url_df <- renderDT(
  datatable(Result_filter_deck(),
            escape = FALSE ,
            extensions = c("Buttons",'RowGroup'),
            # ordering = FALSE,
            filter = "top"
  )
  # dom = 'tB'
)#,
  # filter = "top" ,
  # options = list(columnDefs = list(list(
  #   targets = c(7,8,17,18),
  #   render = JS(
  #     "function(data, type, row, meta) {",
  #     "return type === 'display' && data.length > 6 ?",
  #     "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
  #     "}")
  # )
  # )
  # ),
  # callback = JS('table.page(3).draw(false);')
  
  


}, ignoreInit = FALSE, ignoreNULL = FALSE)





################################################################################  
############################   Fetch optimizer  ###########################
################################################################################   
result_opti <- eventReactive({
  input$deck_list_fetch_opti
},{
  if(!is.null(input$deck_list_fetch_opti)){
    
    if (!(tools::file_ext(input$deck_list_fetch_opti$datapath) %in% c('csv',"txt"))){
      validate("file must be .txt or .csv")
    }
    result_opti <- Fetch_land_optimizer(
      input$deck_list_fetch_opti$datapath,
      Modern_card_DB,
      list_format =tools::file_ext(input$deck_list_fetch_opti$datapath)
    )
  }
}
)
observeEvent(req(result_opti()),{
  
  output$res_text_opti_fetch <- renderText({ result_opti()$text })
  
  output$efetch_opti_res_base <- renderDT({
    req(result_opti())
    datatable(result_opti()$result_base,
              rownames = FALSE,
              # filter = "top",
              options = list(
                dom = 'tB'
              )
              
    )}
  )
  output$efetch_opti_res_number_of_fetchable <- renderDT({
    req(result_opti())
    datatable(result_opti()$result,
              rownames = FALSE,
              # filter = "top",
              options = list(
                dom = 'tB'
              )
    )}
  )
})

}

shinyApp(ui, server)