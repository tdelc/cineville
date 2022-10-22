library(shiny)
library(DT)
library(shinyjs)
library(googledrive)
library(googlesheets4)
library(tidyverse)
library(lubridate)

options(dplyr.summarise.inform = FALSE)

source('functions.R')

values <- reactiveValues()
values$BDD_FILMS <- bdd_import()
values$RESET_ROW <- 0

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Prévoir les choix possible de l'UI
  observe({
    
    vec_cine <- unique(values$BDD_FILMS$TX_NOM_CINEMA)
    vec_date <- unique(values$BDD_FILMS$TX_DATE_AFFICHE_PLUS)
    
    updateCheckboxGroupButtons(
      session,
      "choix_cine",
      choices = vec_cine,
      selected = vec_cine,
      status = "primary",
      checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
    )
    
    updateCheckboxGroupButtons(
      session,
      "choix_date",
      choices = vec_date,
      selected = vec_date[1:5],
      status = "info",
      checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
    )
    
    DATE_EXTRACT <- max(values$BDD_FILMS$TX_DATE_EXTRACT)
    DATE_EXTRACT <- paste(
      substr(DATE_EXTRACT,5,6),
      substr(DATE_EXTRACT,3,4),
      substr(DATE_EXTRACT,1,2),
      sep="/"
    )
    
    output$title <- renderText({paste0(
      "Liste des films projectés"
    )})
    
    output$info <- renderText({
      "Chaque horaire indiqué concerne l'un des cinémas diffusant le film. Cliquez pour connaître la programmation par cinéma."
    })

    output$credit <- renderUI({
      HTML(paste0("Application développée par <a href='t.delclite@gmail.com'> Thomas Delclite </a>, dernière extraction des films en date du ",DATE_EXTRACT,". Code source disponible <a href='https://github.com/tdelc/cineville'> ici </a>."))
    })
    
  })
  
  # Table de synthese
  observe({
    vec_cine <- input$choix_cine
    vec_date <- input$choix_date
    values$BDD_SYNTHESE <- bdd_synthese_films(values$BDD_FILMS,substitute(TX_NOM_CINEMA %in% vec_cine & TX_DATE_AFFICHE_PLUS %in% vec_date))
  })
  
  # Table de détails sur les films
  observeEvent(input$tbl_cell_clicked,{

    if (is.null(input$tbl_rows_selected)){
      values$BDD_FILM = NULL
    }else{
      
      FILM <- pull(values$BDD_SYNTHESE[input$tbl_rows_selected,"TX_TITRE"])
      CINEMA <- str_split(pull( values$BDD_SYNTHESE[input$tbl_rows_selected,"TX_CINEMAS"])," / ",simplify = TRUE)
      
      # values$BDD_FILM <- bdd_listing_films(values$BDD_FILMS,substitute(TX_TITRE %in% FILM & TX_NOM_CINEMA %in% CINEMA))
      values$BDD_FILM <- bdd_listing_films(values$BDD_FILMS,substitute(TX_TITRE %in% FILM))
      
      real <- recherche_infos(values$BDD_FILMS,FILM,"TX_AUTEURICE")
      duree <- recherche_infos(values$BDD_FILMS,FILM,"TX_DUREE")
      resume <- recherche_infos(values$BDD_FILMS,FILM,"TX_RESUME")
      langue <- recherche_infos(values$BDD_FILMS,FILM,"TX_LANGUE_VO")
      image <- recherche_infos(values$BDD_FILMS,FILM,"TX_IMG")
      
      resume <- str_remove(resume,"^(<br/>)+")
      
      showModal( modalDialog(
        div(column(width = 7,
                   h2(FILM),
                   if (real != '' ) h4(paste0("Réalisation : ",real)),
                   if (duree != '' ) h4(paste0("Durée : ",duree)),
                   if (langue != '' ) h4(paste0("Langue originale : ",langue)),
                   if (resume != '' ) HTML(resume)
                   # if (resume != '' )
                   #   p(paste0("Résumé : ",liste_resume[1])),
                   # if (length(liste_resume)>1)
                   #     for (i_resume in 2:length(liste_resume))
                   #       br(),p(liste_resume[i_resume]),
                   # p()
        ),
        column(width = 5,
               if (image != '' ) img(src=image,width="100%")
        )),
        column(width = 12,
        renderDT(DT::datatable(values$BDD_FILM,
                               selection = 'none',
                               rownames = FALSE,
                               escape = FALSE,
                               extensions = c('FixedColumns',"FixedHeader"),
                               colnames = c('Date' = 'TX_DATE_AFFICHE'),
                               options = list(lengthChange = FALSE,pageLength = 100, info = FALSE,searching = FALSE, paging=FALSE,ordering=FALSE)
        ))
        )
        ,easyClose = TRUE,size = 'l')
      )
      
    }
  })
  
  output$tbl <- renderDT({

    DT::datatable(values$BDD_SYNTHESE, 
                  selection = 'single',
                  rownames = FALSE,
                  colnames = c('Titre du film' = 'TX_TITRE','Cinéma(s) disponible(s)' = 'TX_CINEMAS'),
                  extensions = c('FixedColumns',"FixedHeader"),
                  options = list(lengthChange = FALSE,pageLength = 100, info = FALSE,searching = TRUE, paging=FALSE, fixedHeader=TRUE,ordering=FALSE)
  )})
  
  
  # observe({
  #   if (nrow(values$BDD_SYNTHESE) == 0){
  #     hide("tbl")
  #   }else{
  #     show("tbl")
  #   }
  #   if (is.null(values$BDD_FILM)){
  #     hide("tbl2")
  #   }else{
  #     show("tbl2")
  #   }
  # })
  
})
