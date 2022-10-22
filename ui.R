
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(shinyjs)
library(googledrive)
library(googlesheets4)
library(tidyverse)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  theme = shinytheme("paper"),
  
    useShinyjs(),

    # Application title
    img(src='cineville.png', align = "center", width="30%",inline= TRUE),
    #titlePanel("Liste des films"),
    # h1(textOutput("text")),
    br(),
    
              checkboxGroupButtons(
                inputId = "choix_cine",
                label = "Filtrez vos cinémas",
                choices = c("CHARGEMENT..."),
                justified = TRUE, status = "primary",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
              ),
              
              checkboxGroupButtons(
                inputId = "choix_date", 
                label = "Filtrez vos dates",
                choices = c("CHARGEMENT..."),
                justified = FALSE, status = "info",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
              ),
    
    # tabsetPanel(
    #   tabPanel("Test",
    mainPanel(width = 12,
              
       h3(textOutput("title")),
       h5(textOutput("info")),
       DTOutput('tbl'),
       br(),
       p(uiOutput("credit")),
       )
    # ))  
))
