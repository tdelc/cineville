setwd("C:/Users/thomas.delclite/OneDrive - GCloud Belgium/Privé/R/shiny cineville")

library(googledrive)
library(googlesheets4)
library(tidyverse)
library(XML)
library(httr)
library(rvest)
library(RSelenium)
library(binman)
library(lubridate)

date_special <- function(date){
  yy <- substr(year(date),3,4)
  mm <- str_pad(month(date),2,pad="0")
  dd <- str_pad(day(date),2,pad="0")
  paste0(yy,mm,dd)
}

# token_gs4 <- gs4_token()
# save(token_gs4,file="token_gs4")
load("token_gs4")
gs4_auth(token=token_gs4)

# token_gd <- googledrive::drive_token()
# save(token_gd,file="token_gd")
load("token_gd")
drive_auth(token = token_gd)

# range_flood(google_table, "PALACE",range = "A2:Z10000")
# range_flood(google_table, "FLAGEY",range = "A2:Z10000")
# range_flood(google_table, "GALERIES",range = "A2:Z10000")
# range_flood(google_table, "VENDOME",range = "A2:Z10000")

source("Palace.R")
source("flagey.R")
source("galeries.R")
source("vendome.R")
source("aventure.R")

# Réunion des BDD
BDD_FILMS <- rbind(BDD_PALACE,BDD_GALERIES,
                   BDD_FLAGEY,BDD_VENDOME,
                   BDD_AVENTURE)

### Nettoyage

# Titre en majuscule  
BDD_FILMS$TX_TITRE <- str_to_upper(BDD_FILMS$TX_TITRE)
# Titre sans accent
BDD_FILMS$TX_TITRE <- stringi::stri_trans_general(BDD_FILMS$TX_TITRE,id = "Latin-ASCII")
# Nettoye Real
BDD_FILMS$TX_AUTEURICE <- str_remove(BDD_FILMS$TX_AUTEURICE,"Réalisé par")

# Date à afficher
DATE_LUBRI <- ymd(BDD_FILMS$TX_DATE)

BDD_FILMS$TX_DATE_AFFICHE <- paste(
  str_to_title(wday(DATE_LUBRI,label = TRUE,abbr = FALSE)),
  day(DATE_LUBRI),
  month(DATE_LUBRI,label = TRUE,abbr = TRUE)
  # substr(BDD_FILMS$TX_DATE,5,6),
  # substr(BDD_FILMS$TX_DATE,3,4),
  # substr(BDD_FILMS$TX_DATE,1,2),
  # sep="/"
)

load('id_sheet_films')
google_table<-gs4_get(id_sheet_films)

sheet_append(google_table,BDD_FILMS,sheet = "COMPLET")
