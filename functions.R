bdd_import <- function(){
  
  #### Chargement via google
  
  load("token_gs4")
  load("token_gd")
  load('id_sheet_films')
  
  gs4_deauth() # TEST
  gs4_auth(token=token_gs4)
  drive_auth(token = token_gd)
  google_table<-gs4_get(id_sheet_films)
  
  # Chargement de la table
  BDD_FILMS <- read_sheet(google_table,"COMPLET")

  ### Filtre
  
  # Pour chaque cinéna, ne garder que la dernière date d'extraction
  BDD_FILMS <- BDD_FILMS %>% 
    group_by(TX_NOM_CINEMA) %>% 
    mutate(TX_DATE_EXTRACT_LAST = max(TX_DATE_EXTRACT)) %>% 
    filter(TX_DATE_EXTRACT == TX_DATE_EXTRACT_LAST) %>% 
    ungroup()
  
  # Ne garder que les dates après aujourd'hui 
  DATE_NOW <- as.numeric(paste0(year(now()) - 2000,
                                month(now()),
                                day(now())))
  
  # Belle date pour aujourd'hui et demain
  BDD_FILMS <- BDD_FILMS %>%
    mutate(TX_DATE_AFFICHE = ifelse(as.numeric(TX_DATE) == DATE_NOW,"Aujourd'hui",TX_DATE_AFFICHE)) %>% 
    mutate(TX_DATE_AFFICHE = ifelse(as.numeric(TX_DATE) == DATE_NOW+1,"Demain",TX_DATE_AFFICHE))
  
  # date spéciale pour écarter les films de plus de 1 semaine
  # Date plus + 1 semaine
  TX_DATE_MAX <- BDD_FILMS %>% 
    filter(as.numeric(TX_DATE) == DATE_NOW+7) %>% 
    select(TX_DATE_AFFICHE) %>% pull()
  
  BDD_FILMS <- BDD_FILMS %>%
    mutate(TX_DATE_AFFICHE_PLUS = ifelse(as.numeric(TX_DATE) > DATE_NOW+7,paste0("Apres le ",TX_DATE_MAX),TX_DATE_AFFICHE)) %>% 
    mutate(TX_DATE_PLUS = ifelse(as.numeric(TX_DATE) > DATE_NOW+7,DATE_NOW+8,TX_DATE))
  
  BDD_FILMS <- BDD_FILMS %>% 
    filter(as.numeric(TX_DATE) >= DATE_NOW)
  
  BDD_FILMS
}

bdd_synthese_films <- function(BDD_FILMS,filtre = TRUE){
  
  BDD_SYNTHESE <- BDD_FILMS %>%
    filter(eval(filtre)) %>% 
    select(TX_NOM_CINEMA,TX_TITRE,TX_DATE_PLUS,TX_DATE_AFFICHE_PLUS,TX_HEURE) %>% 
    distinct() %>%
    arrange(TX_TITRE,TX_DATE_PLUS,TX_HEURE) %>% 
    group_by(TX_TITRE,TX_DATE_PLUS,TX_DATE_AFFICHE_PLUS) %>% 
    summarise(TX_HEURES = paste(TX_HEURE,collapse = " / ")) %>%
    ungroup() %>% 
    arrange(as.numeric(TX_DATE_PLUS)) %>%
    select(-TX_DATE_PLUS) %>%
    pivot_wider(names_from = "TX_DATE_AFFICHE_PLUS",values_from = "TX_HEURES") %>% 
    arrange(TX_TITRE)

  # mutate(TX_HEURES = ifelse(str_count(TX_HEURES,"/") < 5,TX_HEURES,str_count(TX_HEURES,"/"))) %>% 
    

  BDD_CINEMAS <- BDD_FILMS %>%
    filter(eval(filtre)) %>% 
    select(TX_NOM_CINEMA,TX_TITRE) %>% 
    distinct() %>% 
    group_by(TX_TITRE) %>% 
    summarise(TX_CINEMAS = paste(unique(TX_NOM_CINEMA),collapse = " / "))
  
  BDD_SYNTHESE <- BDD_SYNTHESE %>%
    left_join(BDD_CINEMAS,by="TX_TITRE") %>% 
    relocate(TX_CINEMAS,.after=TX_TITRE)
  
  BDD_SYNTHESE
  
}

bdd_listing_films <- function(BDD_FILMS,filtre = TRUE){
  
  BDD_SYNTHESE <- BDD_FILMS %>%
    # select(TX_NOM_CINEMA,TX_TITRE,TX_DATE,TX_DATE_AFFICHE,TX_HEURE,TX_AUTEURICE,TX_LANGUE_VO,TX_LANGUE_DIFFUSION,TX_DUREE,TX_RESUME) %>% 
    filter(eval(filtre)) %>% 
    select(TX_NOM_CINEMA,TX_DATE,TX_DATE_AFFICHE,TX_LANGUE_DIFFUSION,TX_HEURE,TX_URL) %>% 
    distinct() %>%
    arrange(TX_NOM_CINEMA,TX_DATE,TX_HEURE) %>% 
    group_by(TX_NOM_CINEMA,TX_DATE,TX_DATE_AFFICHE,TX_LANGUE_DIFFUSION,TX_URL) %>% 
    summarise(TX_HEURES = paste(TX_HEURE,collapse = " / ")) %>% 
    ungroup() %>% 
    mutate(TX_NOM_CINEMA = ifelse(is.na(TX_LANGUE_DIFFUSION),TX_NOM_CINEMA,paste0(TX_NOM_CINEMA," (",TX_LANGUE_DIFFUSION,")"))) %>% 
    mutate(TX_HEURES = paste0("<a href='",TX_URL,"'>",TX_HEURES,"</a>")) %>% 
    arrange(as.numeric(TX_DATE)) %>% 
    select(-TX_DATE,-TX_URL,-TX_LANGUE_DIFFUSION) %>% 
    # pivot_wider(names_from = "TX_DATE_AFFICHE",values_from = "TX_HEURES") %>% 
    pivot_wider(names_from = "TX_NOM_CINEMA",values_from = "TX_HEURES")
  
  BDD_SYNTHESE
  
}


recherche_infos <- function(BDD_FILMS,titre,info){
  
  temp <- unique(pull(BDD_FILMS[BDD_FILMS$TX_TITRE == titre,info]))

  temp <- temp[!is.na(temp)]

  temp <- temp[1]

  temp <- ifelse(is.na(temp),"",temp)
}
