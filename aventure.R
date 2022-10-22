
# URL à extraire
base_url <- "https://www.cinema-aventure.be/catalogue/?&period=3"

###
# Récupération automatique
###

# Déclaration du tableau
tableau_films <- tibble(
  TX_DATE_EXTRACT = character(),
  TX_NOM_CINEMA = character(),
  TX_TITRE = character(),
  TX_AUTEURICE = character(),
  TX_LANGUE_VO = character(),
  TX_LANGUE_DIFFUSION = character(),
  TX_DUREE = character(),
  TX_RESUME = character(),
  TX_DATE = character(),
  TX_HEURE = character(),
  TX_URL = character(),
  TX_IMG = character()
)


page_html <- content(GET(base_url))
# page_html <- content(GET(base_url,proxy))
Sys.sleep(2)

liste_films <- html_elements(page_html,xpath = "//div[@class='product']")

for (i_film in 1:length(liste_films)){

  film <- liste_films[[i_film]]
  
  titre <- html_text(html_element(film,xpath = ".//div[contains(@class,'product-title')]"))
  
  type <- html_text(html_element(film,xpath = ".//div[contains(@class,'product-meta')]"), trim = TRUE)
  
  url_film <- html_attr(html_element(film,xpath = ".//div[contains(@class,'product-title')]/a"),'href')
  url_film <- paste0("https://www.cinema-aventure.be",url_film)
  
  page_film <- content(GET(url_film))
  # page_film <- content(GET(url_film,proxy))
  Sys.sleep(2)
  
  resume <- paste(html_text(html_elements(page_film,xpath = "//p"), trim = TRUE),collapse = "<br/><br/>")
  
  realisation <- html_text(html_element(page_film,xpath = "//h6[preceding-sibling::span[text()='Réalisation']]"),trim = TRUE)
  
  duree <- html_text(html_element(page_film,xpath = "//li[contains(text(),'Durée :')]"),trim = TRUE)
  duree <- str_remove(duree,"Durée : ")
  
  url_image <- html_attr(html_element(page_film,xpath = "//img[@class='img-fluid']"),'src')
  
  liste_seances <- html_elements(page_film,xpath = "//li[contains(@class,'movideDate')]")
  
  if (length(liste_seances) == 0) next
  
  for (i_seance in 1:length(liste_seances)){
  
    seance <- liste_seances[[i_seance]]
    
    date_heure <- html_text(html_element(seance,xpath = ".//a[@class='sessionDate']"), trim = TRUE)
    
    date <- str_split(date_heure," - ")[[1]][1]
    date <- date_special(dmy(date))
    
    heure <- str_split(date_heure," - ")[[1]][2]
    
    infos_sup <- html_text(html_element(seance,xpath = ".//div[@class='sessionInfo']"), trim = TRUE)
    diffusion <- str_split(infos_sup," \\| ")[[1]][2]
    if (!is.na(str_split(infos_sup," \\| ")[[1]][3])){
      diffusion <- paste0(diffusion," ST ",str_split(infos_sup," \\| ")[[1]][3])
    }
    
    tableau_films <- tableau_films %>% 
      add_row(TX_DATE_EXTRACT = date_special(today()),
              TX_NOM_CINEMA = "AVENTURE",
              TX_TITRE = titre,
              TX_AUTEURICE = realisation,
              TX_LANGUE_VO = NA,
              TX_LANGUE_DIFFUSION = diffusion,
              TX_DUREE = duree,
              TX_RESUME = resume,
              TX_DATE = date,
              TX_HEURE = heure,
              TX_URL = url_film,
              TX_IMG = url_image)
  }
}

BDD_AVENTURE <- tableau_films
rm(tableau_films)
