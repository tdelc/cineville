date_special2 <- function(date){
  yy <- 22
  mm <- substr(date,4,5)
  dd <- substr(date,1,2)
  paste0(yy,mm,dd)
}

###
# Extraction nb pages
###

# URL à extraire
url <- "http://www.cinema-vendome.be/les_films/a_l_affiche"

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

  
page_html <- content(GET(url))

liste_films <- html_elements(page_html,xpath="//div[@class='alaffiche_box_film']")

for (i_film in 1:length(liste_films)){
  
  film <- liste_films[[i_film]]
  
  titre <- html_text2(html_element(film,xpath=".//h2"))
  titre <- str_split(titre,"\n")[[1]][1]

  url_film <- html_attr(html_elements(film,xpath=".//a"),"href")[1]
  url_film <- paste0("http://www.cinema-vendome.be",url_film)
  
  diffusion <- html_text(html_elements(film,xpath=".//h2/div"))
  titre <- str_remove(titre,diffusion)
  
  resume <- html_text(html_elements(film,xpath=".//div[@class='alaffiche_synopsis']"))
  
  auteurice <- html_text(html_elements(film,xpath=".//h4"))
  
  url_tickets <- html_attr(html_element(film,xpath=".//a[@title='Horaires']"),name = "href")
  url_tickets <- paste0("http://www.cinema-vendome.be",url_tickets)
  
  if (is.na(url_tickets)) next
  
  page_tickets <- content(GET(url_tickets))
  Sys.sleep(2)
  
  url_image <- html_attr(html_element(page_tickets,xpath="//div[@id='zone_contenu_film']//img"),name = "src")
  url_image <- paste0("http://www.cinema-vendome.be",url_image)
  
  liste_jours <- html_elements(page_tickets,xpath="//div[contains(@class,'film_ligne_horaire')]")
  
  for (jour in liste_jours){
    
    date <- html_text(html_element(jour,xpath = ".//div[@class='film_horaire_date']"))
    
    liste_heures <- html_text(html_elements(jour,xpath = ".//div[@class='film_horaire_seance']"))
    
    for (heure in liste_heures){
      
      if (nchar(heure) == 1) next
    
      tableau_films <- tableau_films %>% 
      add_row(TX_DATE_EXTRACT = date_special(today()),
              TX_NOM_CINEMA = "VENDOME",
              TX_TITRE = titre,
              TX_AUTEURICE = NA,
              TX_LANGUE_VO = NA,
              TX_LANGUE_DIFFUSION = diffusion,
              TX_DUREE = NA,
              TX_RESUME = resume,
              TX_DATE = date_special2(date),
              TX_HEURE = heure,
              TX_URL = url_film,
              TX_IMG = url_image)
      
    }
    
  }

}

BDD_VENDOME <- tableau_films
rm(tableau_films)
