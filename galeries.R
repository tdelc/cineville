
# URL à extraire
base_url <- "https://galeries.be/fr/"

liste_date <- rep(today(),10)+c(0:9)

liste_url <- paste0(base_url,"?from=",liste_date)

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

for (url in liste_url){
  
  date <- date_special(str_extract(url,"[0-9-]+"))
  
  page_html <- content(GET(url))
  # page_html <- content(GET(url,proxy))
  
  liste_films <- html_elements(page_html,xpath = "//li[1]//li[@class='event-single mini']")
  
  if (length(liste_films) == 0) next
  
  for (i_film in 1:length(liste_films)){
    
    film <- liste_films[[i_film]]
    
    titre <- html_text(html_elements(film,xpath=".//div[@class='wrap-title']/a"))
    titre <- str_squish(titre)
    
    url_film <- html_attr(html_elements(film,xpath=".//div[@class='wrap-title']/a"),"href")
    
    auteurice <- html_text(html_elements(film,xpath=".//div[@class='wrap-title']/div"))
    auteurice <- str_squish(auteurice)
    
    diffusion <- html_text(html_elements(film,xpath=".//span[@class='version']"))

    heure <- html_text(html_elements(film,xpath=".//time"))
    
    tableau_films <- tableau_films %>% 
      add_row(TX_DATE_EXTRACT = date_special(today()),
              TX_NOM_CINEMA = "GALERIES",
              TX_TITRE = titre,
              TX_AUTEURICE = auteurice,
              TX_LANGUE_VO = NA,
              TX_LANGUE_DIFFUSION = diffusion,
              TX_DUREE = NA,
              TX_RESUME = NA,
              TX_DATE = date,
              TX_HEURE = heure,
              TX_URL = url_film,
              TX_IMG = NA)
      
  }
  Sys.sleep(2)
}

BDD_GALERIES <- tableau_films
rm(tableau_films)
