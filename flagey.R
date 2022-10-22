###
# Extraction nb pages
###

# URL à extraire
base_url <- "https://www.flagey.be/fr/program/2-cinema"
"https://www.flagey.be/fr/program/2-cinema?_current_page=4&_nb_items_per_page=12"

page_html <- content(GET(base_url))

nb_pages <- html_text(html_elements(page_html,xpath="//li[@class='pager__item']"))
nb_pages <- as.numeric(nb_pages[length(nb_pages)])

# Préparation des url

liste_url <- paste0(base_url,"?_current_page=",1:nb_pages)

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
  
  page_html <- content(GET(url))
  
  liste_films <- html_elements(page_html,xpath="//li[contains(@class,'item--activity')]")
  
  if (length(liste_films) == 0) next
  
  for (i_film in 1:length(liste_films)){
    
    film <- liste_films[[i_film]]
    
    titre <- html_text(html_elements(film,xpath=".//h3"))
    
    url_film <- html_attr(html_elements(film,xpath="./a"),"href")[1]
    
    auteurice <- html_text(html_elements(film,xpath=".//h4"))
    
    url_tickets <- html_attr(html_element(film,xpath=".//a[@class='btn btn--ticket']"),name = "href")
    
    if (is.na(url_tickets)) next
    
    page_tickets <- content(GET(url_tickets))
    
    # div qui contiennent le span ticket
    liste_tickets <- html_elements(page_tickets,xpath="//time")
    
    for (ticket in liste_tickets){
      
      heure <- html_text(html_elements(ticket,xpath=".//span[@class='infos__hour']"))
      date <- str_squish(str_remove(html_text(ticket),heure))
      
      tableau_films <- tableau_films %>% 
        add_row(TX_DATE_EXTRACT = date_special(today()),
                TX_NOM_CINEMA = "FLAGEY",
                TX_TITRE = titre,
                TX_AUTEURICE = auteurice,
                TX_LANGUE_VO = NA,
                TX_LANGUE_DIFFUSION = NA,
                TX_DUREE = NA,
                TX_RESUME = NA,
                TX_DATE = date_special(dmy(date)),
                TX_HEURE = heure,
                TX_URL = url_film,
                TX_IMG = NA)
      
    }

    Sys.sleep(2)
  }
}

BDD_FLAGEY <- tableau_films
rm(tableau_films)
