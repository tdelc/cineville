
###
# Activation d'un proxy
###

list_versions("chromedriver")

remDr <- rsDriver(browser='chrome', port=as.integer(round(runif(1,4000,4999),0)), chromever = "106.0.5249.61")$client
# remDr$open()


###
# Récupération automatique
###

# URL à extraire
base_url <- "https://cinema-palace.be/fr/en-salles"

liste_date <- rep(today(),10)+c(0:9)
liste_date <- str_remove_all(substr(liste_date,3,50),"-")

liste_url <- paste0(base_url,"?key=",liste_date)
# https://cinema-palace.be/fr/en-salles?key=220829

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
  
  date <- str_extract(url,"[0-9]+")
  
  remDr$navigate(url)
  
  pagesource <- remDr$getPageSource()
  page_html <- read_html(pagesource[[1]])
  
  # liste_films <- html_elements(page_html,xpath="//div[@class='col-xs-7 filmhuls']")
  liste_films <- html_elements(page_html,xpath="//div[@class='col-sm-12 film processed']")
  
  if (length(liste_films) == 0) next
  
  for (i_film in 1:length(liste_films)){
    
    film <- liste_films[[i_film]]
    
    titre <- html_text(html_elements(film,xpath=".//h3"),trim = TRUE)[1]
    
    url_film <- html_attr(html_elements(film,xpath=".//a"),"href")[1]
    url_film <- paste0("https://cinema-palace.be",url_film)
    
    url_image <- html_attr(html_elements(film,xpath=".//div[@class='film-image']"),"style")
    url_image <- paste0("https://cinema-palace.be",str_split(url_image,"'")[[1]][2])
    
    auteurice <- html_text(html_elements(film,xpath=".//h4"))
    auteurice <- str_squish(auteurice)
    
    langue <- html_text(html_elements(film,xpath=".//span[@class='land']"))
    duree <- html_text(html_elements(film,xpath=".//span[@class='duur']"))[1]
    diffusion <- html_text(html_elements(film,xpath=".//span[@class='taal']"))
    langue <- str_squish(langue)
    duree <- str_squish(duree)
    diffusion <- str_squish(diffusion)
    
    resume <- html_text(html_elements(film,xpath=".//p"))
    resume <- str_squish(resume)
    
    liste_heures <- html_text(html_elements(film,xpath=".//li/a"))
    
    for (heure in liste_heures){
      tableau_films <- tableau_films %>% 
        add_row(TX_DATE_EXTRACT = date_special(today()),
                TX_NOM_CINEMA = "PALACE",
                TX_TITRE = titre,
                TX_AUTEURICE = auteurice,
                TX_LANGUE_VO = langue,
                TX_LANGUE_DIFFUSION = diffusion,
                TX_DUREE = duree,
                TX_RESUME = resume,
                TX_DATE = date,
                TX_HEURE = heure,
                TX_URL = url_film,
                TX_IMG = url_image)
      
    }
    
  }
  Sys.sleep(2)
}

BDD_PALACE <- tableau_films
rm(tableau_films)
