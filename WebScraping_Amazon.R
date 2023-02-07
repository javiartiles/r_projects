### TRABAJO FINAL TEXT MINING - MACHINE LEARNING III - 4º E3 ANALYTICS



### ----------------------------- TEXT MINING AMAZON REVIEWS (REPETIDOR WIFI) ----------------------------- ###

scrape_amazon <- function(ASIN, numpag){
  
  reviews_all<-NULL
  for (page in 1:numpag){
  
    
  url<- paste0("https://www.amazon.es/product-reviews/B00A0VCJPI/?pageNumber=1", ASIN,"/?pageNumber=",page)
  doc <- read_html(url)
  
  titulo<-doc %>% 
    html_nodes("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
    html_text()%>%
    gsub("\n", "", .) %>% 
    trimws()
  
  fecha<-doc %>%
    html_nodes("[data-hook='review-date']") %>%
    html_text()%>%
    str_extract(., "[0-9]+ de [a-z]+ de [0-9]+")%>%
    gsub(" de ","/", .)%>%
    parse_date_time(., orders="dmy")     
  
  comentario<-doc %>%
    html_nodes("[class='a-size-base review-text review-text-content']") %>%
    html_text()%>%
    gsub("\n", "", .) %>% 
    trimws()
  
  stars<-doc %>%
    html_nodes("[data-hook='review-star-rating']") %>%
    html_text()%>%
    str_extract(., "[0-9],[0-9]")%>%
    gsub(",", ".", .)%>% as.numeric()
  
  contenido_pag<-data.frame(titulo=titulo, fecha=fecha, texto=comentario, rating=stars, stringsAsFactors = F)
  reviews_all<-rbind(reviews_all,contenido_pag)
  # para que no nos detecten, esperamos 3 seg tras cada lectura de HTML
  # ponemos tambien un descanso extra cada 3 paginas leidas
  
  Sys.sleep(3) # descanso de 3 sec
  print(paste0("lectura", page))
  
  if((page %% 3) == 0){ 
    
    message("Descanso")
    
    Sys.sleep(2) # tomamos un descanso extra de 2 sec
  }
  
  }
  return (reviews_all)
}