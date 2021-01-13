######## Moritz Sommerlad
######## Final Script 1
######## Newspaper Articles Taz 

#Loading Libraries
library(rvest) # used to parse web pages
library(RCurl) # used to download web pages
library(tidyverse) # used to manipulate characters
library(data.table) # used to put data frames together
library(stringr)
library(regex)
#Clear
rm(list = ls())

####################################################
## Url setup
base.url <- "https://taz.de/!s=&Titel="
end.url <- "&eTagAb=2000-01-01/?search_page="
##########################
topic <- list("CO2","Umwelt","Verschmutzung","Klima","Nachhaltig", "Plastik", "Recycling")
dfs <- list()
total.hits <- 0

for (i in topic) {
  archive <- paste0("TAZ -- ",i)
  if(!dir.exists(archive)) {
    dir.create(archive)}

  
  base.url <- "https://taz.de/!s=&Titel="
  end.url <- "&eTagAb=2000-01-01&isWochenende=1/?search_page="
  
  page <- 0
  max.page <- 0
  repeat {
    if (page > max.page) {break} 
    
    
    
    title <- paste0("./",archive,"/","Page ",page," -- ", Sys.Date())
    final.url <- paste0(base.url,i,end.url,page)

    site <- read_html(final.url)
    save(site, file = title)
    

    if(page == 0) {
      node <- site %>%
        html_nodes("h2 a") 
      hits <-  xml_attrs(node[[1]]) %>%
        str_extract_all("von [[:digit:]]*") %>%
        unlist() %>%
        str_split(" ")%>%
        `[[`(1) %>%
        `[[`(2) %>%
        as.numeric()
      max.page <- ceiling(hits/20)-1
        
      print(paste(hits, " Treffer verfügbar"))
    }
    
    titels <- site %>%
      html_nodes("h3") %>% 
      html_text(trim = T)
    
    date <- site %>%
      html_nodes(".date") %>% 
      html_text(trim = T)

    
    excerpt <- site %>%
      html_nodes(".abstract") %>% 
      html_text(trim = T)
    
    
    
    if (length(titels)<length(date)) {
      missing <- length(date) - length(titels)
      titels <- rep(append(titels,"na"), times = missing)
    }


    key <- rep(i, times = length(titels))
    df <- data.frame(title = titels,
                     date = date,
                     key = key)
    
    
    
    dfs[[(length(dfs) + 1)]] <- df
    
    print(paste("Working on",i,"page:", page, "of", max.page))
    Sys.sleep(10 + runif(1)*10)
    page <-  page +1
  }
  total.hits <- hits + total.hits
}
print(paste("Loop done. Total Hits:",total.hits))

taz.df  <- bind_rows(dfs) %>%
  unique()

save(taz.df, file = "taz.df")

    