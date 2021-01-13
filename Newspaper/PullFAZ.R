######## Moritz Sommerlad
######## Final Script 1
######## Newspaper Articles FAZ 

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
base.url <- "https://epetitionen.bundestag.de/epet/petuebersicht/suche.abgeschlossen.$$$.batchsize.10.page."
mid.url <- ".html?_charset_=UTF-8&text="
end.url <- "&page=1"
##########################
topic <- list("CO2", "Umwelt","Verschmutzung","Klima","Nachhaltig", "Plastik", "Recycling","Umwelt")

dfs <- list()
total.hits <- 0
for (i in topic) {
  archive <- paste0("FAZ -- ",i)
  if(!dir.exists(archive)) {
    dir.create(archive)}

  
  start.url <- "http://www.faz-biblionet.de/faz-portal/faz-archiv?q="
  
  mid.url <- "&source=FAZT%2CFAZ%2CFAZH&max=30&sort=&offset="
  
  end.url <- "&searchIn=TI&_ts=1609670609661&CN=C4EUGE&DT_from=01.01.2000&DT_to=03.01.2021&timeFilterType=0#hitlist"
    
  offset <- 0
  hits <-  0
  repeat {
      if (offset > hits) {break} 
      
      print(paste("Working on",i,"page:", offset/30, "of", ceiling(hits/30)))
        
      
      final.url <- paste0(start.url,i,mid.url,offset,end.url)
      
      
      title <- paste0("./",archive,"/","Page ",offset/30," -- ", Sys.Date())
      
      if(!file.exists(title)){
        tmp <- getURL(final.url)
        write(tmp, title)
        site <- read_html(final.url)
        Sys.sleep(10 + runif(1)*10)
      } else {
        site <- read_html(title)
      }
      
      if(offset == 0) {
        hits <- site %>%
          html_nodes(".summary:nth-child(1) .boxNum") %>% 
          html_text(trim = T) %>%
          str_split(" ") %>%
          unlist() %>%
          `[[`(1) 
        hits <-  gsub("\\.","",hits) %>%
          as.numeric()
        print(paste(hits, " Treffer verfügbar"))
      }
      
      titels <- site %>%
        html_nodes("h3 a") %>% 
        html_text(trim = T)
      
       date <- site %>%
        html_nodes(".summary-block li:nth-child(1)") %>% 
        html_text(trim = T) %>%
        str_sub(end = -2)
      
      excerpt <- site %>%
        html_nodes(".abstract") %>% 
        html_text(trim = T)
  
      
      key <- rep(i, times = length(titels))
      
      
      df <- data.frame(title = titels,
                       excerpt = excerpt,
                       date = date,
                       key = key)
      
      
      
      dfs[[(length(dfs) + 1)]] <- df
      offset <-  offset +30 
  }
  total.hits <- hits + total.hits
}
print(paste("Loop done. Total Hits:",total.hits))
faz.df  <- bind_rows(dfs) %>%
  unique()

save(faz.df, file = "faz.df")


     