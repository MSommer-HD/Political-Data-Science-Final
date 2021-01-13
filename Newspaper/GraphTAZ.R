######## Moritz Sommerlad
######## Final Script 3
######## Working with TAZ

#Loading Libraries
library(rvest) # used to parse web pages
library(RCurl) # used to download web pages
library(tidyverse) # used to manipulate characters
library(data.table) # used to put data frames together
library(stringr)
library(scales)
library(hrbrthemes)
#Clear
rm(list = ls())

load("taz.df")

taz.work <- mutate(taz.df,year = as.numeric(sub('.*(\\d{4}).*', '\\1', taz.df$date)))
taz.work <- filter(taz.work, year < 2021)



taz.summary.key <-taz.work%>%
  group_by(year,key) %>%
  summarise(count = n())

taz.summary.year <- subset(taz.work, select=-c(key)) %>% 
  unique() %>%
  group_by(year) %>%
  summarise(count = n())

# Just by year
ggplot(taz.summary.year, aes(x = year,y = count)) + 
  geom_point() +
  labs(title = "Articles related to the environemt", subtitle = "by Die Tageszeitung")+
  scale_x_continuous("Year")+
  scale_y_continuous("Number of articles")+
  theme_ipsum_rc() +
  geom_smooth(method="lm", size=1, color="darkblue",alpha = .3)
ggsave("tazyear.png")
#Split by key

ggplot(taz.summary.key, aes(x = year,y = count)) + 
  geom_point() +
  labs(title = "Articles related to the environemt", subtitle = "by Die Tageszeitung")+
  scale_x_continuous("Year")+
  scale_y_continuous("Number of articles")+
  theme_ipsum_rc() +
  facet_wrap(~key, scales = "free")+
  geom_smooth(method="lm", size=1, color="darkblue",alpha = .3)
ggsave("tazkeyyear.png")
