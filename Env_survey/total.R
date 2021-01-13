#############

## Pulling all together


rm(list = ls())
library(dplyr)
library(readstata13)
library(scales)
library(hrbrthemes)
library(viridis)
library(lubridate)

load("df.2010")
load("df.2012")
load("df.2014")
load("df.2016")
load("df.2018")

total <- rbind(df.2010, df.2012, df.2014,df.2016,df.2018) 


ggplot(total, aes(x = as.numeric(year), y=value, fill=year)) + 
  geom_bar(position="dodge", stat="identity") +
  #scale_y_continuous("Perecent")+
  ylab("Percent")+
  scale_x_discrete("")+
  ylim(0, 80)+
  labs(title = "Environmental protection as basis for political decisions", subtitle = "between 2010 and 2018")+
  facet_wrap(~measure) +
  theme_ipsum_rc()+
  theme(legend.position="bottom", aspect.ratio = 1.3/1, legend.title=element_blank(), legend.text = element_text(size = 12),
        legend.key.width=unit(1.5,"cm"))+
  geom_text(aes(label = value), size = 3, colour = "grey", position = position_stack(vjust = 0.9))+


position <- c("Master future challenges", "Secure welfare" , "Secure competitiveness", "Create jobs", "Increase social justice")

total$measure <- factor(total$measure, levels = rev(position))


