#############

## 2014

rm(list = ls())
library(dplyr)
library(readstata13)
library(scales)
library(hrbrthemes)


dat <- read.dta13("2014.dta")



key <- "Ein hinreichender Umwelt- und Klimaschutz stellt für diese Aufgabe eine grundlegende Bedingung dar"


df.zukunft.total <- dat %>%
  select(f16e)

zukunft.share <- nrow(filter(df.zukunft.total,f16e == key)) /nrow(df.zukunft.total)

df.wl.total <- dat %>%
  select(f16a)

wl.share <- nrow(filter(df.wl.total,f16a == key)) /nrow(df.wl.total)


df.wett.total <- dat %>%
  select(f16c)

wett.share <- nrow(filter(df.wett.total,f16c == key)) /nrow(df.wett.total)


df.al.total <- dat %>%
  select(f16b)

al.share <- nrow(filter(df.al.total,f16b == key)) /nrow(df.al.total)

df.soz.total <- dat %>%
  select(f16d) 

soz.share <- nrow(filter(df.soz.total,f16d == key)) /nrow(df.soz.total)




df.2014 <- data.frame(measure=c("Master future challenges", "Secure welfare" , "Secure competitiveness", "Create jobs", "Increase social justice"),
                      value=c(round(zukunft.share*100), round(wl.share*100) , round(wett.share*100), round(al.share*100), round(soz.share*100)))
df.2014$year<- 2014

position <- c("Master future challenges", "Secure welfare" , "Secure competitiveness", "Create jobs", "Increase social justice")


df.2014$measure <- factor(df.2014$measure, levels = rev(position))

ggplot(df.2014, aes(x = measure, y=value, fill=measure)) + 
  scale_x_discrete(" ",name = "")+
  labs(title = "Environmental protection as basis for political decisions", subtitle = "between 2014 and 2016")+
  scale_y_continuous("Perecent")+
  geom_bar(stat = 'identity', width = 0.6) +
  scale_fill_manual(values=c("#006600", "#FFCC00", "#003366","#3399FF","#00CC00"))+
  theme_ipsum_rc()+
  theme(legend.position="bottom", aspect.ratio = 1.5/1, legend.title=element_blank(), legend.text = element_text(size = 10))+
  geom_text(aes(label = value), size = 4, colour = "grey99", position = position_stack(vjust = 0.95))

save(df.2014, file= "df.2014")





