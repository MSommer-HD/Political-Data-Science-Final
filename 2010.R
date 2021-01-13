#############

## 2010

rm(list = ls())
library(dplyr)
library(readstata13)
library(scales)
library(hrbrthemes)



dat <- read.dta13("2010.dta")
View(df.zukunft.total)



df.zukunft.total <- dat %>%
  select(v45_5)%>%
  drop_na()

key <- df.zukunft.total$v45_5[3]

zukunft.share <- nrow(filter(df.zukunft.total,v45_5 == key)) /nrow(df.zukunft.total)


df.wl.total <- dat %>%
  select(v45_1)%>%
  drop_na()

wl.share <- nrow(filter(df.wl.total,v45_1 == key)) /nrow(df.wl.total)


df.wett.total <- dat %>%
  select(v45_3)%>%
  drop_na()

wett.share <- nrow(filter(df.wett.total,v45_3 == key)) /nrow(df.wett.total)


df.al.total <- dat %>%
  select(v45_2)%>%
  drop_na()


al.share <- nrow(filter(df.al.total,v45_2 == key)) /nrow(df.al.total)

df.soz.total <- dat %>%
  select(v45_4) %>%
  drop_na()

soz.share <- nrow(filter(df.soz.total,v45_4 == key)) /nrow(df.soz.total)



df.2010 <- data.frame(measure=c("Master future challenges", "Secure welfare" , "Secure competitiveness", "Create jobs", "Increase social justice"),
                      value=c(round(zukunft.share*100), round(wl.share*100) , round(wett.share*100), round(al.share*100), round(soz.share*100)))
df.2010$year<- 2010

position <- c("Master future challenges", "Secure welfare" , "Secure competitiveness", "Create jobs", "Increase social justice")
df.2010$measure <- factor(df.2010$measure, levels = rev(position))

ggplot(df.2010, aes(x = measure, y=value, fill=measure)) + 
  scale_x_discrete(" ",name = "")+
  labs(title = "Environmental protection as basis for political decisions", subtitle = "between 2014 and 2016")+
  scale_y_continuous("Perecent")+
  geom_bar(stat = 'identity', width = 0.6) +
  scale_fill_manual(values=c("#006600", "#FFCC00", "#003366","#3399FF","#00CC00"))+
  theme_ipsum_rc()+
  theme(legend.position="bottom", aspect.ratio = 1.5/1, legend.title=element_blank(), legend.text = element_text(size = 10))+
  geom_text(aes(label = value), size = 4, colour = "grey99", position = position_stack(vjust = 0.95))

save(df.2010, file= "df.2010")
