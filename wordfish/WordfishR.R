
##### Creating wordfish scores for the party manifestos



rm(list = ls())
library(tifyvers)
library(readstata13)
library(scales)
library(hrbrthemes)
library(manifestoR)
library(quanteda)
library(quanteda.textmodels)
library(hrbrthemes)
library(tidyverse)

mp_setapikey("key.txt")
dat.mani <- filter(mp_maindataset(),countryname == "Germany")
View(filter(dat.mani, date > 200000))

dat.corpus.green <- mp_corpus(countryname == "Germany" & edate >as.Date("2000-01-01"), codefilter =c(501))



q.corpus <- corpus(dat.corpus.green)


docvars(q.corpus, field = "year") <- substr(docvars(q.corpus, field = "date"), 0, 4)



docvars(q.corpus, "party") <- str_replace_all(docvars(q.corpus, "party"),"41113", "Greens")
docvars(q.corpus, "party") <- str_replace_all(docvars(q.corpus, "party"),"41221", "The Left")
docvars(q.corpus, "party") <- str_replace_all(docvars(q.corpus, "party"),"41320", "SPD")
docvars(q.corpus, "party") <- str_replace_all(docvars(q.corpus, "party"),"41420", "FDP")
docvars(q.corpus, "party") <- str_replace_all(docvars(q.corpus, "party"),"41521", "CDU/CSU")
docvars(q.corpus, "party") <- str_replace_all(docvars(q.corpus, "party"),"41223", "The Left")
docvars(q.corpus, "party") <- str_replace_all(docvars(q.corpus, "party"),"41952", "Pirates")
docvars(q.corpus, "party") <- str_replace_all(docvars(q.corpus, "party"),"41222", "The Left")
docvars(q.corpus, "party") <- str_replace_all(docvars(q.corpus, "party"),"41953", "AfD")

q.corp.main<- corpus_subset(q.corpus, party %in% c("The Left", "FDP", "Greens", "CDU/CSU","SPD"))

texts <- tokens(q.corp.main,
                remove_numbers = T,
                remove_punct = T,
                remove_symbols = T,
                remove_separators = T,
                remove_hyphens = T,
                remove_url = T,
                verbose = T)

texts <- tokens_tolower(texts)
texts <- tokens_remove(texts, stopwords("german"))
texts <- tokens_remove(texts, c("freie", "demokraten","cdu","csu","linke","spd","fdp"))



tm <- dfm(texts)
tm <- dfm_wordstem(tm, language = "german")
tm <- dfm_trim(tm, min_termfreq = 3)
tm <- tm[, str_length(colnames(tm)) > 2]


rownames(tm) <- paste(tm$party,"-",substr(tm$date, 0, 4))




wordfish_first <- textmodel_wordfish(tm, dir = c(5, 6))



textplot_scale1d(wordfish_first, groups = tm$party, sort = FALSE) +
  labs(title = "Position on green policies", subtitle = "based on general election manifestos from 2002 to 2017")+
  ylab("Party Position ") +
  theme_ipsum_rc()

highlighted = c("kernenergi", "kohleausstieg","grad","atomaus", "klimaabkomm", "massentierhalt","marktwirtschaft","klimakris")

textplot_scale1d(wordfish_first, margin = "features", highlighted = highlighted) +
  labs(title = "Estimated word position", subtitle = "based on all manifestos")+
  theme_ipsum_rc()+
  xlab("Word weights")+
  ylab("Word fixed effects")



###############2017

corpus.2017 <- corpus_subset(q.corp.main, 
              year %in% c("2017")) 

texts.17 <- tokens(corpus.2017,
                remove_numbers = T,
                remove_punct = T,
                remove_symbols = T,
                remove_separators = T,
                remove_hyphens = T,
                remove_url = T,
                verbose = T)

texts.17 <- tokens_tolower(texts.17)
texts.17 <- tokens_remove(texts.17, stopwords("german"))
texts.17 <- tokens_remove(texts.17, c("freie", "demokraten","cdu","csu","linke","spd"))



tm.17 <- dfm(texts.17, groups = "party")
tm.17 <- dfm_trim(tm.17, min_termfreq = 4)
tm.17 <- tm.17[, str_length(colnames(tm.17)) > 3]  

textplot_wordcloud(tm.17, comparison = TRUE)




