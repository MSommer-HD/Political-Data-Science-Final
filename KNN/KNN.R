##################
################## 


rm(list = ls())

library(scales)
library(hrbrthemes)
library(mlr)
library(tidyverse)
library(class)
library(readstata13)

rm(list = ls())
dat.alb <- read.dta13("ALLBUS18.dta")

keep <- c("pv01","pa01","pa11","sex","agec","iscd11","incc","gs01","pp22","pp40","ep01","ep03")

keep.18 <- subset(dat.alb, select = keep)


keep.18$gender <- as.numeric(recode(keep.18$sex,"MALE" = 0, "FEMALE" = 1))
keep.18$placement <- as.numeric(recode(keep.18$pa01,"F - LEFT" = 1, "A" = 2, "M" = 3, "O" = 4, "G" = 5,"Z" = 6, "E" = 7, "Y" = 8, "I" = 9, "P - RIGHT" = 10))
keep.18$env_prot <- recode(keep.18$pa11,"AGREE COMPLETELY" = 1, "TEND TO AGREE" = 2, "NEITHER NOR" = 3, "TEND TO DISAGREE" = 4, "DISAGREE COMPLETELY" = 5)
keep.18$age <- recode(keep.18$agec,"18-29 YEARS" = 1, "30-44 YEARS" = 2, "45-59 YEARS" = 3, "60-74 YEARS" = 4, "75-89 YEARS" = 5,"90 YEARS AND OLDER" = 6)
keep.18$edu <- recode(keep.18$iscd11,"PRIMARY EDUCATION" = 1, "LOWER SECONDARY" = 2, "UPPER SECONDARY" = 3, "POST SECONDARY" = 4, "SHORT-CYCLE TERTIARY" = 5, 
                         "BACHELOR LEVEL" = 6, "MASTER LEVEL" = 7, "DOCTORAL LEVEL" = 8)
keep.18$income <- recode(keep.18$incc, 
                         "NO OWN INCOME" = 0,
                         "UNDER 200 EURO" = 100,
                         "200-299 EURO" = 250,
                         "300-399 EURO" = 350,
                         "400-599 EURO" = 500,
                         "500-624 EURO" = 575,
                         "625-749 EURO" = 687,
                         "750-874 EURO" = 812,
                         "875-999 EURO" = 937,
                         "1000-1124 EURO" = 1075,
                         "1125-1249 EURO" = 1187,
                         "1250-1374 EURO" = 1312,
                         "1375-1499 EURO" = 1437,
                         "1500-1749 EURO" = 1625,
                         "1750-1999 EURO" = 1825,
                         "2000-2249 EURO" = 2125,
                         "2250-2499 EURO" = 2325,
                         "2500-2749 EURO" = 2625,
                         "2750-2999 EURO" = 2825,
                         "3000-3999 EURO" = 3500,
                         "4000-4999 EURO" = 4500,
                         "5000-7499 EURO" = 6125,
                         "7500 EURO AND MORE" = 8000)
 
keep.18$housing <- recode(keep.18$gs01,"BIG CITY" = 1, "SUBURB OF BIG CITY" = 2, "SMALL CITY, TOWN" = 3, "COUNTRY VILLAGE" = 4, "SOLITARY (FARM)HOUSE" = 5)

keep.18$vote_choice <- factor(recode(keep.18$pv01,"CDU-CSU" = 0,"THE GREENS" = 1))
                             #.default = NA_character_))

keep.18$cons_w <- recode(keep.18$pp22,"SELECTED" = 1, "NOT SELECTED" = 0)
keep.18$cons_do <- recode(keep.18$pp40,"SELECTED" = 1, "NOT SELECTED" = 0)


keep.18$germany <- recode(keep.18$ep01,"VERY GOOD" = 1, "GOOD" = 2, "PART GOOD, PART BAD" = 3, "BAD" = 4, "VERY BAD" = 5)
keep.18$personal <- recode(keep.18$ep03,"VERY GOOD" = 1, "GOOD" = 2, "PART GOOD, PART BAD" = 3, "BAD" = 4, "VERY BAD" = 5)


# select relevant data 

data_we_need_small = keep.18 %>%
  select(vote_choice, gender,age, placement, env_prot, housing)

data_we_need_medium = keep.18 %>%
  select(vote_choice, gender,age, placement, housing, income, edu, env_prot) 

data_we_need_big = keep.18 %>%
  select(vote_choice, gender,age, placement, housing, income, edu, env_prot, germany, personal, cons_w, cons_do)

#drop na base on relevant df


data_we_need = na.omit(data_we_need_small)                         
colnames(data_we_need) <- make.names(names(data_we_need))

######################################## TASK
voteTask <- makeClassifTask(data = data_we_need, target = "vote_choice")
voteTask

######################################## TUNING
knnParamSpace <- makeParamSet(makeDiscreteLearnerParam("k", values = 1:50))

gridSearch <- makeTuneControlGrid()

cvtuning <- makeResampleDesc(method = "RepCV", folds = 10, reps = 20,
                             stratify = TRUE)

tuneK <- tuneParams("classif.knn", task = voteTask,
                    resampling = cvtuning, measures = acc, par.set = knnParamSpace,
                    control = gridSearch)


################################################## ESTIMATION



knn_learner <- makeLearner("classif.knn", par.vals = list("k" = 25))

knnModel <- train(knn_learner, voteTask)




kfold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 50,
                          stratify = TRUE)
kfoldCV <- resample(learner = knn_learner, task = voteTask,
                    resampling = kfold, measures = list(mmce,acc))
kfoldCV$aggr


benchmark <- nrow(filter(data_we_need, vote_choice == 0))/nrow(data_we_need)
benchmark


######################################## PLOTTING


knnTuningData <- generateHyperParsEffectData(tuneK)

plotHyperParsEffect(knnTuningData, x = "k", y = "acc.test.mean", plot.type = "line")+
  theme_ipsum_rc() +
  labs(title = "Hyperparameter tuning (k)", subtitle = "based on 20 repeated 10 fold cross-validations")+
  ylab("Mean accuracy")+
  xlab("Number of neighbors")






