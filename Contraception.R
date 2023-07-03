setwd('C:/Users/Admin/Documents/PROJECTS/CONTRACEPTION')

library(dplyr)
library(rio)
library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(gridExtra)

options(warn=-1)

cmc <- read.table('C:/Users/Admin/Documents/PROJECTS/CONTRACEPTION/cmc.data', sep = ',')

names(cmc) <- c("age","education","husband_education","no_children","religion",
               "working_status","husband_occupation","std_index","media_exposure",
               "contraceptive_method")


cmc$education = factor(cmc$education, levels = c(1,2,3,4), labels = c("low", "mid_low", "mid_high", "high"), ordered=TRUE)
cmc$husband_education = factor(cmc$husband_education, levels = c(1,2,3,4), labels = c("low", "mid_low", "mid_high", "high"), ordered=TRUE)
cmc$religion = factor(cmc$religion, levels = c(0,1), labels = c('non_islam','islam'))
cmc$working_status = factor(cmc$working_status, levels = c(0,1), labels = c('yes','no'))
cmc$husband_occupation = factor(cmc$husband_occupation, levels = c(1,2,3,4), ordered=TRUE)
cmc$std_index = factor(cmc$std_index, levels = c(1,2,3,4),labels = c("low", "mid_low", "mid_high", "high"), ordered=TRUE)
cmc$media_exposure = factor(cmc$media_exposure, levels = c(0,1), labels = c('good','not_good'))
cmc$contraceptive_method = factor(cmc$contraceptive_method, levels = c(1,3,2), labels = c('no_use','short_term','long_term'), ordered=TRUE)

str(cmc)

library(ggplot2)
p1 = ggplot(cmc, aes(x = contraceptive_method, y = education)) + 
  geom_count() + 
  scale_x_discrete(limits =  c('no_use','short_term','long_term'))
p2 = ggplot(cmc, aes(x = contraceptive_method, y = husband_education)) + 
  geom_count() + 
  scale_x_discrete(limits =  c('no_use','short_term','long_term'))
grid.arrange(p1, p2)


ggplot(cmc,aes(x=contraceptive_method))+
  geom_bar()

set.seed(1)
train.index <- sample(1:nrow(cmc), round(0.70*nrow(cmc),0))

cmc.train <- cmc[train.index, ]
cmc.test <- cmc[-train.index,  ]

cmc.train.fit <- rpart(contraceptive_method ~ . , data = cmc.train, method = "class", cp = 0.05)

plot(cmc.train.fit, palettes = c("Greens", "Reds"), sub = "")
text(cmc.train.fit)
