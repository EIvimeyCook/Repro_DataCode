library(lme4)
library(lmerTest)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(rvest)
library(readr)
library(knitr)
library("data.table")
library(gplots)
library(ggridges)
library(shiny)
library(lme4)
library(rpart)
library(caret)
library(randomForest)
library(glmnet)
library("xgboost")
library("plotly")
library(tidyverse)

dat1 = read.csv("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/AllFiles/Conferences/EMPSEB28/EMPSEB28_ANALYSIS/EMPSEB_ANALYSIS_raw.csv")
dat1$T1 <- as.numeric(dat1$T1)
dat1$T2 <-   as.numeric(dat1$T2)
dat1$T3 <-   as.numeric(dat1$T3)
dat1$T4 <-  as.numeric(dat1$T4)
dat1$T5 <-   as.numeric(dat1$T5)
dat2<-pivot_longer(dat1, cols = 5:9, names_to = "b.type", values_to = "brood no") %>% filter(INd != 5)
full.dat<-dat2[complete.cases(dat2$`brood no`), ]
write.csv(full.dat, file ="~/Library/CloudStorage/OneDrive-UniversityofGlasgow/AllFiles/Conferences/EMPSEB28/EMPSEB28_ANALYSIS/EMPSEB_ANALYSIS_datafinal.csv")


#run a poisson model
m1a<-lmer(`brood no`~ 
           + hab
         + sex.of.indivdual
         + (1|INd),
         data=full.dat)

summary(m1a)

m1b<-lmer(`brood no`~ 
           + hab*sex.of.indivdual
         + (1|INd),
         data=full.dat)

summary(m1b)

m1c<-lm(`brood no`~ 
           + hab
         + sex.of.indivdual,
         data=full.dat)

summary(m1c)

anova(m1a,m1b,m1c)


