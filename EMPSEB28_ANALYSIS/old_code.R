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
libray(tidyverse)

Exp.Weight = read.csv("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/AllFiles/Conferences/EMPSEB28/EMPSEB28_ANALYSIS/EMPSEB_ANALYSIS_raw.csv")
Exp.Weight$T1 <- as.numeric(Exp.Weight$T1)
Exp.Weight$T2 <- as.numeric(Exp.Weight$T2)
Exp.Weight$T3 <- as.numeric(Exp.Weight$T3)
Exp.Weight$T4 <- as.numeric(Exp.Weight$T4)
Exp.Weight$T5 <- as.numeric(Exp.Weight$T5)
dat<-pivot_longer(Exp.Weight, cols = 5:9, names_to = "trt", values_to = "trait no") %>% filter(INd != 5)
full.weight<-dat[complete.cases(dat$`trait no`), ]
write.csv(full.weight, file ="~/Library/CloudStorage/OneDrive-UniversityofGlasgow/AllFiles/Conferences/EMPSEB28/EMPSEB28_ANALYSIS/EMPSEB_ANALYSIS_datafinal.csv")

m1a<-lmer(`trait no`~ 
           + trt
         + sex.of.indivdual
         + (1|INd),
         data=full.weight)

summary(m1a)

m1b<-lmer(`trait no`~ 
           + trt*sex.of.indivdual
         + (1|INd),
         data=full.weight)

summary(m1b)

m1c<-lm(`trait no`~ 
           + trt
         + sex.of.indivdual,
         data=full.weight)

summary(m1c)

anova(m1a,m1b,m1c)


