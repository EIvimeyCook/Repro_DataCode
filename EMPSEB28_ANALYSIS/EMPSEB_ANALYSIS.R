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
write.csv(full.wieght. file ="~/Library/CloudStorage/OneDrive-UniversityofGlasgow/AllFiles/Conferences/EMPSEB28/EMPSEB28_ANALYSIS/EMPSEB_ANALYSIS_datafinal.csv")

#run a poisson model
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


Na<-full.weight[complete.cases(full.weight$sex.of.indivdual),]

gp <-ggplot(data=Na, aes(x=as.factor(sex.of.indivdual), y=`trait no`, colour = as.factor(trt))) + 
  theme_bw() +
  geom_point()

ggp=gp +
  geom_smooth(linetype="dotted", method="lm", colour="black", fill="light grey") +
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", shape=0, width=0.8) +
  stat_summary(fun.data="mean_cl_boot", geom="point", shape=0, width=0.8)

final<-ggp +
labs(x = "Sex", y = "traitvalue") %>%
  labs(colour = "TraitNumber")


ggsave(filename = "final2.jpg", path = "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/AllFiles/Conferences/EMPSEB28/EMPSEB28_ANALYSIS", dpi = 300, device = "jpg")


