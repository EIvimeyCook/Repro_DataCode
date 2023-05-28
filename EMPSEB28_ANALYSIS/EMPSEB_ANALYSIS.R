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
libray(hablar)

dat1 = read.csv("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/AllFiles/Conferences/EMPSEB28/EMPSEB28_ANALYSIS/EMPSEB_ANALYSIS_raw.csv")
dat1$T1 <- as.numeric(dat1$T1)
dat1$T2 <-   as.numeric(dat1$T2)
dat1$T3 <-   as.numeric(dat1$T3)
dat1$T4 <-  as.numeric(dat1$T4)
dat1$T5 <-   as.numeric(dat1$T5)
dat2<-pivot_longer(dat1, cols = 3:7, names_to = "b.type", values_to = "brood no") %>% filter(IND != 5)
full.dat<-dat2[complete.cases(dat2$`brood no`), ]
write.csv(full.dat, file ="~/Library/CloudStorage/OneDrive-UniversityofGlasgow/AllFiles/Conferences/EMPSEB28/EMPSEB28_ANALYSIS/EMPSEB_ANALYSIS_datafinal.csv", row.names = F)

full_dat = read.csv("~/Library/CloudStorage/OneDrive-UniversityofGlasgow/AllFiles/Conferences/EMPSEB28/EMPSEB28_ANALYSIS/EMPSEB_ANALYSIS_datafinal.csv")

summary(full_dat)
plot(as.factor(full_dat$HAB), full_dat$brood.no)

full_dat <- full_dat %>%
  convert(fct(IND,SEX.of.individual,b.type, HAB)) %>%
  filter(SEX.of.individual == "M" | SEX.of.individual == "F" & !is.na(HAB))


m1a<-lmer(brood.no~ 
            + HAB
          + SEX.of.individual
          + (1|IND),
          data=full_dat)

summary(m1a)

m1b<-lmer(brood.no~ 
            HAB*SEX.of.individual
          + (1|IND),
          data=full_dat)

summary(m1b)


m1c<-lm(brood.no~ 
            + HAB*SEX.of.individual,
          data=full_dat)

summary(m1c)


anova(m1a,m1b,m1c)


Na<-full_dat[complete.cases(full_dat$SEX.of.individual),]

ggplot(data=Na, aes(x=as.factor(SEX.of.individual), y=brood.no, colour = as.factor(HAB))) + 
  theme_bw() +
  geom_jitter(alpha = 0.3) +
  geom_smooth(linetype="dotted", method="lm", colour="black", fill="light grey") +
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", shape=0, width=0.8) +
  stat_summary(fun.data="mean_cl_boot", geom="point", shape=0, width=0.8) +
  labs(x = "Sex", y = "Brood Size", colour = "Habitat")

ggsave(filename = "final2.jpg", path = "~/Library/CloudStorage/OneDrive-UniversityofGlasgow/AllFiles/Conferences/EMPSEB28/EMPSEB28_ANALYSIS", dpi = 300, device = "jpg")


