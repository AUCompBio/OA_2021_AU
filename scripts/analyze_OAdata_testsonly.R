# lm WOS

# the following code takes web of science (WOS)
# raw data and runs a linear model (anova) to test
# whether open access (OA) status predicts
# citations for published manuscripts.

# WOS raw data should be in .xls format (ie use the
# 'Export to Excel' option when exporting records). 

# Setup ========================
# (data, directories, libraries)

setwd("/home/tcm0036/OA_2021_AU/")

# clear workspace
rm(list=ls(all.names=TRUE))

# install.packages('ggplot2') # run once
library(ggplot2)
# install.packages('tidyverse') # run once
library(tidyverse)
#install.packages('knitr')
library(knitr)
#install.packages('doBy')
library(doBy)
#install.packages('reshape2')
library(reshape2)
#install.packages('maptools')
library(maptools)
#install.packages('nlme')
library(nlme)
#install.packages('lme4')
library(lme4)
#install.packages('lmerTest')
library(lmerTest)
#install.packages('car')
library(car)

# Load dataset
datum <- read_csv("data/OA_data_fin.csv", col_names = TRUE)

# check data
names(datum)
head(datum)
summary(datum)


# Data Exploration  ================
ls.str(datum)
#need to id variables I want as a factor
datum$jour <- as.factor(datum$jour)
datum$OAlab <- as.factor(datum$OAlab)
datum$field <- as.factor(datum$field)
datum$jour_loc <- as.factor(datum$jour_loc)
datum$JCR_quart <- as.factor(datum$JCR_quart)
datum$pub <- as.factor(datum$pub)
datum$year <- as.factor(datum$year)

# Log-transform norm_cit for use in linear models. Add 1 first to avoid infinite values
datum$norm_cit_log <- log(datum$norm_cit + 1)

# Statistical Tests ================

# Recode categorical fields (deviation- compares level to grand mean)
contrasts(datum$field) = contr.sum(12)
contrasts(datum$year) = contr.sum(6)

# general linear model using norm_cite as the response
mod1 <- lmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count+field+JCR_quart+AIS+APC+year+(1|field:jour), 
             data = datum)
summary(mod1)
anova(mod1)

# general linear model using ln-transformed norm_cite as the response
mod1.log <- lmer(norm_cit_log~relevel(OAlab, ref = "Closed Access")+auth_count+field+JCR_quart+AIS+APC+year+(1|field:jour), 
             data = datum)
summary(mod1.log)
anova(mod1.log)

# general linear model using norm_cite as the response. Including interactions for access by field and author count by APC
mod1.2 <- lmer(norm_cit~relevel(OAlab, ref = "Closed Access")*field+auth_count*scale(APC)+JCR_quart+AIS+year+(1|field:jour), 
               data = datum)
summary(mod1.2)
Anova(mod1.2)

# Poisson regression using norm_cit as response
  # basic model of all factors, with a random effect of journal nested in field
mod2.1 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count+field+JCR_quart+AIS+APC+year+
                  APC+(1|field:jour), 
              data = datum, family = poisson(link = "log"))
summary(mod2.1)

  # basic model of all factors (numerical factors scaled), with a random effect of journal nested in field
mod2.2 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+field+scale(auth_count)+JCR_quart+scale(AIS)+scale(APC)+year+(1|field:jour), 
                data = datum, family = poisson(link = "log"))
summary(mod2.2)
  
  # basic model of all factors (numerical factors scaled) plus interaction of auth_count and APC, with a random effect of journal nested in field
mod2.3 <-  glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count*scale(APC)+field+JCR_quart+AIS+year+(1|field:jour), 
                 data = datum, family = poisson(link = "log"))
summary(mod2.3)

  # basic model of most factors (numerical factors scaled) plus interaction of auth_count and APC,with random effects of field and journal nested in field 
mod2.4 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count*scale(APC)+JCR_quart+AIS+year+(1|field/jour), 
                data = datum, family = poisson(link = "log"))
summary(mod2.4)
  # basic model of most factors (numerical factors scaled) plus interaction of auth_count and APC,with random effects of field and journal nested in field 
    # increased the number of iterations using maxfun = 200,000 (started with 10, 50, 80,000; 100, 150,000)
mod2.5 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+scale(auth_count)*scale(APC)+JCR_quart+scale(AIS)+(1|field/jour), 
                data = datum, family = poisson(link = "log"), control = glmerControl( optCtrl = list(maxfun = 200000)))
summary(mod2.5)
  # model with only terms that we are interested in interactions between, with random effect of journal nested in field
mod2.6 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")*field+scale(auth_count)*scale(APC)+(1|field:jour), 
                data = datum, family = poisson(link = "log"))
summary(mod2.6)
