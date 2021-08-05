
# lm WOS

# the following code takes web of science (WOS)
# raw data and runs a linear model (anova) to test
# whether open access (OA) status predicts
# citations for published manuscripts.

# WOS raw data should be in .xls format (ie use the
# 'Export to Excel' option when exporting records). 

# Setup ========================
# (data, directories, libraries)

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

# Below code used to generate dataframe excluding all records with fewer than 5 citations
  # datum <- read_csv("data/OA_data_fin.csv", col_names = TRUE)
  # Remove records with fewer than 5 citations 
  # datum_filtered <- datum %>% filter(citations > 4)


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
#datum$year <- as.factor(datum$year)

# write.csv(x=datum_filtered, file="data/OA_data_fiveormore_records.csv", row.names=FALSE, quote=FALSE)
datum_filtered <- datum %>% filter(citations > 4) %>% filter(field != "CellBio") 
names(datum_filtered)
head(datum_filtered)
summary(datum_filtered)
ls.str(datum_filtered)

# Re-do steps to create norm_cit and norm_cit_log variables from `combine_OAdata.R`
# for filtered dataframe

#a slightly different approach to apply threshold to high citation values - citation count is correlated with year
mod=lm(datum_filtered$citations~datum_filtered$year)

#use model coefficient to make fitted column
datum_filtered$fitted <- mod$coefficients[2]*datum_filtered$year + mod$coefficients[1]

# calculate cooks d for all data
datum_filtered$cooksd <- cooks.distance(mod)

#get upper limit of citation count
upper_limit_citations=min(datum_filtered[(datum_filtered$cooksd >=3*mean(datum_filtered$cooksd, na.rm=T)) & # cooksD is high
                                  (datum_filtered$citations > datum_filtered$fitted),]$citations)

datum_filtered$norm_cit=ifelse(datum_filtered$citations<upper_limit_citations,
                               datum_filtered$citations,upper_limit_citations)

# Log-transform norm_cit for use in linear models. Add 1 first to avoid infinite values
datum_filtered$norm_cit_log <- log(datum_filtered$norm_cit + 1)


# Change year category to factor
datum$year <- as.factor(datum$year)
datum_filtered$year <- as.factor(datum_filtered$year)

# Data Exploration  ===============


# Plot histogram; estimate mean & variance for response variables
par(mfrow=c(1,2))
hist(datum_filtered$clean_citations)
hist(datum$clean_citations)
mean(datum_filtered$clean_citations)
mean(datum$clean_citations)
var(datum_filtered$clean_citations)
var(datum$clean_citations)


hist(datum_filtered$norm_cit) # for alternative normalization metric
# clean citations is not normally distributed; likely should not use 
#   linear model to fit these data. should use generalized linear model (Poisson).
hist(datum$norm_cit)
mean(datum_filtered$norm_cit)
var(datum_filtered$norm_cit) # for alternative normalization metric
# the variance is higher than the mean, indicating an expectation of 
#   over-dispersion in the model.

hist(datum_filtered$norm_cit_log)
hist(datum$norm_cit_log)
mean(datum_filtered$norm_cit_log)
var(datum_filtered$norm_cit_log)

hist(datum_filtered$AIS)
hist(datum$AIS)
hist(datum_filtered$auth_count, breaks = 100)
hist(datum$auth_count, breaks=100)


# Statistical Tests ================

# Recode categorical fields (deviation- compares level to grand mean)
contrasts(datum_filtered$field) = contr.sum(12)
contrasts(datum_filtered$year) = contr.sum(6)

# general linear model using norm_cite as the response
## As of 8/5/2021, both this script and `analyze_OAdata.R` excluded the variable "pub" from mod1. TM corrected that
mod1 <- lmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count+field+JCR_quart+AIS+year+pub+(1|field:jour), 
             data = datum_filtered)
summary(mod1)
anova(mod1)

# general linear model using ln-transformed norm_cite as the response
mod1.log <- lmer(norm_cit_log~relevel(OAlab, ref = "Closed Access")+auth_count+field+JCR_quart+AIS+year+pub+(1|field:jour), 
                 data = datum_filtered)
summary(mod1.log)
anova(mod1.log)


# general linear model using norm_cite as the response. Including interactions for access by field and author count by APC
mod1.2 <- lmer(norm_cit~relevel(OAlab, ref = "Closed Access")*field+auth_count+JCR_quart+AIS+year+(1|field:jour), 
               data = datum_filtered)
summary(mod1.2)
Anova(mod1.2, type = 3)

# general linear model using norm_cit_log as the response. Including interactions for access by field and author count by APC
mod1.2.log <- lmer(norm_cit_log~relevel(OAlab, ref = "Closed Access")*field+auth_count+JCR_quart+AIS+year+(1|field:jour), 
                   data = datum_filtered)
summary(mod1.2.log)
Anova(mod1.2.log, type = 3)


# Comparing norm_cit_log models with and without interactions
anova(mod1.log,mod1.2.log)


# Poisson regression using norm_cit as response
# basic model of all factors, with a random effect of journal nested in field
mod2.1 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count+field+JCR_quart+AIS+year+(1|field:jour), 
                data = datum_filtered, family = poisson(link = "log"))
#Warning messages:
#1: contrasts dropped from factor field due to missing levels 
#2: contrasts dropped from factor field due to missing levels 
#3: contrasts dropped from factor field due to missing levels 
#4: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#Model failed to converge with max|grad| = 0.0251909 (tol = 0.002, component 1)
#5: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#Model is nearly unidentifiable: very large eigenvalue
#- Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
#- Rescale variables?
summary(mod2.1)
Anova(mod2.1)

########## Failed to converge too
# basic model of all factors (numerical factors scaled), with a random effect of journal nested in field
mod2.2 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+field+scale(auth_count)+JCR_quart+scale(AIS)+year+(1|field:jour), 
                data = datum_filtered, family = poisson(link = "log"))
#Warning messages:
#1: contrasts dropped from factor field due to missing levels 
#2: contrasts dropped from factor field due to missing levels 
#3: contrasts dropped from factor field due to missing levels 
#4: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                  Model is nearly unidentifiable: very large eigenvalue
#                - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
#                - Rescale variables?
summary(mod2.2)
Anova(mod2.2)

# basic model of all factors (numerical factors scaled) plus interaction of auth_count and APC, with a random effect of journal nested in field
mod2.3 <-  glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count+field+JCR_quart+AIS+year+(1|field:jour), 
                 data = datum_filtered, family = poisson(link = "log"))
summary(mod2.3)
Anova(mod2.3)

# basic model of most factors (numerical factors scaled) plus interaction of auth_count and APC,with random effects of field and journal nested in field 
mod2.4 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count+JCR_quart+AIS+year+(1|field/jour), 
                data = datum_filtered, family = poisson(link = "log"))

summary(mod2.4)
Anova(mod2.4)
# basic model of most factors (numerical factors scaled) plus interaction of auth_count and APC,with random effects of field and journal nested in field 
# increased the number of iterations using maxfun = 200,000 (started with 10, 50, 80,000; 100, 150,000)
mod2.5 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+scale(auth_count)+JCR_quart+scale(AIS)+(1|field/jour), 
                data = datum_filtered, family = poisson(link = "log"), control = glmerControl( optCtrl = list(maxfun = 200000)))
summary(mod2.5)
Anova(mod2.5)

# model with only terms that we are interested in interactions between, with random effect of journal nested in field
mod2.6 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")*field+scale(auth_count)+(1|field:jour), 
                data = datum_filtered, family = poisson(link = "log"))
summary(mod2.6)
Anova(mod2.6)