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
# install.packages('broom') # run once
library(broom)
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
#install.packages("numDeriv")
library(numDeriv)
#install.packages("performance")
library(performance)
#install.packages("stargazer")
library(stargazer) # stargazer does not like tibbles
library(MASS)

datum <- read_csv("data/OA_data_fin.csv", col_names = TRUE)


# check data
names(datum)
head(datum)
summary(datum)


# Summary Stats =================
# comment

# code for generating tables and output in Latex format
stardat <- as.data.frame(datum)
stargazer(stardat[c("citations", "year", "auth_count",
                    "norm_cit", "Volume", "Issue", "JCR_quart",
                    "AIS")], 
          type = "text",
          title = "Summary Statistics for Full Open Access Dataset",
          covariate.labels = c("Raw Citation Count", "Publication Year",
                               "Author Count", "Normalized Citation Count",
                               "Volume", "Issue", "Journal Citation Reports Quartiles",
                               "Article Influence Score"),
          out = "outputs/stats/OA_datum_sumstats.tex")


# print number of records per journal
datum$jour = as.factor(datum$jour) # reset as factor
sink("outputs/stats/recordnum_journal.txt")
summary(datum$jour) # print number
sink()

#print range of records by journal
range(summary(datum$jour))
#number of journals
length(unique(sort(datum$jour)))
#number of field
length(unique(sort(datum$field)))


# summary stats by OA label ---more detail than the mean calculations above, but should agree
sum.stat <- datum %>% group_by(OAlab) %>% 
  summarise(mean_cite = round(mean(norm_cit),2),
            sd_cite = round(sd(norm_cit),2),
            num_cite = length(norm_cit))

yr.stat <- datum %>% group_by(year) %>% 
  summarise(mean_cite = round(mean(norm_cit),2),
            sd_cite = round(sd(norm_cit),2),
            num_cite = length(norm_cit))

jcr.stat <- datum %>% group_by(JCR_quart) %>% 
  summarise(mean_cite = round(mean(norm_cit),2),
            sd_cite = round(sd(norm_cit),2),
            num_cite = length(norm_cit))

gni.stat <- datum %>% group_by(gni_class) %>% 
  summarise(mean_cite = round(mean(norm_cit),2),
            sd_cite = round(sd(norm_cit),2),
            num_cite = length(norm_cit))

# output summary stats
sink("outputs/stats/OAdes_summarystats")
kable(sum.stat)
sink()

# Data Exploration  ================
ls.str(datum)

# Plot histogram; estimate mean & variance for response variables
hist(datum$citations)#hist(datum$clean_citations)
mean(datum$citations) #mean(datum$clean_citations)
var(datum$citations) #var(datum$clean_citations)

hist(datum$norm_cit) # for alternative normalization metric
# clean citations is not normally distributed; likely should not use 
#   linear model to fit these data. should use generalized linear model (Poisson).
mean(datum$norm_cit)
var(datum$norm_cit) # for alternative normalization metric
# the variance is higher than the mean, indicating an expectation of 
#   over-dispersion in the model.

# looking at AIS and author count distributions
hist(datum$AIS)
hist(datum$auth_count, breaks = 100)


# Statistical Tests ================

#need to id variables we want as a factor
datum$jour <- as.factor(datum$jour)
datum$OAlab <- as.factor(datum$OAlab)
datum$field <- as.factor(datum$field)
datum$jour_loc <- as.factor(datum$jour_loc)
datum$JCR_quart <- as.factor(datum$JCR_quart)
datum$pub <- as.factor(datum$pub)
datum$year <- as.factor(datum$year)
datum$gni_class <- as.factor(datum$gni_class)

# Rescaled numerical fields, into new columns
datum <- datum %>% 
  mutate(auth_count_scaled = scale(auth_count,center = TRUE, scale = TRUE),
         AIS_scaled = scale(AIS,center = TRUE, scale = TRUE))
#datum$auth_count_scaled <- scale(datum$auth_count)
#datum$AIS_scaled <- scale(datum$AIS)
# this makes a matrix inside of our dataframe

##### Models that didn't converge :( #####
# Poisson regression using norm_cit as response
  # basic model of all factors, with a random effect of journal nested in field
#mod2.1 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count+JCR_quart+AIS+year+gni_class+
#                  (1|field/jour), 
#              data = datum, family = poisson(link = "log"))
#summary(mod2.1)
#Anova(mod2.1)
# Previous Warning (before 8/30/21)
#Warning messages:
  #  1: Some predictor variables are on very different scales: consider rescaling 
  #  2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  #                  Model failed to converge with max|grad| = 0.0449433 (tol = 0.002, component 1)
  #                3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  #                                  Model is nearly unidentifiable: very large eigenvalue
  #                                - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
  #                                - Rescale variables?
# Warning message as of 8/30/21
#Warning messages:
  #  1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  #                    Model failed to converge with max|grad| = 0.0166963 (tol = 0.002, component 1)
  #                  2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  #                                    Model is nearly unidentifiable: very large eigenvalue
  #                                  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
  #                                  - Rescale variables?
  # basic model of all factors (numerical factors scaled), with a random effect of journal nested in field

mod2.2 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count_scaled+JCR_quart+AIS_scaled+gni_class+(1|field/jour)+(1|year), 
                data = datum, family = poisson(link = "log"))

### Remove random effects to evaluate warnings
mod2.2a <- glm(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count_scaled+JCR_quart+AIS_scaled+year+gni_class, 
                data = datum, family = poisson(link = "log"))
### Compare to negative binomial model
mod2.2c <- glm.nb(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count_scaled+JCR_quart+AIS_scaled+year+gni_class, 
               data = datum)
### Changed y variable to explore outliers
mod2.2b <- glm(citations~relevel(OAlab, ref = "Closed Access")+auth_count_scaled+JCR_quart+AIS_scaled+year+gni_class, 
                      data = datum, family = poisson(link = "log"))
### Compare to negative binomial model
mod2.2d <- glm.nb(citations~relevel(OAlab, ref = "Closed Access")+auth_count_scaled+JCR_quart+AIS_scaled+year+gni_class, 
                  data = datum)

# Putting model coefficients into a dataframe
Mod2.2 <- as.data.frame(coef(summary(mod2.2)))
# Adding column with interpretable betas
Mod2.2 <- Mod2.2 %>% mutate(Exp_Estim =exp(Estimate))

## interaction with gni_class & access level
mod2.3 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")*gni_class+auth_count_scaled+JCR_quart+AIS_scaled+year+(1|field/jour), 
                data = datum, family = poisson(link = "log"))

Mod2.3 <- as.data.frame(coef(summary(mod2.3)))
Mod2.3 <- Mod2.3 %>% mutate(Exp_Estim =exp(Estimate))

# Previous message (before 8/30/21)
#Warning messages:
  #1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  #                    Model failed to converge with max|grad| = 0.00371365 (tol = 0.002, component 1)
  #2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  #                                    Model is nearly unidentifiable: very large eigenvalue
  #- Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
  #- Rescale variables?
# Warning message as of 8/30/21
#Warning message:
 #  In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
 #                 Model is nearly unidentifiable: very large eigenvalue
 #               - Rescale variables?
# export results from anova to .txt file

sink("clean_results_anova_WOS2.txt")
print(summary(model))
print(confint(model))
sink()  # returns output to the console


#### ARCHIVED MODEL BUILDING ####
#### general linear model using norm_cite as the response
##mod1 <- lmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count+field+JCR_quart+AIS+APC+year+(1|field:jour), 
##             data = datum)
##summary(mod1)
#anova(mod1)
#
## general linear model using ln-transformed norm_cite as the response
#mod1.log <- lmer(norm_cit_log~relevel(OAlab, ref = "Closed Access")+auth_count+field+JCR_quart+AIS+scale(APC)+year+pub+(1|field:jour), 
#                 data = datum)
#summary(mod1.log)
#anova(mod1.log)
##Anova(mod1.log)
### Note: mod1 and mod1 with the log-transformed norm_cit response both recover the fit warning:
### some predictor variables are on very different scales
#
#
## general linear model using norm_cite as the response. Including interactions for access by field and author count by APC
#mod1.2 <- lmer(norm_cit~relevel(OAlab, ref = "Closed Access")*field+auth_count*scale(APC)+JCR_quart+AIS+year+(1|field:jour), 
#               data = datum)
#summary(mod1.2)
#Anova(mod1.2, type = 3)
#
## general linear model using norm_cit_log as the response. Including interactions for access by field and author count by APC
#mod1.2.log <- lmer(norm_cit_log~relevel(OAlab, ref = "Closed Access")*field+auth_count*scale(APC)+JCR_quart+AIS+year+(1|field:jour), 
#                   data = datum)
#summary(mod1.2.log)
#Anova(mod1.2.log, type = 3)
## Pub is not significant-- removed from model.
#
## Comparing norm_cit_log models with and without interactions
#anova(mod1.log,mod1.2.log)
#
#mod2.3 <-  glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count*scale(APC)+field+JCR_quart+AIS+year+(1|field:jour), 
#                 data = datum, family = poisson(link = "log"))
#summary(mod2.3)
##Warning messages:
##  1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
##                    Model failed to converge with max|grad| = 1.75564 (tol = 0.002, component 1)
##  2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
##                   Model is nearly unidentifiable: very large eigenvalue
##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
##  - Rescale variables?
#
## basic model of most factors (numerical factors scaled) plus interaction of auth_count and APC,with random effects of field and journal nested in field 
#mod2.4 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count*scale(APC)+JCR_quart+AIS+year+(1|field/jour), 
#                data = datum, family = poisson(link = "log"))
#summary(mod2.4)
## basic model of most factors (numerical factors scaled) plus interaction of auth_count and APC,with random effects of field and journal nested in field 
## increased the number of iterations using maxfun = 200,000 (started with 10, 50, 80,000; 100, 150,000)
#mod2.5 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+scale(auth_count)*scale(APC)+JCR_quart+scale(AIS)+(1|field/jour), 
#                data = datum, family = poisson(link = "log"), control = glmerControl( optCtrl = list(maxfun = 200000)))
#summary(mod2.5)
## model with only terms that we are interested in interactions between, with random effect of journal nested in field
#mod2.6 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")*field+scale(auth_count)*scale(APC)+(1|field:jour), 
#                data = datum, family = poisson(link = "log"))
#summary(mod2.6)
#  # basic model of all factors (numerical factors scaled) plus interaction of auth_count and APC, with a random effect of journal nested in field
#
## Tried starting the model from where it failed to converge
#ss <- getME(mod2.5,c("theta","fixef"))
#model_2 <- update(mod2.5,start=ss)
#
##Checking for singularity (want this value to not be close to zero)
## https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
#tt <- getME(mod2.5,"theta")
#ll <- getME(mod2.5,"lower")
#min(tt[ll==0])
#
##
#ranef(mod2.5)
#fixef(mod2.5)
#
## Plotting estimates
#theme_set(theme_sjplot())
#plot_model(mod2.5,sort.est = TRUE,show.values = TRUE,
#           value.offset = .4,vline.color = "green")
#plot_model(mod2.5, type = 're')
#
#range(datum$AIS)

#datum$norm_cit_log <- log(datum$norm_cit + 1) 

#hist(datum$norm_cit_log)
#mean(datum$norm_cit_log)
#var(datum$norm_cit_log)

### mean calculations
# grand mean of norm_cit
#mean(datum$norm_cit)
# means for norm_cit across levels of categorical variables
#myr <- with(datum, tapply(norm_cit, year, mean))
#moa <- with(datum, tapply(norm_cit, OAlab, mean))
#mjcr <- with(datum, tapply(norm_cit, JCR_quart, mean))
#mgni <- with(datum, tapply(norm_cit, gni_class, mean))
# differences between levels for each categ. var.
#successive when level > 2
#dyr <- diff(myr)
#doa <- diff(moa)
#djcr <- diff(mjcr)
#dgni <- diff(mgni)
#means of each combination of all categorical variables
#m2var <- with(datum,tapply(norm_cit,list(year,gni_class,OAlab,JCR_quart),mean))
#mean of year when all other categories are reference values
#mean(m2var[,"High", "Closed Access", 1])

#####
# Recode year field (deviation- compares level to grand mean)
#contrasts(datum$year) = contr.sum(6)
# contrasts(datum$field) = contr.sum(12) no longer relevant with field as random effect
