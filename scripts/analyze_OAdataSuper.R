# lm WOS

# the following code takes web of science (WOS)
# raw data and runs a linear model (anova) to test
# whether open access (OA) status predicts
# citations for published manuscripts.

# WOS raw data should be in .xls format (ie use the
# 'Export to Excel' option when exporting records). 

# Setup ========================
# (data, directories, libraries)

# # clear workspace
# rm(list=ls(all.names=TRUE))
# 
# # install.packages('ggplot2') # run once
# library(ggplot2)
# # install.packages('tidyverse') # run once
 library(tidyverse)
# # install.packages('broom') # run once
# library(broom)
# #install.packages('knitr')
# library(knitr)
# #install.packages('doBy')
# library(doBy)
# #install.packages('reshape2')
# library(reshape2)
# #install.packages('maptools')
# library(maptools)
# #install.packages('nlme')
# library(nlme)
# #install.packages('lme4')
 library(lme4)
#library(MASS)
#library(DHARMa)
#install.packages("DHARMa")
# #install.packages('lmerTest')
# library(lmerTest)
# #install.packages('car')
# library(car)
# #install.packages("numDeriv")
# library(numDeriv)
# #install.packages("performance")
# library(performance)
# #install.packages("stargazer")
# library(stargazer) # stargazer does not like tibbles
# library(MASS)
# library(emmeans)

#May need to set working directory first
datum <- read.csv("data/OA_data_fin.csv")
#'jour' is journal
#'citations' is number of citations (y-variable)
#'OAdes' - 
#'OAlab' - type of access (closed, green, bronze, other gold)
#'year' is year of publication
#'Volume' is volume of journal publication is in
#'issue' is issue of journal publication is in
#'apc_cat' is Article processing charge (noAPC, lowAPC, highAPC)
#'AIS' is Article Influence Score - journal level metric
#'JCR_quart' is - from journal citation reports
#'field' is Field of biology which article is about
#'auth_count_scaled' is scaled number of authors


# check data
# names(datum)
# head(datum)
# summary(datum)


# Summary Stats =================
# comment

# code for generating tables and output in Latex format
# stardat <- as.data.frame(datum)
# stargazer(stardat[c("citations", "year", "auth_count",
#                     "norm_cit", "Volume", "Issue", "JCR_quart",
#                     "AIS")], 
#           type = "text",
#           title = "Summary Statistics for Full Open Access Dataset",
#           covariate.labels = c("Raw Citation Count", "Publication Year",
#                                "Author Count", "Normalized Citation Count",
#                                "Volume", "Issue", "Journal Citation Reports Quartiles",
#                                "Article Influence Score"),
#           out = "outputs/stats/OA_datum_sumstats.tex")
# 

# print number of records per journal
datum$jour = as.factor(datum$jour) # reset as factor
# sink("outputs/stats/recordnum_journal.txt")
# summary(datum$jour) # print number
# sink()

#print range of records by journal
# # range(summary(datum$jour))
# # #number of journals
# # length(unique(sort(datum$jour)))
# # #number of field
# # length(unique(sort(datum$field)))
# # 
# 
# # summary stats by OA label ---more detail than the mean calculations above, but should agree
# sum.stat <- datum %>% group_by(OAlab) %>% 
#   summarise(mean_cite = round(mean(norm_cit),2),
#             sd_cite = round(sd(norm_cit),2),
#             num_cite = length(norm_cit))
# 
# yr.stat <- datum %>% group_by(year) %>% 
#   summarise(mean_cite = round(mean(norm_cit),2),
#             sd_cite = round(sd(norm_cit),2),
#             num_cite = length(norm_cit))
# 
# jcr.stat <- datum %>% group_by(JCR_quart) %>% 
#   summarise(mean_cite = round(mean(norm_cit),2),
#             sd_cite = round(sd(norm_cit),2),
#             num_cite = length(norm_cit))
# 
# gni.stat <- datum %>% group_by(gni_class) %>% 
#   summarise(mean_cite = round(mean(norm_cit),2),
#             sd_cite = round(sd(norm_cit),2),
#             num_cite = length(norm_cit))
# 
# output summary stats
# sink("outputs/stats/OAdes_summarystats")
# kable(sum.stat)
# sink()
# 
# # Data Exploration  ================
# ls.str(datum)
# 
# Plot histogram; estimate mean & variance for response variables
# hist(datum$citations)#hist(datum$clean_citations)
# mean(datum$citations) #mean(datum$clean_citations)
# var(datum$citations) #var(datum$clean_citations)

#Normalized citation column no longer exists
#hist(datum$norm_cit) # for alternative normalization metric
# clean citations is not normally distributed; likely should not use 
#   linear model to fit these data. should use generalized linear model (Poisson).
#mean(datum$norm_cit)
#var(datum$norm_cit) # for alternative normalization metric
# the variance is higher than the mean, indicating an expectation of 
#   over-dispersion in the model.

# looking at AIS and author count distributions
# hist(datum$AIS)
# hist(datum$auth_count, breaks = 100)


# Statistical Tests ================

#need to id variables we want as a factor
datum$jour <- as.factor(datum$jour)
datum$field <- as.factor(datum$field)
#datum$jour_loc <- as.factor(datum$jour_loc)
datum$JCR_quart <- as.factor(datum$JCR_quart)
#datum$pub <- as.factor(datum$pub)
datum$year <- as.factor(datum$year)
#datum$gni_class <- as.factor(datum$gni_class)

# Rescaled numerical fields, into new columns
datum <- datum %>% 
  mutate(auth_count_scaled = scale(auth_count,center = TRUE, scale = TRUE),
         AIS_scaled = scale(AIS,center = TRUE, scale = TRUE))
#datum$auth_count_scaled <- scale(datum$auth_count)
datum$AIS_scaled <- scale(datum$AIS)
# this makes a matrix inside of our dataframe
datum$ln_AIS=log(datum$AIS)
datum$ln_auth_count=log(datum$auth_count)

datum$OAlab[datum$OAlab=="Closed Access"]="Access Closed"
datum$OAlab <- as.factor(datum$OAlab)

# set.seed(123)
# randomVector=seq(1:length(datum$OAlab))
# x=sample(randomVector,20000)
# datumSub=datum[x,]
# mod2.2 <- glmer(citations~OAlab+auth_count_scaled+JCR_quart+AIS_scaled+year+(1|field/jour), 
#                 data = datum, family = poisson(link = "log"))



# mod2.2.1 <- glmer.nb(citations~OAlab+auth_count_scaled+JCR_quart+AIS_scaled+year+(1|field/jour), 
#                 data = datum)
#Uses raw Citations; takes out GNI_class. 

#Compare both models
#anova(mod2.2.1,mod2.2)
#Significant, so negative binomial is better; poisson doesn't adequately fit the data

# ### Remove random effects to evaluate warnings
# mod2.2a <- glm(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count_scaled+JCR_quart+AIS_scaled+year+gni_class, 
#                 data = datum, family = poisson(link = "log"))
# ### Compare to negative binomial model
# mod2.2c <- glm.nb(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count_scaled+JCR_quart+AIS_scaled+year+gni_class, 
#                data = datum)
# ### Changed y variable to explore outliers
# mod2.2b <- glm(citations~relevel(OAlab, ref = "Closed Access")+auth_count_scaled+JCR_quart+AIS_scaled+year+gni_class, 
#                       data = datum, family = poisson(link = "log"))
# ### Compare to negative binomial model
#mod2.2d <- glm.nb(citations~relevel(OAlab, ref = "Closed Access")+auth_count_scaled+JCR_quart+AIS_scaled+year, 
#                   data = datum)
# ### Evaluations indicate that warnings can be ignored. 

### Test of interactions

# mod2.2.1.int <- glmer.nb(citations~OAlab*auth_count_scaled+
#                        JCR_quart+JCR_quart:OAlab+
#                        AIS_scaled+AIS_scaled:OAlab+
#                        year+year:OAlab+
#                        (1|field/jour), 
#                      data = datum)

mod2.2.2.int <- glmer.nb(citations~OAlab*auth_count_scaled*
                         JCR_quart*AIS_scaled*year+
                         (1|field/jour),
                         data = datum)



save(list=mod2.2.2.int,file="model.RData",envir=.GlobalEnv)


