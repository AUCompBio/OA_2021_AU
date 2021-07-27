
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
  # write.csv(x=datum_filtered, file="data/OA_data_fiveormore_records.csv", row.names=FALSE, quote=FALSE)

datum <- read_csv("data/OA_data_fiveormore_records.csv", col_names = TRUE)

# check data
names(datum)
head(datum)
summary(datum)


