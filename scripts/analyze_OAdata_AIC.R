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
install.packages('tidyverse') # run once
library(tidyverse)
#install.packages('doBy')
library('doBy')
#install.packages('reshape2')
library('reshape2')
#install.packages('maptools')
library(maptools)
#install.packages('nlme')
library(nlme)
#install.packages('lme4')
library(lme4)

datum <- read_csv("data/OA_data_fin.csv", col_names = TRUE)

# check data
names(datum)
head(datum)
summary(datum)



# Summary Stats =================
# comment

# print number of records per journal
datum$journal = as.factor(datum$journal) # reset as factor
sink("outputs/stats/recordnum_journal.txt")
summary(datum$journal) # print number
sink()

#print range of records by journal
range(summary(datum$journal))

#number of journals
length(unique(sort(datum$journal)))

#number of field
length(unique(sort(datum$field)))

# summary stats: other gold 
OG = subset(datum,OAlab=='Other Gold') # subset data to other gold
OGss = length(OG$journal)
OGmean = mean(OG$clean_citations)
OGsd = sd(OG$clean_citations)
# summary stats: bronze
Bron = subset(datum,OAlab=='Bronze') # subset data to bronze
Bronss = length(Bron$journal)
Bronmean = mean(Bron$clean_citations)
Bronsd = sd(Bron$clean_citations)
# summary stats: green
Green = subset(datum,OAlab=='Green') # subset data to green
Greenss = length(Green$journal)
Greenmean = mean(Green$clean_citations)
Greensd = sd(Green$clean_citations)
# summary stats: non open access (non OA)
nonOA = subset(datum,OAlab=='Closed Access') # subset data to non OA
nonOAss = length(nonOA$journal)
nonOAmean = mean(nonOA$clean_citations)
nonOAsd = sd(nonOA$clean_citations)

# output summary stats
sink("outputs/stats/OAdes_summarystats.txt")
print(paste0("Mean(SD) Other Gold Citations = ",round(OGmean,2),"(",round(OGsd,2),")"))
print(paste("Sample Size =", OGss))

print(paste0("Mean(SD) non OA Citations = ",round(nonOAmean,2),"(",round(nonOAsd,2),")"))
print(paste("Sample Size =", nonOAss))

print(paste0("Mean(SD) Green Citations = ",round(Greenmean,2),"(",round(Greensd,2),")"))
print(paste("Sample Size =", Greenss))

print(paste0("Mean(SD) Bronze Citations = ",round(Bronmean,2),"(",round(Bronsd,2),")"))
print(paste("Sample Size =", Bronss))
sink()

# AIC & Multimodel Inference ================

# Import Library
library(MuMIn)
library(arm)

# drop col w/ NAs
select(datum, -clean_citations)

# drop one last annoying NA
datum = datum[-106010, ]

# Models w/ Every Variable Subset
model = lm(clean_citations~., data=datum, na.action=na.fail) # where nal.fail avoids default list-wise deletion for missing data
dd = dredge(model, extra=se.coef) # provides table w/ AIC results
write.table(dd, file="AICresults.txt")

# Models w/ Variable Subsets Limited by AIC
test = model.avg(get.models(dd,subset=TRUE))
summary(test)
importance(dd)
