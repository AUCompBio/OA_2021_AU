# the following code performs correlation tests on all parameters used in the models
# of `analyze_OAdata.R` and `analyze_OAdata_filtered.R` to identify if any violate
# assumption of no multicollinearity

library(tidyverse)

# clear workspace
rm(list=ls(all.names=TRUE))

datum <- read_csv("data/OA_data_fin.csv", col_names = TRUE)

# check data
names(datum)
head(datum)
summary(datum)


# isolate numerical variable columns
OAnum <- datum %>% select(auth_count, AIS, JCR_quart, year)

# generate correlation matrix
OAcor <- cor(OAnum)
round(OAcor, digits = 2)
#             auth_count AIS JCR_quart  year
#auth_count          1    NA        NA    NA
#AIS                NA  1.00     -0.34 -0.01
#JCR_quart          NA -0.34      1.00  0.01
#year               NA -0.01      0.01  1.00