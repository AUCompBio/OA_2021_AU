

# the following code performs correlation tests on all parameters used in the models
# of `analyze_OAdata.R` and `analyze_OAdata_filtered.R` to identify if any violate
# assumption of no multicollinearity

# clear workspace
rm(list=ls(all.names=TRUE))

datum <- read_csv("data/OA_data_fin.csv", col_names = TRUE)

# check data
names(datum)
head(datum)
summary(datum)


# isolate numerical variable columns
OAnum <- datum %>% select(APC, auth_count, AIS, year)

# generate correlation matrix
OAcor <- cor(OAnum)
round(OAcor, 2)
