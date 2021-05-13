# Header and Comments ==================

# clear workspace
rm(list=ls(all.names=TRUE))

# install.packages('readxl') # run once
library(readxl)
# install.packages('tidyverse') # run once
library(tidyverse)
# install.packages('stringr')
library(stringr)

# list/save dir names in curr dir
dirnames = paste0("data/raw/", list.files(path = "data/raw/"))

# create empty dataframe
datum = data.frame()

# loop thru 'dirnames', import files, append to 'datum'
ptm <- proc.time() #Start time
for (dir in dirnames) {
  templist = list.files(path=dir, pattern='/*.xls')
  tempdatum = ldply(paste(dir,templist,sep='/'), read_excel)
  datum = rbind(datum, tempdatum)
}

proc.time() - ptm #Time elapsed

# Exporting journal names to a csv for validation and matching
journals <- data.frame(unique(datum$`Source Title`))
write_csv(journals, file = "output/tables/jourlist.csv")

# Clean up =====================
# 1. rename cols
# 2. clean names
# 3. drop unneeded cols
# 4. create new cols
# 5. merge w/ metadata

# 1. rename cols
datum <- datum %>% rename(journal = 'Source Title',
         citations = 'Times Cited, All Databases', OAdes = 'Open Access Designations',
         year = 'Publication Year', corrAuth_loc = 'Reprint Addresses')

#names(datum)[names(datum)=='Source Title'] = 'journal'
#names(datum)[names(datum)=='Times Cited, All Databases'] = 'citations'
#names(datum)[names(datum)=='Open Access Designations'] = 'OAdes'
#names(datum)[names(datum)=='Publication Year'] = 'year'
#names(datum)[names(datum)=='Reprint Addresses'] = 'corrAuth_loc'

# 2. clean up journal names
# Removing excluded journals and special issues; making all journal titles uppercase
datum <- datum %>%  
  mutate(journal = toupper(journal)) %>% 
  filter(is.na(`Special Issue`)) %>% 
  filter(!journal %in% c('ONCOTARGET', 'FUNGAL BIOLOGY REVIEWS', 
                         'PERSOONIA', 'PLANT BIOTECHNOLOGY JOURNAL'))
# Correcting specific journal name
datum <- datum %>% 
  mutate(across(journal,str_replace_all, 
                pattern = "JOURNAL OF COMPARATIVE PHYSIOLOGY B-BIOCHEMICAL SYSTEMIC AND ENVIRONMENTAL PHYSIOLOGY",
                replacement = "JOURNAL OF COMPARATIVE PHYSIOLOGY B-BIOCHEMICAL SYSTEMS AND ENVIRONMENTAL PHYSIOLOGY"))


# 3a. create new col(s) with univariate outliers corrected
clean_cols = c('citations') # select cols to correct
for (col in clean_cols) {
  var = datum[[col]] # select col
  quarters = quantile(var,na.rm=TRUE) # determine quartiles
  IQR = quarters[3]-quarters[2] # .75 quartile - .25 quartile (IQR)
  var[var>median(var)+(2*IQR)] = median(var)+(2*IQR) # replace values > upp limit
  var[var<median(var)-(2*IQR)] = median(var)-(2*IQR) # replace values < low limit
  datum[,paste0("clean_",col)] = var # append col
  rm(col,IQR,quarters,var)
}
# 3b. create new col from OA designations (ie OA, Closed, Other)
datum$OAlab = ifelse(grepl('Gold', datum$OAdes), 
                     'Other Gold', ifelse(grepl('Bronze', datum$OAdes), 'Bronze',
                                          ifelse(grepl('Green', datum$OAdes), 'Green',
                                                 ifelse(is.na(datum$OAdes), 'Closed Access', 'Error'))))
# 3c. create new col from authors with count
datum <- datum %>% add_column(auth_count = str_count(datum$Authors, ";") + 1)
# 3d. create new col from reprint addresses with author country
datum <- datum %>% add_column(auth_loc = str_remove(word(datum$`Reprint Addresses`, -1),"[.]"))

# 4. remove unneeded cols
datum <- datum %>% select(journal, citations, OAdes,  year, auth_loc, Publisher, auth_count)
#keep_cols = c('journal','OAdes','citations', 'year', 'Authors', 'corrAuth_loc', 'Publisher')
#datum = datum[keep_cols]

# 5a. adding metadata
md <- read_csv("data/Clean_JournalImpact.csv", col_names = TRUE)
# 5b. adding IF quantiles to metadata and merge to datum data.frame
IFquant <- quantile(md$IF_5Y_2019)
IFquant
md <- md %>% add_column(JIFquant = cut(md$IF_5Y_2019, 
                            breaks = c(0, IFquant[[2]], IFquant[[3]], IFquant[[4]], 40),
                            labels = c("Q1","Q2","Q3","Q4")))
# 5c. joining metadata with clean data and writing to output file
datum <- left_join(datum, md, by = "journal")
write_csv(datum, file = "data/OA_data_fin.csv")

