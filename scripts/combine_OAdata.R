# Header and Comments ==================

# clear workspace
rm(list=ls(all.names=TRUE))

# install.packages('plyr') # run once
library(plyr)
# install.packages('readxl') # run once
library(readxl)
# install.packages('tidyverse') # run once
library(tidyverse)
# install.packages('stringr')
library(stringr)
# install.packages('readr')
library(readr)
# install.packages('data.table')
library(data.table)
# install.packages('maptools')
library(maptools)

# list/save dir names in curr dir
dirnames = paste0("data/raw/", list.files(path = "data/raw/"))

# create empty list
datumlist = list()

# loop thru 'dirnames', import files, append to 'datum'
ptm <- proc.time() #Start time
for (dir in dirnames) {
  templist = list.files(path=dir, pattern='/*.xls')
  tempdatum = ldply(paste(dir,templist,sep='/'), read_excel)
  datumlist = append(datumlist, list(tempdatum))
}
datum <- as.data.frame(rbindlist(datumlist))
# Replacing NA with blanks for OA column (necessary for matching)
datum$`Open Access Designations`= replace_na(datum$`Open Access Designations`, "")
proc.time() - ptm #Time elapsed

# Exporting journal names to a csv for validation and matching
#journals <- data.frame(unique(datum$`Source Title`))
#write_csv(journals, file = "output/tables/jourlist.csv")

# OA/CA Matched Issues =========
# Functions and subsequent code to produce OA/CA matched issues dataframe

## Function that takes grouped data.frames and checks for the presence of both target OA designation patterns in each group
matchbytibble <- function(tibs) {
  # OA designation patterns needed to match; using grep so regex should work 
  patt <- c("^$", "Other Gold")
  # Check for Other Gold in the designation field
  ckgold <- any(grepl(patt[[2]],tibs$`Open Access Designations`))
  # Check for blanks in the designation field
  ckblank <- any(grepl(patt[[1]],tibs$`Open Access Designations`))
  
  # If ckgold & ckblank evaluate at TRUE...else...
  if (ckgold == TRUE && ckblank == TRUE) {
    # add tib in list form to matched list of data.tables
    matchlist <<- append(matchlist, list(tibs)) 
    print("Both subscription (blank) and Other Gold designations are found in this issue. Writing to matched dataframe...")
  } else {
    print("Subscription (blank) and/or Other Gold designations were not found in this issue. Omitting this issue from the matched dataframe.")
  }
}


## Function that isolates Volume/Issues that have target OA designations in them. Applies the "matchbytibble" function to write issues that have both target OA designations to a new csv.
splitbyjournal <- function(journal) {
  # OA designation patterns needed to match; using grep so regex should work 
  patt <- c("^$", "Other Gold")
  # subset single journal in dataset
  sjournal <- datum %>% filter(`Source Title` == journal)
  # Filtering for target OA designations in a single journal
  fsjournal <- sjournal %>% 
    filter(grepl(paste(patt, collapse = "|"), `Open Access Designations`))
  # Intersection of Volume-Issue combinations that had either target OA designation. 
  sectjournal <- sjournal %>% 
    semi_join(fsjournal, by =c("Volume", "Issue"))
  # Data is grouped by Volume & Issue; Next apply the matchbytibble function to get designation-matched issues
  sectjournal %>% 
    group_split(Volume,Issue) %>% 
    map(~ matchbytibble(.x))
}

# Initializing empty list
matchlist <- list()
# Getting Journal Names. Concatenation of the csv's converted from the download xls files leaves a part of the header as a field. This is why I am excluding the last line.
jnames <- unique(datum$`Source Title`)
# Apply splitbyjournal and subsequent matchbytibble functions
sapply(jnames, splitbyjournal)

# Convert list of lists to data.frame
matched <- as.data.frame(rbindlist(matchlist))

# Clean up =====================
# 1. rename cols
# 2. clean names
# 3. drop unneeded cols
# 4. create new cols
# 5. merge w/ metadata

# 1. rename cols
datum <- datum %>% dplyr::rename(journal = 'Source Title',
                          citations = 'Times Cited, All Databases', OAdes = 'Open Access Designations',
                          year = 'Publication Year', corrAuth_loc = 'Reprint Addresses')

matched <- matched %>% dplyr::rename(journal = 'Source Title',
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
  filter(year %in% c(2013,2014,2015,2016,2017,2018)) %>% 
  filter(!journal %in% c('ONCOTARGET', 'FUNGAL BIOLOGY REVIEWS', 
                         'PERSOONIA', 'PLANT BIOTECHNOLOGY JOURNAL',
                         'GENERAL AND COMPARATIVE ENDOCRINOLOGY',
                         'AVIAN BIOLOGY RESEARCH', 'DEVELOPMENTAL BIOLOGY'))
# Correcting specific journal name
datum <- datum %>% 
  mutate(across(journal,str_replace_all, 
                pattern = "JOURNAL OF COMPARATIVE PHYSIOLOGY B-BIOCHEMICAL SYSTEMIC AND ENVIRONMENTAL PHYSIOLOGY",
                replacement = "JOURNAL OF COMPARATIVE PHYSIOLOGY B-BIOCHEMICAL SYSTEMS AND ENVIRONMENTAL PHYSIOLOGY"))
#for matched: Removing excluded journals and special issues; making all journal titles uppercase 
matched <- matched %>%  
  mutate(journal = toupper(journal)) %>% 
  filter(is.na(`Special Issue`)) %>% 
  filter(year %in% c(2013,2014,2015,2016,2017,2018)) %>% 
  filter(!journal %in% c('ONCOTARGET', 'FUNGAL BIOLOGY REVIEWS', 
                         'PERSOONIA', 'PLANT BIOTECHNOLOGY JOURNAL'))
# Correcting specific journal name
matched <- matched %>% 
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

#a slightly different approach to apply threshold to high citation values - citation count is correlated with year
mod=lm(datum$citations~datum$year)

#use model coefficient to make fitted column
datum$fitted <- mod$coefficients[2]*datum$year + mod$coefficients[1]

# calculate cooks d for all data
datum$cooksd <- cooks.distance(mod)

#get upper limit of citation count
upper_limit_citations=min(datum[(datum$cooksd >=3*mean(datum$cooksd, na.rm=T)) & # cooksD is high
          (datum$citations > datum$fitted),]$citations)

datum$norm_cit=ifelse(datum$citations<upper_limit_citations,datum$citations,upper_limit_citations)


# 3b. create new col from OA designations (ie OA, Closed, Other)
datum$`OAdes`= replace_na(datum$`OAdes`, "")
datum$OAlab = ifelse(grepl('Gold', datum$OAdes), 
                     'Other Gold', ifelse(grepl('Bronze', datum$OAdes), 'Bronze',
                                          ifelse(grepl('Green', datum$OAdes), 'Green',
                                                 ifelse(grepl('^$', datum$OAdes), 'Closed Access', 'Error'))))
# 3c. create new col from authors with count
datum <- datum %>% add_column(auth_count = str_count(datum$Authors, ";") + 1)
# 3d. create new col from reprint addresses with author country
#datum <- datum %>% add_column(auth_loc = str_remove(word(datum$corrAuth_loc, -1),"[.]"))
#datum$auth_loc=str_remove(gsub(".*,","",corr_auth_full),"[.]")
datum$auth_loc=ifelse(str_detect(datum$corrAuth_loc,"USA"),str_remove(word(datum$corrAuth_loc, -1),"[.]"),str_remove(gsub(".*,","",datum$corrAuth_loc),"[.]"))
datum$auth_loc=str_trim(datum$auth_loc, side = "left")

#list all countries
myCountries=unique(sort(datum$auth_loc))

#match with country list for maptools package
data("wrld_simpl")
CountryIntersect = myCountries %in% wrld_simpl@data$NAME

#list countries not in list
myCountries[CountryIntersect==FALSE]

#correct countries without a match in datafile
datum$auth_loc[datum$auth_loc=='Zealand'] = 'New Zealand'
datum$auth_loc[datum$auth_loc=='DEM REP CONGO'] = 'Congo'
datum$auth_loc[datum$auth_loc=='BELARUS'] = 'Belarus'
datum$auth_loc[datum$auth_loc=='USA'] = 'United States'
datum$auth_loc[datum$auth_loc=='Rep Congo'] = 'Congo'
datum$auth_loc[datum$auth_loc=='Vietnam'] = 'Viet Nam'
datum$auth_loc[datum$auth_loc=='Wales'] = 'United Kingdom'
datum$auth_loc[datum$auth_loc=='U Arab Emirates'] = 'United Arab Emirates'
datum$auth_loc[datum$auth_loc=='Turks & Caicos'] = 'Turks and Caicos Islands'
datum$auth_loc[datum$auth_loc=='Trinidad Tobago'] = 'Trinidad and Tobago'
datum$auth_loc[datum$auth_loc=='Tanzania'] = 'United Republic of Tanzania'
datum$auth_loc[datum$auth_loc=='Syria'] = 'Syrian Arab Republic'
datum$auth_loc[datum$auth_loc=='St Vincent'] = 'Saint Vincent and the Grenadines'
datum$auth_loc[datum$auth_loc=='St Kitts & Nevi'] = 'Saint Kitts and Nevis'
datum$auth_loc[datum$auth_loc=='Scotland'] = 'United Kingdom'
datum$auth_loc[datum$auth_loc=='Arabia'] = 'Saudi Arabia'
datum$auth_loc[datum$auth_loc=='Myanmar'] = 'Burma'
datum$auth_loc[datum$auth_loc=='Laos'] = 'Lao People\'s Democratic Republic'
datum$auth_loc[datum$auth_loc=='Libya'] = 'Libyan Arab Jamahiriya'
datum$auth_loc[datum$auth_loc=='Peoples R China'] = 'China'
datum$auth_loc[datum$auth_loc=='England'] = 'United Kingdom'
datum$auth_loc[datum$auth_loc=='Papua N Guinea'] = 'Papua New Guinea'
datum$auth_loc[datum$auth_loc=='Ascension Isl'] = 'United Kingdom'
datum$auth_loc[datum$auth_loc=='Bonaire'] = 'Netherlands'
datum$auth_loc[datum$auth_loc=='Bosnia & Herceg'] = 'Bosnia and Herzegovina'
datum$auth_loc[datum$auth_loc=='Brunei'] = 'Brunei Darussalam'
datum$auth_loc[datum$auth_loc=='Emirates'] = 'United Arab Emirates'
datum$auth_loc[datum$auth_loc=='South Korea'] = 'Korea, Republic of'
datum$auth_loc[datum$auth_loc=='Rica'] = 'Costa Rica'
datum$auth_loc[datum$auth_loc=='Macedonia'] = 'The former Yugoslav Republic of Macedonia'
datum$auth_loc[datum$auth_loc=='North Macedonia'] = 'The former Yugoslav Republic of Macedonia'
datum$auth_loc[datum$auth_loc=='Republic'] = 'Czech Republic'
datum$auth_loc[datum$auth_loc=='North Ireland'] = 'United Kingdom'
datum$auth_loc[datum$auth_loc=='Moldova'] = 'Republic of Moldova'
datum$auth_loc[datum$auth_loc=='Kosovo'] = 'Serbia'
datum$auth_loc[datum$auth_loc=='Korea'] = 'Korea, Republic of'
datum$auth_loc[datum$auth_loc=='Iran'] = 'Iran (Islamic Republic of)'
datum$auth_loc[datum$auth_loc=='Guinea Bissau'] = 'Guinea-Bissau'
datum$auth_loc[datum$auth_loc=='Falkland Island'] = 'Falkland Islands (Malvinas)'
datum$auth_loc[datum$auth_loc=='Eswatini'] = 'Swaziland'
datum$auth_loc[datum$auth_loc=='Curacao'] = 'Netherlands Antilles'
datum$auth_loc[datum$auth_loc=='Cote Ivoire'] = 'Cote d\'Ivoire'
datum$auth_loc[datum$auth_loc=='Cent Afr Republ'] = 'Central African Republic'
datum$auth_loc[datum$auth_loc=='Africa'] = 'South Africa'

#list countries not in list (should be zero now!)
myCountries=unique(sort(datum$auth_loc))
CountryIntersect = myCountries %in% wrld_simpl@data$NAME
myCountries[CountryIntersect==FALSE]

# for matched: 3a. create new col(s) with univariate outliers corrected
clean_cols = c('citations') # select cols to correct
for (col in clean_cols) {
  var = matched[[col]] # select col
  quarters = quantile(var,na.rm=TRUE) # determine quartiles
  IQR = quarters[3]-quarters[2] # .75 quartile - .25 quartile (IQR)
  var[var>median(var)+(2*IQR)] = median(var)+(2*IQR) # replace values > upp limit
  var[var<median(var)-(2*IQR)] = median(var)-(2*IQR) # replace values < low limit
  matched[,paste0("clean_",col)] = var # append col
  rm(col,IQR,quarters,var)
}

#a slightly different approach to apply threshold to high citation values - citation count is correlated with year
mod=lm(matched$citations~matched$year)

#use model coefficient to make fitted column
matched$fitted <- mod$coefficients[2]*matched$year + mod$coefficients[1]

# calculate cooks d for all data
matched$cooksd <- cooks.distance(mod)

#get upper limit of citation count
upper_limit_citations=min(matched[(matched$cooksd >=3*mean(matched$cooksd, na.rm=T)) & # cooksD is high
                                  (matched$citations > matched$fitted),]$citations)

matched$norm_cit=ifelse(matched$citations<upper_limit_citations,matched$citations,upper_limit_citations)

# 3b. create new col from OA designations (ie OA, Closed, Other)
matched$OAlab = ifelse(grepl('Gold', matched$OAdes), 
                     'Other Gold', ifelse(grepl('Bronze', matched$OAdes), 'Bronze',
                                          ifelse(grepl('Green', matched$OAdes), 'Green',
                                                 ifelse(grepl('^$', matched$OAdes), 'Closed Access', 'Error'))))
# 3c. create new col from authors with count
matched <- matched %>% add_column(auth_count = str_count(matched$Authors, ";") + 1)
# 3d. create new col from reprint addresses with author country
matched <- matched %>% add_column(auth_loc = str_remove(word(matched$corrAuth_loc, -1),"[.]"))

# 4. remove unneeded cols
datum <- datum %>% select(journal, citations, clean_citations, OAdes, OAlab, 
                          year, auth_loc, auth_count,norm_cit,Volume,Issue)

matched <- matched %>% select(journal, citations, clean_citations, OAdes, OAlab, 
                          year, auth_loc, auth_count,norm_cit,Volume,Issue)
#keep_cols = c('journal','OAdes','citations', 'year', 'Authors', 'corrAuth_loc', 'Publisher')
#datum = datum[keep_cols]

# 5a. adding metadata
md <- read_csv("data/oa_metadata.csv", col_names = TRUE)

# 5b. joining metadata with clean data and writing to output file
datum <- left_join(datum, md, by = "journal")
matched <- left_join(matched, md, by = "journal")
write_csv(datum, file = "data/OA_data_fin.csv")
write_csv(matched, file = "data/matched_OA_data_fin.csv")
