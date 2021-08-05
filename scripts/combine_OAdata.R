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
#install.packages('doBy')
library(doBy)
#install.packages('reshape2')
library(reshape2)

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
datum <- datum %>% dplyr::rename(jour = 'Source Title',
                          citations = 'Times Cited, All Databases', OAdes = 'Open Access Designations',
                          year = 'Publication Year', corrAuth_loc = 'Reprint Addresses')

matched <- matched %>% dplyr::rename(jour = 'Source Title',
                          citations = 'Times Cited, All Databases', OAdes = 'Open Access Designations',
                          year = 'Publication Year', corrAuth_loc = 'Reprint Addresses')


#names(datum)[names(datum)=='Source Title'] = 'jour'
#names(datum)[names(datum)=='Times Cited, All Databases'] = 'citations'
#names(datum)[names(datum)=='Open Access Designations'] = 'OAdes'
#names(datum)[names(datum)=='Publication Year'] = 'year'
#names(datum)[names(datum)=='Reprint Addresses'] = 'corrAuth_loc'

# 2. clean up journal names
# Removing excluded journals and special issues; making all journal titles uppercase
datum <- datum %>%  
  mutate(jour = toupper(jour)) %>% 
  filter(is.na(`Special Issue`)) %>% 
  filter(year %in% c(2013,2014,2015,2016,2017,2018)) %>% 
  filter(!jour %in% c('ONCOTARGET', 'FUNGAL BIOLOGY REVIEWS', 
                         'PERSOONIA', 'PLANT BIOTECHNOLOGY JOURNAL',
                         'GENERAL AND COMPARATIVE ENDOCRINOLOGY',
                         'AVIAN BIOLOGY RESEARCH', 'DEVELOPMENTAL BIOLOGY',
                         'PAKISTAN JOURNAL OF ZOOLOGY','GAYANA',
                         'ANIMAL CELLS AND SYSTEMS','ITALIAN JOURNAL OF ZOOLOGY',
                         'ACTA THERIOLOGICA', 'NATURE ECOLOGY & EVOLUTION'))
# Correcting specific journal name
datum <- datum %>% 
  mutate(across(jour,str_replace_all, 
                pattern = "JOURNAL OF COMPARATIVE PHYSIOLOGY B-BIOCHEMICAL SYSTEMIC AND ENVIRONMENTAL PHYSIOLOGY",
                replacement = "JOURNAL OF COMPARATIVE PHYSIOLOGY B-BIOCHEMICAL SYSTEMS AND ENVIRONMENTAL PHYSIOLOGY"))
#for matched: Removing excluded journals and special issues; making all journal titles uppercase 
matched <- matched %>%  
  mutate(jour = toupper(jour)) %>% 
  filter(is.na(`Special Issue`)) %>% 
  filter(year %in% c(2013,2014,2015,2016,2017,2018)) %>% 
  filter(!jour %in% c('ONCOTARGET', 'FUNGAL BIOLOGY REVIEWS', 
                         'PERSOONIA', 'PLANT BIOTECHNOLOGY JOURNAL',
                         'GENERAL AND COMPARATIVE ENDOCRINOLOGY',
                         'AVIAN BIOLOGY RESEARCH', 'DEVELOPMENTAL BIOLOGY',
                         'PAKISTAN JOURNAL OF ZOOLOGY','GAYANA',
                         'ANIMAL CELLS AND SYSTEMS','ITALIAN JOURNAL OF ZOOLOGY',
                         'ACTA THERIOLOGICA', 'NATURE ECOLOGY & EVOLUTION'))
# Correcting specific journal name
matched <- matched %>% 
  mutate(across(jour,str_replace_all, 
                pattern = "JOURNAL OF COMPARATIVE PHYSIOLOGY B-BIOCHEMICAL SYSTEMIC AND ENVIRONMENTAL PHYSIOLOGY",
                replacement = "JOURNAL OF COMPARATIVE PHYSIOLOGY B-BIOCHEMICAL SYSTEMS AND ENVIRONMENTAL PHYSIOLOGY"))

##### 3a. create new col(s) with univariate outliers corrected##### 
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
#####
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

# Log-transform norm_cit for use in linear models. Add 1 first to avoid infinite values
datum$norm_cit_log <- log(datum$norm_cit + 1)

# 3b. create new col from OA designations (ie OA, Closed, Other)
datum$OAdes = replace_na(datum$OAdes, "")
datum$OAlab = ifelse(grepl('Gold', datum$OAdes), 
                     'Other Gold', ifelse(grepl('Bronze', datum$OAdes), 'Bronze',
                                          ifelse(grepl('Green', datum$OAdes), 'Green',
                                                 ifelse(grepl('^$', datum$OAdes), 'Closed Access', 'Error'))))
# 3c. create new col from authors with count
datum <- datum %>% add_column(auth_count = str_count(datum$Authors, ";") + 1)

# 3d. create new col from reprint addresses with author country
#datum <- datum %>% add_column(auth_loc = str_remove(word(datum$corrAuth_loc, -1),"[.]"))
#datum$auth_loc=str_remove(gsub(".*,","",corr_auth_full),"[.]")
datum <- datum %>% add_column(corr_auth_count = str_count(datum$corrAuth_loc, ";") + 1)
#datum$auth_loc=ifelse(datum$corr_auth_count>1,NA,
#                      ifelse(str_detect(datum$corrAuth_loc,"USA"),str_remove(word(datum$corrAuth_loc, -1),"[.]"),str_remove(gsub(".*,","",datum$corrAuth_loc),"[.]")))
#datum$auth_loc=str_trim(datum$auth_loc, side = "left")

#read in GNI data to match with corresponding author countries
gni=read.csv(file="data/GNI.Percapita_reduced.csv",header=T,stringsAsFactors = T)

#read in map data
data("wrld_simpl")

#make two column data frame with country names and 3 letter codes
countries=data.frame(wrld_simpl@data$NAME,wrld_simpl@data$ISO3)

#test out code to extract multiple countries for one pub (use max value 19 as test)
#test=datum[datum$corr_auth_count>=17 & !is.na(datum$corr_auth_count),c("corrAuth_loc")]
auth_loc=vector(length = length(datum$corrAuth_loc))
corrAuth_code=vector(length = length(datum$corrAuth_loc))
corrAuth_gni=vector(length = length(datum$corrAuth_loc))

for (t in 1:length(datum$corrAuth_loc)) {

  test2=unlist(strsplit(as.character(datum$corrAuth_loc[t]), ";"))
  test3=vector(length=length(test2))
  
  for (i in 1:length(test2)) {
    test3[i]=ifelse(str_detect(test2[i],"corresponding author"),ifelse(str_detect(test2[i],"USA"),str_remove(word(test2[i], -1),"[.]"),str_remove(gsub(".*,","",test2[i]),"[.]")),NA)
    test3[i]=str_trim(test3[i], side = "left")
    }
  
  #get unique non-NA values
  test4=unique(sort(na.omit(test3)))
  #add in code to calculate GNI for corresponding author country (use max GNI for country and value)
  #auth_loc[t]=ifelse(length(test4)==1,test4,NA)
  coun_codes=vector(length=length(test4))
  max_gni=vector(length=length(test4))
  for (u in 1:length(test4)) {

    #ugly ifelse nest to rename countries
    test4[u]=ifelse(test4[u]=='Republic','Czech Republic',ifelse(test4[u]=='Zealand','New Zealand',
    ifelse(test4[u]=='USA','United States',ifelse(test4[u]=='DEM REP CONGO','Congo',
    ifelse(test4[u]=='BELARUS','Belarus',ifelse(test4[u]=='Rep Congo','Congo',ifelse(test4[u]=='Vietnam','Viet Nam',
    ifelse(test4[u]=='Wales','United Kingdom',ifelse(test4[u]=='U Arab Emirates','United Arab Emirates',
    ifelse(test4[u]=='Turks & Caicos','Turks and Caicos Islands',ifelse(test4[u]=='Trinidad Tobago','Trinidad and Tobago',
    ifelse(test4[u]=='Tanzania','United Republic of Tanzania',ifelse(test4[u]=='Syria','Syrian Arab Republic',
    ifelse(test4[u]=='St Vincent','Saint Vincent and the Grenadines',ifelse(test4[u]=='St Kitts & Nevi','Saint Kitts and Nevis',
    ifelse(test4[u]=='Scotland','United Kingdom',ifelse(test4[u]=='Arabia','Saudi Arabia',ifelse(test4[u]=='Myanmar','Burma',
    ifelse(test4[u]=='Laos','Lao People\'s Democratic Republic',ifelse(test4[u]=='Libya','Libyan Arab Jamahiriya',
    ifelse(test4[u]=='Peoples R China','China',ifelse(test4[u]=='England','United Kingdom',
    ifelse(test4[u]=='Papua N Guinea','Papua New Guinea',ifelse(test4[u]=='Ascension Isl','United Kingdom',
    ifelse(test4[u]=='Bonaire','Netherlands',ifelse(test4[u]=='Bosnia & Herceg','Bosnia and Herzegovina',
    ifelse(test4[u]=='Brunei','Brunei Darussalam',ifelse(test4[u]=='Emirates','United Arab Emirates',
    ifelse(test4[u]=='South Korea','Korea, Republic of',ifelse(test4[u]=='Rica','Costa Rica',
    ifelse(test4[u]=='Macedonia','The former Yugoslav Republic of Macedonia',
    ifelse(test4[u]=='North Macedonia','The former Yugoslav Republic of Macedonia',
    ifelse(test4[u]=='North Ireland','United Kingdom',ifelse(test4[u]=='Moldova','Republic of Moldova',
    ifelse(test4[u]=='Kosovo','Serbia',ifelse(test4[u]=='Korea','Korea, Republic of',
    ifelse(test4[u]=='Iran','Iran (Islamic Republic of)',ifelse(test4[u]=='Guinea Bissau','Guinea-Bissau',
    ifelse(test4[u]=='Falkland Island','Falkland Islands (Malvinas)',ifelse(test4[u]=='Eswatini','Swaziland',
    ifelse(test4[u]=='Curacao','Netherlands Antilles',ifelse(test4[u]=='Cote Ivoire','Cote d\'Ivoire',
    ifelse(test4[u]=='Cent Afr Republ','Central African Republic',ifelse(test4[u]=='Africa','South Africa',test4[u]))))))))))))))))))))))))))))))))))))))))))))
    
    test5=ifelse(length(as.character(countries$wrld_simpl.data.ISO3[countries$wrld_simpl.data.NAME==test4[u]]))==0,NA,as.character(countries$wrld_simpl.data.ISO3[countries$wrld_simpl.data.NAME==test4[u]]))

    coun_codes[u]=test5
    max_gni[u]=ifelse(length(gni$AVG_2013.2018[gni$Country.Code==test5])==0,NA,gni$AVG_2013.2018[gni$Country.Code==test5])
  }
  corrAuth_gni[t]=ifelse(all(is.na(max_gni)),NA,ifelse(length(max_gni)==1,max_gni,max(max_gni,na.rm=T)))
  corrAuth_code[t]=ifelse(is.na(corrAuth_gni[t]),coun_codes[1],ifelse(length(coun_codes)==1,coun_codes,coun_codes[which(max_gni==corrAuth_gni[t])[[1]]]))
  auth_loc[t]=ifelse(is.na(corrAuth_gni[t]),test4[1],ifelse(length(test4)==1,test4,test4[which(max_gni==corrAuth_gni[t])[[1]]]))
}

#three new columns to datum
datum$auth_loc=auth_loc
datum$corrAuth_gni=corrAuth_gni
datum$corrAuth_code=corrAuth_code

#list all countries
myCountries=unique(sort(datum$auth_loc))
#write.csv(myCountries,file="data/corresponding_author_country_list.csv",row.names = F,quote = F)

#match with country list for maptools package
CountryIntersect = myCountries %in% wrld_simpl@data$NAME

#list countries not in list; sanity check! All should already be in the list from nasty ifelse statement within the loop!
myCountries[CountryIntersect==FALSE]


##### for matched: 3a. create new col(s) with univariate outliers corrected #####
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
#####
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

# Log-transform norm_cit for use in linear models. Add 1 first to avoid infinite values
matched$norm_cit_log <- log(matched$norm_cit + 1)

# 3b. create new col from OA designations (ie OA, Closed, Other)
matched$OAdes = replace_na(matched$OAdes, "")
matched$OAlab = ifelse(grepl('Gold', matched$OAdes), 
                     'Other Gold', ifelse(grepl('Bronze', matched$OAdes), 'Bronze',
                                          ifelse(grepl('Green', matched$OAdes), 'Green',
                                                 ifelse(grepl('^$', matched$OAdes), 'Closed Access', 'Error'))))
# 3c. create new col from authors with count
matched <- matched %>% add_column(auth_count = str_count(matched$Authors, ";") + 1)
# 3d. create new col from reprint addresses with author country
matched <- matched %>% add_column(auth_loc = str_remove(word(matched$corrAuth_loc, -1),"[.]"))

# 4. select desired cols
datum <- datum %>% dplyr::select(jour, citations, clean_citations, OAdes, OAlab, 
                          year, auth_loc, auth_count,norm_cit,norm_cit_log,Volume,Issue)

matched <- matched %>% dplyr::select(jour, citations, clean_citations, OAdes, OAlab, 
                          year, auth_loc, auth_count,norm_cit,norm_cit_log,Volume,Issue)
#keep_cols = c('journal','OAdes','citations', 'year', 'Authors', 'corrAuth_loc', 'Publisher')
#datum = datum[keep_cols]

# 5a. adding metadata
md <- read_csv("data/oa_metadata.csv", col_names = TRUE)
# exclude empty fields
md <- md %>% dplyr::select(!(starts_with("X")))
md$APC <- as.numeric(gsub("[\\$,]", "", md$APC))

# 5b. joining metadata with clean data and writing to output file
datum <- left_join(datum, md, by = "jour")
matched <- left_join(matched, md, by = "jour")
write_csv(datum, file = "data/OA_data_fin.csv")
write_csv(matched, file = "data/matched_OA_data_fin.csv")

# 6. Make data tables
t1=datum[,c("year","jour","field","norm_cit","OAlab")]
t1$jour=as.factor(t1$jour)

#make dummy column to count articles
t1$art=1

#use art column to get sum of articles by field/journal
num.art=summaryBy(art~field+jour,data=t1,FUN=c(mean,sum))
colnames(num.art)=c("field","jour","jour_count","Number articles")

#further summarize by field to get journal count and article count by field
num.jour=summaryBy(jour_count+`Number articles`~field,data=num.art,FUN=c(sum))

#add a column for the matched data
t1m=matched[,c("year","jour","field","norm_cit","OAlab")]
t1m$jour=as.factor(t1m$jour)
t1m$art=1
num.art3=summaryBy(art~field+jour,data=t1m,FUN=c(mean,sum))
colnames(num.art3)=c("field","jour","jour_count","Number articles")
num.jour2=summaryBy(jour_count+`Number articles`~field,data=num.art3,FUN=c(sum))

#remove NA field (need to fix later!)
#num.jour2=num.jour2[complete.cases(num.jour2),]
table1=cbind(num.jour,num.jour2$`Number articles.sum`)

#get mean cit per access type
num.art2=dcast(t1,field~OAlab,value.var="art",fun.aggregate = length)

table1=cbind(table1,num.art2[,2:5])

#add col names
colnames(table1)=c("Research Area","Number of Journals","Number of Articles","Number of Matched Articles","Bronze","Closed Access","Green","Other Gold")

#fix field names
table1$`Research Area`=c("Biochemistry & Molecular Biology","Cell Biology","Entomology","Evolutionary Biology","Genetics & Heredity",
               "Marine & Freshwater Biology","Microbiology","Mycology","Neurosciences & Neurology","Oncology",         
               "Plant Sciences","Zoology")

#make row for totals and grand means
totals=c("Totals",round(sum(table1$`Number of Journals`),0),round(sum(table1$`Number of Articles`),0),round(sum(table1$`Number of Matched Articles`),0),sum(table1$Bronze),sum(table1$`Closed Access`),sum(table1$Green),sum(table1$`Other Gold`))

#rbind totals to table1
table1_new=rbind(table1,totals)

#output table
write.csv(table1_new,"outputs/stats/Table1_Data_Summary.csv",row.names=F,quote=F)
