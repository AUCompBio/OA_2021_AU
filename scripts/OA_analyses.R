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
# install.packages('plyr') # run once
library(plyr)
# install.packages('readxl') # run once
library(readxl)
# install.packages('tidyverse') # run once
library(tidyverse)

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

# check raw data
names(datum)
head(datum)
summary(datum)

# Exporting journal names to a csv for validation and matching
journals <- data.frame(unique(datum$`Source Title`))
write_csv(journals, file = "output/tables/jourlist.csv")

# Clean up =====================
# 1. rename cols
# 2. drop unneeded cols
# 3. create new cols
# 4. clean names

# 1. rename cols
names(datum)[names(datum)=='Source Title'] = 'journal'
names(datum)[names(datum)=='Times Cited, All Databases'] = 'citations'
names(datum)[names(datum)=='Open Access Designations'] = 'OAdes'

# 2. remove unneeded cols
keep_cols = c('journal','OAdes','citations')
datum = datum[keep_cols]

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

# 4. clean up journal names
# Removing excluded journals, making all journal titles uppercase
datum <- datum %>%  
  mutate(journal = toupper(journal)) %>% 
  filter(!journal %in% c('ONCOTARGET', 'FUNGAL BIOLOGY REVIEWS', 
                       'PERSOONIA', 'PLANT BIOTECHNOLOGY JOURNAL'))
# Correcting specific journal name
datum <- datum %>% 
  mutate(across(journal,str_replace_all, 
                pattern = "JOURNAL OF COMPARATIVE PHYSIOLOGY B-BIOCHEMICAL SYSTEMIC AND ENVIRONMENTAL PHYSIOLOGY",
                replacement = "JOURNAL OF COMPARATIVE PHYSIOLOGY B-BIOCHEMICAL SYSTEMS AND ENVIRONMENTAL PHYSIOLOGY"))

# 5. adding metadata
md <- read_csv("data/Clean_JournalImpact.csv", col_names = TRUE)
md <- md %>% rename(journal = "Selected_Journals")
datum <- left_join(datum, md, by = "journal")


# Summary Stats =================
# comment

# print number of records per journal
datum$journal = as.factor(datum$journal) # reset as factor
sink("outputs/stats/recordnum_journal.txt")
summary(datum$journal) # print number
sink()

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

# Statistical Tests ================
#  (run lm (anova), export results))

# lm (anova): test whether OAlab predicts citations
model = lm(clean_citations~relevel(as.factor(OAlab), ref = "Closed Access"),data=datum, subset = c(OAlab != "Error"))
anova(model)
summary(model)
confint(model)

# export results from anova to .txt file
sink("clean_results_anova_WOS2.txt")
print(summary(model))
print(confint(model))
sink()  # returns output to the console

# Plotting ============
#  (lm (anova) results)

#compute data summary to add to plot
data_summary <- function (datum) {
  m = mean(datum)
  ymin = m-sd(datum)
  ymax = m+sd(datum)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# violin plot: Citations by Access Designation
vplot <- ggplot(datum,aes(x=OAlab,y=clean_citations,fill=OAlab)) +
  geom_violin(trim=FALSE) 
vplot <- vplot + ggtitle("Open Access Status & Citation Count") +
  xlab("Status") + ylab("Citations") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)

vplot + annotate(geom="text", x="Bronze", y=-4.75, label = Bronss)+
  annotate(geom="text", x="Closed Access", y=-4.75, label = nonOAss)+
  annotate(geom="text", x="Green", y=-4.75, label = Greenss)+
  annotate(geom="text", x="Other Gold", y=-4.75, label = OGss)
# + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.8, binwidth=1) # add above to include data points in plot
ggsave("clean_vplot_OAdes.png", device = "png", path ="outputs/plots/", width=4,height=4)


# violin plot: Citations by Access Designation for each field

vplot <- ggplot(datum,aes(x=OAlab,y=clean_citations,fill=OAlab)) +
  geom_violin(trim=FALSE) +
  facet_wrap(~ Research_Field)
vplot <- vplot + ggtitle("Open Access Status & Citation Count") +
  xlab("Status") + ylab("Citations") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)

ggsave("clean_vplot_Field_OAdes.png", device = "png", path ="outputs/plots/", width=4,height=4)
