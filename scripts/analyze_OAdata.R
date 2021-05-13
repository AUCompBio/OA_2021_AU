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
#



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
