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
OGmean = mean(OG$norm_cit)
OGsd = sd(OG$norm_cit)
# summary stats: bronze
Bron = subset(datum,OAlab=='Bronze') # subset data to bronze
Bronss = length(Bron$journal)
Bronmean = mean(Bron$norm_cit)
Bronsd = sd(Bron$norm_cit)
# summary stats: green
Green = subset(datum,OAlab=='Green') # subset data to green
Greenss = length(Green$journal)
Greenmean = mean(Green$norm_cit)
Greensd = sd(Green$norm_cit)
# summary stats: non open access (non OA)
nonOA = subset(datum,OAlab=='Closed Access') # subset data to non OA
nonOAss = length(nonOA$journal)
nonOAmean = mean(nonOA$norm_cit)
nonOAsd = sd(nonOA$norm_cit)

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
# Bottom up model selection


# lm (anova): test whether OAlab predicts citations
m1 = lm(clean_citations~relevel(as.factor(OAlab), ref = "Closed Access"),
        data=datum, subset = c(OAlab != "Error"))
anova(m1)
summary(m1)
confint(m1)

m2 = lm(clean_citations~relevel(as.factor(OAlab), ref = "Closed Access") + IF_5Y_2019,
        data=datum, subset = c(OAlab != "Error"))
anova(m2)
summary(m2)
confint(m2)

m3 = lm(clean_citations~relevel(as.factor(OAlab), ref = "Closed Access") +
          IF_5Y_2019 + year ,data=datum, subset = c(OAlab != "Error"))
anova(m3)
summary(m3)
confint(m3)

m4 = lm(clean_citations~relevel(as.factor(OAlab), ref = "Closed Access") +
          IF_5Y_2019 + year + field ,data=datum, subset = c(OAlab != "Error"))
anova(m4)
summary(m4)
confint(m4)

m5 = lm(clean_citations~relevel(as.factor(OAlab), ref = "Closed Access") +
          IF_5Y_2019 + year + field + publisher,data=datum, subset = c(OAlab != "Error"))
anova(m5)
summary(m5)
confint(m5)

m6 = lm(clean_citations~relevel(as.factor(OAlab), ref = "Closed Access") +
          IF_5Y_2019 + year + field + publisher + auth_count,data=datum, subset = c(OAlab != "Error"))
anova(m6)
summary(m6)
confint(m6)

m7 = lm(clean_citations~relevel(as.factor(OAlab), ref = "Closed Access") +
          IF_5Y_2019 + year + field + publisher + auth_count + auth_loc,data=datum, subset = c(OAlab != "Error"))
anova(m7)
summary(m7)
confint(m7)

m8 = lm(clean_citations~relevel(as.factor(OAlab), ref = "Closed Access") +
          IF_5Y_2019 + year + field + publisher + auth_count + auth_loc + jour_loc,data=datum, subset = c(OAlab != "Error"))
anova(m8)
summary(m8)
confint(m8)

m9 = lm(clean_citations~relevel(as.factor(OAlab), ref = "Closed Access") *
          year + IF_5Y_2019 + field + publisher + auth_count + auth_loc + jour_loc,data=datum, subset = c(OAlab != "Error"))
anova(m9)
summary(m9)
confint(m9)

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
vplot <- ggplot(datum,aes(x=OAlab,y=norm_cit,fill=OAlab)) +
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

vplot <- ggplot(datum,aes(x=OAlab,y=norm_cit,fill=OAlab)) +
  geom_violin(trim=FALSE) +
  facet_wrap(~field)
vplot <- vplot + ggtitle("Open Access Status & Citation Count") +
  xlab("Status") + ylab("Citations") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)

ggsave("clean_vplot_Field_OAdes.png", device = "png", path ="outputs/plots/", width=4,height=4)

#further delving into the research fields
#downloaded from InCites
fields=read.csv(file="data/Incites Research Areas Report Updated.csv",header=T)
#fields=fields[1:14,]

fun <- function(x){
  c(m=mean(x), v=var(x), n=length(x))
}

#use doBy and reshape2 packages to summarize data by Author Country of Origin
rfs=summaryBy(clean_citations~field+OAlab, data=datum,FUN=fun)

#reshape data to have mean citation count for each type of access
rf_mean=dcast(rfs,field~OAlab,value.var="clean_citations.m")

#tally total records per country
rf_total=dcast(rfs,field~OAlab,value.var="clean_citations.n")
rf_total$total_cit=rf_total$Bronze+rf_total$`Closed Access`+rf_total$Green+rf_total$`Other Gold`

#merge citation count with mean citations
rf_combined=cbind(rf_mean,rf_total[,2:6])
colnames(rf_combined)=c("field","Bronze mean cit","Closed Access mean cit","Green mean cit","Other gold mean cit","Bronze total records","Closed Access total records","Green total records","Other Gold total records","total records")
rf_combined$field[1]="Biochemistry"
rf_merged=merge(rf_combined,fields,by.x="field",by.y="Name")

#calculate difference in citations from open to closed
rf_merged$bronze_cit_diff=ifelse(is.na(rf_merged$`Bronze total records`),NA,rf_merged$`Bronze mean cit`-rf_merged$`Closed Access mean cit`)
rf_merged$green_cit_diff=ifelse(is.na(rf_merged$`Green total records`),NA,rf_merged$`Green mean cit`-rf_merged$`Closed Access mean cit`)
rf_merged$gold_cit_diff=ifelse(is.na(rf_merged$`Other Gold total records`),NA,rf_merged$`Other gold mean cit`-rf_merged$`Closed Access mean cit`)
rf_merged$cit_diff=ifelse(is.na(rf_merged$`total records`),NA,((rf_merged$`Other gold mean cit`+rf_merged$`Green mean cit`+rf_merged$`Bronze mean cit`)/3)-rf_merged$`Closed Access mean cit`)

#plot cit diff versus field rank
png(filename="outputs/plots/fieldRank_vs_cit_diff.png",width=8,height=8,res=300,pointsize=9,units="in")
par(mfrow=c(2,2)) 
plot(rf_merged$Rank,rf_merged$cit_diff,main="Research Field Rank vs. OA Cit Difference (Overall)",xlab="Research Field Rank",ylab="All OA vs CA citation # diff",pch=16)
plot(rf_merged$Rank,rf_merged$bronze_cit_diff,main="Research Field Rank vs. Bronze OA Cit Difference",xlab="Research Field Rank",ylab="Bronze OA vs CA citation # diff",pch=16)
plot(rf_merged$Rank,rf_merged$green_cit_diff,main="Research Field Rank vs. Green OA Cit Difference",xlab="Research Field Rank",ylab="Green OA vs CA citation # diff",pch=16)
plot(rf_merged$Rank,rf_merged$gold_cit_diff,main="Research Field Rank vs. Other Gold OA Cit Difference",xlab="Research Field Rank",ylab="Other Gold OA vs CA citation # diff",pch=16)
dev.off()

# violin plot: Citations by Journal Impact Factor (JIF) quantile

vplot <- ggplot(datum,aes(x=OAlab,y=clean_citations,fill=OAlab)) +
  geom_violin(trim=FALSE) +
  facet_wrap(~JIFquant)
vplot <- vplot + ggtitle("Open Access Status & Citation Count") +
  xlab("Status") + ylab("Citations") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)

ggsave("clean_vplot_JIF_OAdes.png", device = "png", path ="outputs/plots/", width=4,height=4)



# violin plot: Citations by Year Published

vplot <- ggplot(datum,aes(x=OAlab,y=norm_cit,fill=OAlab)) +
  geom_violin(trim=FALSE) +
  facet_wrap(~year)
vplot <- vplot + ggtitle("Open Access Status & Citation Count") +
  xlab("Status") + ylab("Citations") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)

ggsave("clean_vplot_Year_OAdes.png", device = "png", path ="outputs/plots/", width=4,height=4)


# violin plot: Citations by Publisher

vplot <- ggplot(datum,aes(x=OAlab,y=norm_cit,fill=OAlab)) +
  geom_violin(trim=FALSE) +
  facet_wrap(~publisher)
vplot <- vplot + ggtitle("Open Access Status & Citation Count") +
  xlab("Status") + ylab("Citations") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)

ggsave("clean_vplot_Publisher_OAdes.png", device = "png", path ="outputs/plots/", width=4,height=4)


# violin plot: Citations by Corresponding Author Country of Origin

vplot <- ggplot(datum,aes(x=OAlab,y=clean_citations,fill=OAlab)) +
  geom_violin(trim=FALSE) +
  facet_wrap(~auth_loc)
vplot <- vplot + ggtitle("Open Access Status & Citation Count") +
  xlab("Status") + ylab("Citations") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)

ggsave("clean_vplot_Auth_Loc_OAdes.png", device = "png", path ="outputs/plots/", width=4,height=4)




fun <- function(x){
  c(m=mean(x), v=var(x), n=length(x))
}

#use doBy and reshape2 packages to summarize data by Author Country of Origin
als=summaryBy(norm_cit~auth_loc+OAlab, data=datum,FUN=fun)

#reshape data to have column for each type of access
a_coo=dcast(als,auth_loc~OAlab,value.var="norm_cit.m")

#tally total records per country
a_coo2=dcast(als,auth_loc~OAlab,value.var="norm_cit.n")
a_coo2$total_cit=a_coo2$Bronze+a_coo2$`Closed Access`+a_coo2$Green+a_coo2$`Other Gold`

#merge citation count with mean citations
a_coo_merged=cbind(a_coo,a_coo2[,2:6])
colnames(a_coo_merged)=c("auth_loc","Bronze mean cit","Closed Access mean cit","Green mean cit","Other gold mean cit","Bronze total records","Closed Access total records","Green total records","Other Gold total records","total records")

#calculate difference in citations from open to closed
a_coo_merged$bronze_cit_diff=ifelse(is.na(a_coo_merged$`Bronze total records`),NA,a_coo_merged$`Bronze mean cit`-a_coo_merged$`Closed Access mean cit`)
a_coo_merged$green_cit_diff=ifelse(is.na(a_coo_merged$`Green total records`),NA,a_coo_merged$`Green mean cit`-a_coo_merged$`Closed Access mean cit`)
a_coo_merged$gold_cit_diff=ifelse(is.na(a_coo_merged$`Other Gold total records`),NA,a_coo_merged$`Other gold mean cit`-a_coo_merged$`Closed Access mean cit`)
a_coo_merged$cit_diff=ifelse(is.na(a_coo_merged$`total records`),NA,((a_coo_merged$`Other gold mean cit`+a_coo_merged$`Green mean cit`+a_coo_merged$`Bronze mean cit`)/3)-a_coo_merged$`Closed Access mean cit`)


#summarize # citations increase due to Open Access
hist(a_coo_merged$cit_diff,main="Difference in citation count due to Open Access",xlab="Difference")
summary(a_coo_merged$cit_diff)

#note: would love to make a map with these values presented as a heat map!
#install.packages('maptools')
library(maptools)
data(wrld_simpl)
brks=round(quantile(a_coo_merged$cit_diff,na.rm=T),2)
a_coo_merged$cd_quant=cut(a_coo_merged$cit_diff,breaks=brks,labels=F,include.lowest=T)
colours=c("#cb181d","#fb6a4a","#fcae91","#fee5d9")
a_coo_merged$colors=as.character(cut(a_coo_merged$cit_diff,breaks=brks,labels=colours,include.lowest=T))

myCountries = wrld_simpl@data$NAME %in% a_coo_merged$auth_loc
png(filename="outputs/plots/clean_map_Auth_Loc_Cit_Diff.png",res=300,pointsize=7,width=8,height=6,units="in")
plot(wrld_simpl, col = a_coo_merged$colors[myCountries])
legend(x=c(-185.8, 7.1), y=c(13, 14.5), legend=leglabs(brks),
       fill=rev(colours), bty="n",cex=1.5)
dev.off()
