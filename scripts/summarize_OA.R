# clear workspace
rm(list=ls(all.names=TRUE))

# install.packages('ggplot2') # run once
library(ggplot2)
# install.packages('tidyverse') # run once
library(tidyverse)
# install.packages('broom') # run once
library(broom)
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
#install.packages("numDeriv")
library(numDeriv)
#install.packages("performance")
library(performance)
#install.packages("stargazer")
library(stargazer) # stargazer does not like tibbles

#read in final data and matched datasets
datum <- read_csv("data/OA_data_fin.csv", col_names = TRUE)
matched <- read_csv("data/matched_OA_data_fin.csv", col_names = TRUE)

### COPIED FROM "combine_OAdata.R" ###
# 6. Make data tables
t1=datum[,c("year","jour","field","citations","OAlab")]
t1$jour=as.factor(t1$jour)

#make dummy column to count articles
t1$art=1

#use art column to get sum of articles by field/journal
num.art=summaryBy(art~field+jour,data=t1,FUN=c(mean,sum))
colnames(num.art)=c("field","jour","jour_count","Number articles")

#further summarize by field to get journal count and article count by field
num.jour=summaryBy(jour_count+`Number articles`~field,data=num.art,FUN=c(sum))

#add a column for the matched data
t1m=matched[,c("year","jour","field","citations","OAlab")] #need to edit this since the name of matched data has changed!
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



### COPIED FROM "analyze_OAdata.R" ###
# Plotting ============

#remove outlier citation counts to facilitate plotting
#start with cutoff of 5k (removes 5 datapoints)
#increase to 500 (removes 195 more datapoint)
datum=subset(datum, datum$citations<=500)

#need to also remove zeros
datum=subset(datum, datum$citations!=0)

# reset vars
#datum$APC = as.numeric(datum$APC)
datum$citations = as.numeric(datum$citations)
datum$jour = as.character(datum$jour)

# subset datum by auth_count
#tdatum = subset(datum, jour == c('BRAIN',
#                                 'ZOOLOGY',
#                                 'ZEBRAFISH'))
#tdatum$group = as.character(tdatum$group)

# bivar scatter: X x Y x Z
#ggplot(tdatum, aes(x=APC,
#                   y=citations,
#                   col=group)) +
#  geom_point(col='gray') + 
#  geom_smooth(method="lm", se=FALSE, size=.5) + 
#  geom_abline(aes(intercept=0, 
#                  slope=1),
 #             data=datum, size=1, 
  #            linetype='longdash')






#compute data summary to add to plot
data_summary <- function (datum) {
  m = mean(datum)
  ymin = m-sd(datum)
  ymax = m+sd(datum)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

#plots access time by author count; not really useful plot
#plot(norm_cit ~ APC, data = datum)
#ggplot(datum, aes(x=APC, y=norm_cit, color=OAlab)) +
#  geom_point() + facet_wrap(~auth_count) 



# violin plot: Citations by Access Designation
vplot <- ggplot(datum,aes(x=OAlab,y=citations,fill=OAlab)) +
  geom_violin(trim=TRUE) 
vplot <- vplot + ggtitle("Open Access Status & Citation Count") +
  xlab("Status") + ylab("Citations") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)

#vplot + annotate(geom="text", x="Bronze", y=-20, label = sum.stat[1,4])+
#  annotate(geom="text", x="Closed Access", y=-20, label = sum.stat[2,4])+
#  annotate(geom="text", x="Green", y=-20, label = sum.stat[3,4])+
#  annotate(geom="text", x="Other Gold", y=-20, label = sum.stat[4,4])
# + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.8, binwidth=1) # add above to include data points in plot
vplot
#  ggsave("clean_vplot_OAdes.png", device = "png", path ="outputs/plots/", width=4,height=4)


# violin plot: Citations by Access Designation for each field

vplot <- ggplot(datum,aes(x=OAlab,y=citations,fill=OAlab)) +
  geom_violin(trim=FALSE) +
  facet_wrap(~field)
vplot <- vplot + ggtitle("Open Access Status & Citation Count") +
  xlab("Status") + ylab("Citations") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)
vplot
#ggsave("clean_vplot_Field_OAdes.png", device = "png", path ="outputs/plots/", width=4,height=4)

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

# violin plot: Citations by Journal C Rank (JCR) quartile

vplot <- ggplot(datum,aes(x=OAlab,y=citations,fill=OAlab)) +
  geom_violin(trim=TRUE) +
  facet_wrap(~JCR_quart)
vplot <- vplot + ggtitle("Open Access Status & Citation Count") +
  xlab("Status") + ylab("Citations") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)
vplot
#ggsave("clean_vplot_JCR_OAdes.png", device = "png", path ="outputs/plots/", width=4,height=4)



# violin plot: Citations by Year Published

vplot <- ggplot(datum,aes(x=OAlab,y=citations,fill=OAlab)) +
  geom_violin(trim=TRUE) +
  facet_wrap(~year)
vplot <- vplot + ggtitle("Open Access Status & Citation Count per Year") +
  xlab("Status") + ylab("Citations") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)
vplot
#ggsave("clean_vplot_Year_OAdes.png", device = "png", path ="outputs/plots/", width=4,height=4)



# violin plot: Citations by APC

vplot <- ggplot(datum,aes(x=apc_cat,y=citations)) +
  geom_violin(trim=TRUE) 
vplot <- vplot + ggtitle("Citation Count per APC") +
  xlab("Status") + ylab("Citations") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)
vplot
#ggsave("clean_vplot_APC_OAdes.png", device = "png", path ="outputs/plots/", width=4,height=4)

# violin plot: Citations by Publisher
#data not in the dataset???
#vplot <- ggplot(datum,aes(x=OAlab,y=norm_cit,fill=OAlab)) +
#  geom_violin(trim=FALSE) +
#  facet_wrap(~publisher)
#vplot <- vplot + ggtitle("Open Access Status & Citation Count") +
#  xlab("Status") + ylab("Citations") + 
#  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
#  stat_summary(fun.data=data_summary)

#ggsave("clean_vplot_Publisher_OAdes.png", device = "png", path ="outputs/plots/", width=4,height=4)


# violin plot: Citations by Corresponding Author Country of Origin
#Not a meaningful plot - too much going on!
#vplot <- ggplot(datum,aes(x=OAlab,y=norm_cit,fill=OAlab)) +
#  geom_violin(trim=FALSE) +
#  facet_wrap(~auth_loc)
#vplot <- vplot + ggtitle("Open Access Status & Citation Count") +
#  xlab("Status") + ylab("Citations") + 
#  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
#  stat_summary(fun.data=data_summary)
#vplot
#ggsave("clean_vplot_Auth_Loc_OAdes.png", device = "png", path ="outputs/plots/", width=4,height=4)




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
data(wrld_simpl)
brks=round(quantile(a_coo_merged$cit_diff,na.rm=T),2)
a_coo_merged$cd_quant=cut(a_coo_merged$cit_diff,breaks=brks,labels=F,include.lowest=T)
colours=rev(c("#cb181d","#fb6a4a","#fcae91","#fee5d9"))
a_coo_merged$colors=as.character(cut(a_coo_merged$cit_diff,breaks=brks,labels=colours,include.lowest=T))

#remove NAs
a_coo_merged=subset(a_coo_merged,!is.na(a_coo_merged$colors))

myCountries = wrld_simpl@data$NAME %in% a_coo_merged$auth_loc
test=data.frame(myCountries,wrld_simpl@data$NAME)
new_col=vector(length = length(wrld_simpl@data$NAME))
#get color list
for (g in 1:length(wrld_simpl@data$NAME)) {
  new_col[g]=ifelse(test$myCountries[g]==TRUE,a_coo_merged$colors[a_coo_merged$auth_loc==test$wrld_simpl.data.NAME[[g]]],"#FFFFFF")
}
test$color=new_col

png(filename="outputs/plots/clean_map_Auth_Loc_Cit_Diff.png",res=300,pointsize=7,width=8,height=6,units="in")
plot(wrld_simpl, col = test$color)
legend(x=c(-185.8, 7.1), y=c(13, 14.5), legend=leglabs(brks),
       fill=(colours), bty="n",cex=1.5)
dev.off()

