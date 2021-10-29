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

datum <- read_csv("data/matched_OA_data_fin.csv", col_names = TRUE)


# check data
names(datum)
head(datum)
summary(datum)

#combine volume and issue into single column
datum$vol_issue=paste(datum$Volume,datum$Issue,sep=".")

# code for generating tables and output in Latex format
stardat <- as.data.frame(datum)
#stardat <- stardat[na.omit(stardat$APC),]
stargazer(stardat[c("citations", "year", "auth_count",
                    "norm_cit", "Volume", "Issue", "vol_issue", "JCR_quart",
                    "AIS", "APC", "apc_cat")], 
          type = "text",
          title = "Summary Statistics for Full Open Access Dataset",
          covariate.labels = c("Raw Citation Count", "Publication Year",
                               "Author Count", "Normalized Citation Count",
                               "Volume", "Issue", #"Volume and Issue", 
                               "Journal Citation Reports Quartiles",
                               "Article Influence Score", "Article Processing Charge"#, "Article Processing Charge Category"
                               ),
          out = "outputs/stats/matched_OA_datum_sumstats.tex")

# Summary Stats =================
# comment

# print number of records per journal
datum$jour = as.factor(datum$jour) # reset as factor
sink("outputs/stats/recordnum_journal.txt")
summary(datum$jour) # print number
sink()

#print range of records by journal
range(summary(datum$jour))
#number of journals
length(unique(sort(datum$jour)))
#number of field
length(unique(sort(datum$field)))

# grand mean of norm_cit
mean(datum$norm_cit)
# means for norm_cit across levels of categorical variables
myr <- with(datum, tapply(norm_cit, year, mean))
moa <- with(datum, tapply(norm_cit, OAlab, mean))
mjcr <- with(datum, tapply(norm_cit, JCR_quart, mean))
mgni <- with(datum, tapply(norm_cit, gni_class, mean))
# differences between levels for each categ. var.
  #successive when level > 2
dyr <- diff(myr)
doa <- diff(moa)
djcr <- diff(mjcr)
dgni <- diff(mgni)
#means of each combination of all categorical variables
m2var <- with(datum,tapply(norm_cit,list(year,gni_class,OAlab,JCR_quart),mean))
#mean of year when all other categories are reference values
mean(m2var[,"High", "Closed Access", 1])
# summary stats by OA label ---more detail than the mean calculations above, but should agree
sum.stat <- datum %>% group_by(OAlab) %>% 
  summarise(mean_cite = round(mean(norm_cit),2),
            sd_cite = round(sd(norm_cit),2),
            num_cite = length(norm_cit))

yr.stat <- datum %>% group_by(year) %>% 
  summarise(mean_cite = round(mean(norm_cit),2),
            sd_cite = round(sd(norm_cit),2),
            num_cite = length(norm_cit))

jcr.stat <- datum %>% group_by(JCR_quart) %>% 
  summarise(mean_cite = round(mean(norm_cit),2),
            sd_cite = round(sd(norm_cit),2),
            num_cite = length(norm_cit))

gni.stat <- datum %>% group_by(gni_class) %>% 
  summarise(mean_cite = round(mean(norm_cit),2),
            sd_cite = round(sd(norm_cit),2),
            num_cite = length(norm_cit))

# output summary stats
sink("outputs/stats/matched_OAdes_summarystats")
kable(sum.stat)
sink()

# Data Exploration  ================
ls.str(datum)
#need to id variables I want as a factor
datum$jour <- as.factor(datum$jour)
datum$OAlab <- as.factor(datum$OAlab)
datum$field <- as.factor(datum$field)
datum$jour_loc <- as.factor(datum$jour_loc)
datum$JCR_quart <- as.factor(datum$JCR_quart)
datum$pub <- as.factor(datum$pub)
datum$year <- as.factor(datum$year)
datum$gni_class <- as.factor(datum$gni_class)
levels(datum$year)

# Plot histogram; estimate mean & variance for response variables
hist(datum$citations)#hist(datum$clean_citations)
mean(datum$citations) #mean(datum$clean_citations)
var(datum$citations) #var(datum$clean_citations)

hist(datum$norm_cit) # for alternative normalization metric
# clean citations is not normally distributed; likely should not use 
#   linear model to fit these data. should use generalized linear model (Poisson).
mean(datum$norm_cit)
var(datum$norm_cit) # for alternative normalization metric
# the variance is higher than the mean, indicating an expectation of 
#   over-dispersion in the model.

# looking at AIS and author count distributions
hist(datum$AIS)
hist(datum$auth_count, breaks = 100)

# Rescaled numerical fields, into new columns
datum <- datum %>% 
  mutate(auth_count_scaled = scale(auth_count,center = TRUE, scale = TRUE),
         AIS_scaled = scale(AIS,center = TRUE, scale = TRUE))
#datum$auth_count_scaled <- scale(datum$auth_count)
#datum$AIS_scaled <- scale(datum$AIS)
# this makes a matrix inside of our dataframe


# Statistical Tests ================

# Recode year field (deviation- compares level to grand mean)
contrasts(datum$year) = contr.sum(6)
# contrasts(datum$field) = contr.sum(12) no longer relevant with field as random effect

# Scale variables, double-check nestedness/definition of random variable -TANNER

#using citation count raw
mod2.1 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+field+JCR_quart+AIS+(1|field/jour/vol_issue), 
                data = datum, family = poisson)
summary(mod2.1)
Anova(mod2.1)


# Plotting ============

# reset vars
datum$APC = as.numeric(datum$APC)
datum$norm_cit = as.numeric(datum$norm_cit)
datum$jour = as.character(datum$jour)

# subset datum by auth_count
tdatum = subset(datum, jour == c('BRAIN',
                                 'ZOOLOGY',
                                 'ZEBRAFISH'))
tdatum$group = as.character(tdatum$group)

# bivar scatter: X x Y x Z
ggplot(tdatum, aes(x=APC,
                   y=norm_cit,
                  col=group)) +
  geom_point(col='gray') + 
  geom_smooth(method="lm", se=FALSE, size=.5) + 
  geom_abline(aes(intercept=0, 
                  slope=1),
              data=datum, size=1, 
              linetype='longdash')






#compute data summary to add to plot
data_summary <- function (datum) {
  m = mean(datum)
  ymin = m-sd(datum)
  ymax = m+sd(datum)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

#plots access time by author count; not really useful plot
plot(norm_cit ~ APC, data = datum)
ggplot(datum, aes(x=APC, y=norm_cit, color=OAlab)) +
  geom_point() + facet_wrap(~auth_count) 

# violin plot: Citations by Access Designation
vplot <- ggplot(datum,aes(x=OAlab,y=norm_cit,fill=OAlab)) +
  geom_violin(trim=FALSE) 
vplot <- vplot + ggtitle("Open Access Status & Citation Count") +
  xlab("Status") + ylab("Citations") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)

vplot + annotate(geom="text", x="Bronze", y=-20, label = sum.stat[1,4])+
  annotate(geom="text", x="Closed Access", y=-20, label = sum.stat[2,4])+
  annotate(geom="text", x="Green", y=-20, label = sum.stat[3,4])+
  annotate(geom="text", x="Other Gold", y=-20, label = sum.stat[4,4])
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
vplot
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

# violin plot: Citations by Journal C Rank (JCR) quartile

vplot <- ggplot(datum,aes(x=OAlab,y=norm_cit,fill=OAlab)) +
  geom_violin(trim=FALSE) +
  facet_wrap(~JCR_quart)
vplot <- vplot + ggtitle("Open Access Status & Citation Count") +
  xlab("Status") + ylab("Citations") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)

ggsave("clean_vplot_JCR_OAdes.png", device = "png", path ="outputs/plots/", width=4,height=4)



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
#Not a meaningful plot - too much going on!
vplot <- ggplot(datum,aes(x=OAlab,y=norm_cit,fill=OAlab)) +
  geom_violin(trim=FALSE) +
  facet_wrap(~auth_loc)
vplot <- vplot + ggtitle("Open Access Status & Citation Count") +
  xlab("Status") + ylab("Citations") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)
vplot
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


