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

datum <- read_csv("data/OA_data_fin.csv", col_names = TRUE)


# check data
names(datum)
head(datum)
summary(datum)



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

# summary stats by OA label
sum.stat <- datum %>% group_by(OAlab) %>% 
  summarise(mean_cite = round(mean(clean_citations),2),
            sd_cite = round(sd(clean_citations),2),
            num_cite = length(clean_citations))


# output summary stats
sink("outputs/stats/OAdes_summarystats.txt")
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



# Plot histogram
hist(datum$clean_citations) 
hist(datum$norm_cit) # for alternative normalization metric
# clean citations is not normally distributed; likely should not use 
#   linear model to fit these data. should use generalized linear model (Poisson).
hist(datum$AIS)
mean(datum$clean_citations)
var(datum$clean_citations)
mean(datum$norm_cit)
var(datum$norm_cit) # for alternative normalization metric
# the variance is higher than the mean, indicating an expectation of 
#   over-dispersion in the model.

# Log-transform norm_cit for use in linear models. Add 1 first to avoid infinite values
datum$norm_cit_log <- log(datum$norm_cit + 1)
# Plot histogram and estimate mean & variance
hist(datum$norm_cit_log)
mean(datum$norm_cit_log)
var(datum$norm_cit_log)

# Statistical Tests ================

# Recode categorical fields (deviation- compares level to grand mean)
contrasts(datum$field) = contr.sum(12)
contrasts(datum$year) = contr.sum(6)

# general linear model using norm_cite as the response
mod1 <- lmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count+field+JCR_quart+AIS+APC+year+(1|field:jour), 
             data = datum)
summary(mod1)
anova(mod1)

# general linear model using ln-transformed norm_cite as the response
mod1.log <- lmer(norm_cit_log~relevel(OAlab, ref = "Closed Access")+auth_count+field+JCR_quart+AIS+APC+year+(1|field:jour), 
             data = datum)
summary(mod1.log)
anova(mod1.log)
## Note: mod1 and mod1 with the log-transformed norm_cit response both recover the fit warning:
## some predictor variables are on very different scales


# general linear model using norm_cite as the response. Including interactions for access by field and author count by APC
mod1.2 <- lmer(norm_cit~relevel(OAlab, ref = "Closed Access")*field+auth_count*scale(APC)+JCR_quart+AIS+year+(1|field:jour), 
               data = datum)
summary(mod1.2)
Anova(mod1.2)

# Poisson regression using norm_cit as response
  # basic model of all factors, with a random effect of journal nested in field
mod2.1 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count+field+JCR_quart+AIS+APC+year+
                  APC+(1|field:jour), 
              data = datum, family = poisson(link = "log"))
summary(mod2.1)
Anova(mod2.1)
#Warning messages:
#  1: Some predictor variables are on very different scales: consider rescaling 
#  2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                  Model failed to converge with max|grad| = 0.0449433 (tol = 0.002, component 1)
#                3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                  Model is nearly unidentifiable: very large eigenvalue
#                                - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
#                                - Rescale variables?

  # basic model of all factors (numerical factors scaled), with a random effect of journal nested in field
mod2.2 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+field+scale(auth_count)+JCR_quart+scale(AIS)+scale(APC)+year+(1|field:jour), 
                data = datum, family = poisson(link = "log"))
summary(mod2.2)
Anova(mod2.2)
#Warning messages:
#1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                    Model failed to converge with max|grad| = 0.00371365 (tol = 0.002, component 1)
#2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                    Model is nearly unidentifiable: very large eigenvalue
#- Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
#- Rescale variables?
  
  # basic model of all factors (numerical factors scaled) plus interaction of auth_count and APC, with a random effect of journal nested in field
mod2.3 <-  glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count*scale(APC)+field+JCR_quart+AIS+year+(1|field:jour), 
                 data = datum, family = poisson(link = "log"))
summary(mod2.3)
#Warning messages:
#  1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                    Model failed to converge with max|grad| = 1.75564 (tol = 0.002, component 1)
#  2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                   Model is nearly unidentifiable: very large eigenvalue
#  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
#  - Rescale variables?

  # basic model of most factors (numerical factors scaled) plus interaction of auth_count and APC,with random effects of field and journal nested in field 
mod2.4 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count*scale(APC)+JCR_quart+AIS+year+(1|field/jour), 
                data = datum, family = poisson(link = "log"))
summary(mod2.4)
  # basic model of most factors (numerical factors scaled) plus interaction of auth_count and APC,with random effects of field and journal nested in field 
    # increased the number of iterations using maxfun = 200,000 (started with 10, 50, 80,000; 100, 150,000)
mod2.5 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+scale(auth_count)*scale(APC)+JCR_quart+scale(AIS)+(1|field/jour), 
                data = datum, family = poisson(link = "log"), control = glmerControl( optCtrl = list(maxfun = 200000)))
summary(mod2.5)
  # model with only terms that we are interested in interactions between, with random effect of journal nested in field
mod2.6 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")*field+scale(auth_count)*scale(APC)+(1|field:jour), 
                data = datum, family = poisson(link = "log"))
summary(mod2.6)


  # Tried starting the model from where it failed to converge
ss <- getME(mod2.5,c("theta","fixef"))
model_2 <- update(mod2.5,start=ss)
  
  #Checking for singularity (want this value to not be close to zero)
  # https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
tt <- getME(mod2.5,"theta")
ll <- getME(mod2.5,"lower")
min(tt[ll==0])

  #
ranef(mod2.5)
fixef(mod2.5)

  # Plotting estimates
theme_set(theme_sjplot())
plot_model(mod2.5,sort.est = TRUE,show.values = TRUE,
           value.offset = .4,vline.color = "green")
plot_model(mod2.5, type = 're')

range(datum$AIS)


# export results from anova to .txt file
sink("clean_results_anova_WOS2.txt")
print(summary(model))
print(confint(model))
sink()  # returns output to the console

# Plotting ============
#  comments

#compute data summary to add to plot
data_summary <- function (datum) {
  m = mean(datum)
  ymin = m-sd(datum)
  ymax = m+sd(datum)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

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

vplot <- ggplot(datum,aes(x=OAlab,y=clean_citations,fill=OAlab)) +
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
