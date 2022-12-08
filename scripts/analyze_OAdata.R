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
library(ggpubr)
# install.packages('tidyverse') # run once
library(tidyverse)
# install.packages('broom') # run once
#library(broom)
#install.packages('knitr')
#library(knitr)
#install.packages('doBy')
#library(doBy)
#install.packages('reshape2')
#library(reshape2)
#install.packages('maptools')
#library(maptools)
#install.packages('nlme')
#library(nlme)
#install.packages('lme4')
library(lme4)
#install.packages('lmerTest')
library(lmerTest)
#install.packages('car')
library(car)
#install.packages("numDeriv")
#library(numDeriv)
#install.packages("performance")
#library(performance)
#install.packages("stargazer")
library(stargazer) # stargazer does not like tibbles
library(MASS)
#install.packages("emmeans")
library(emmeans)
#install.packages("lmtest")
library(lmtest)

#May need to set working directory first
#setwd("C:/Users/")
#setwd("C:/Users/")
datum <- read_csv("data/OA_data_fin.csv", col_names = TRUE)
#'jour' is journal
#'citations' is number of citations (y-variable)
#'OAdes' - 
#'OAlab' - type of access (closed, green, bronze, other gold)
#'year' is year of publication
#'Volume' is volume of journal publication is in
#'issue' is issue of journal publication is in
#'apc_cat' is Article processing charge (noAPC, lowAPC, highAPC)
#'AIS' is Article Influence Score - journal level metric
#'JCR_quart' is - from journal citation reports
#'field' is Field of biology which article is about
#'auth_count_scaled' is scaled number of authors


# check data
names(datum)
head(datum)
summary(datum)


# Summary Stats =================
# comment

# code for generating tables and output in Latex format
# stardat <- as.data.frame(datum)
# stargazer(stardat[c("citations", "year", "auth_count",
#                     "citations", "Volume", "Issue", "JCR_quart",
#                     "AIS")], 
#           type = "text",
#           title = "Summary Statistics for Full Open Access Dataset",
#           covariate.labels = c("Raw Citation Count", "Publication Year",
#                                "Author Count", "Normalized Citation Count",
#                                "Volume", "Issue", "Journal Citation Reports Quartiles",
#                                "Article Influence Score"),
#           out = "outputs/stats/OA_datum_sumstats.tex")


# print number of records per journal
# datum$jour = as.factor(datum$jour) # reset as factor
# sink("outputs/stats/recordnum_journal.txt")
# summary(datum$jour) # print number
# sink()


###Need to clean up the data a bit

summary(as.factor(datum$auth_count))
which(is.na(datum$auth_count)==TRUE)
#datum[103993,]
datum=datum[-c(103993),] #Remove sample without author counts



#print range of records by journal
range(summary(datum$jour))
#number of journals
length(unique(sort(datum$jour)))
#number of field
length(unique(sort(datum$field)))



            
            


# sum.stat <- datum %>% group_by(OAlab) %>% 
#   summarise(mean_cite = round(mean(citations),2),
#             sd_cite = round(sd(citations),2),
#             num_cite = length(citations))
# 
# yr.stat <- datum %>% group_by(year) %>% 
#   summarise(mean_cite = round(mean(norm_cit),2),
#             sd_cite = round(sd(norm_cit),2),
#             num_cite = length(norm_cit))
# 
# jcr.stat <- datum %>% group_by(JCR_quart) %>% 
#   summarise(mean_cite = round(mean(norm_cit),2),
#             sd_cite = round(sd(norm_cit),2),
#             num_cite = length(norm_cit))
# 
# gni.stat <- datum %>% group_by(gni_class) %>% 
#   summarise(mean_cite = round(mean(norm_cit),2),
#             sd_cite = round(sd(norm_cit),2),
#             num_cite = length(norm_cit))
# 
# # output summary stats
# sink("outputs/stats/OAdes_summarystats")
# kable(sum.stat)
# sink()
# 
# # Data Exploration  ================
# ls.str(datum)

# Plot histogram; estimate mean & variance for response variables
hist(datum$citations)#hist(datum$clean_citations)
mean(datum$citations) #mean(datum$clean_citations)
var(datum$citations) #var(datum$clean_citations)

#Normalized citation column no longer exists
#hist(datum$norm_cit) # for alternative normalization metric
# clean citations is not normally distributed; likely should not use 
#   linear model to fit these data. should use generalized linear model (Poisson).
#mean(datum$norm_cit)
#var(datum$norm_cit) # for alternative normalization metric
# the variance is higher than the mean, indicating an expectation of 
#   over-dispersion in the model.

# looking at AIS and author count distributions
hist(datum$AIS)
hist(datum$auth_count, breaks = 100)


# Statistical Tests ================

#need to id variables we want as a factor
datum$jour <- as.factor(datum$jour)
datum$field <- as.factor(datum$field)
#datum$jour_loc <- as.factor(datum$jour_loc)
datum$JCR_quart <- as.factor(datum$JCR_quart)
#datum$pub <- as.factor(datum$pub)
datum$year <- as.factor(datum$year)
#datum$gni_class <- as.factor(datum$gni_class)

# Rescaled numerical fields, into new columns
# datum <- datum %>% 
#   mutate(auth_count_scaled = scale(auth_count,center = TRUE, scale = TRUE),
#          AIS_scaled = scale(AIS,center = TRUE, scale = TRUE))
datum$auth_count_scaled <- scale(datum$auth_count)[,1]
summary(datum$auth_count)
#min=1
#median=5
#mean=6.713

sd(datum$auth_count,na.rm=TRUE)
#sd=6.290967
datum$AIS_scaled <- scale(datum$AIS)[,1] 

#datum$ln_AIS=log(datum$AIS)
#datum$ln_auth_count=log(datum$auth_count)

datum$OAlab[datum$OAlab=="Closed Accesr"]="Closed Access" #Fix a type
datum$OAlab[datum$OAlab=="Closed Access"]="Access Closed" #Rename category to permanently change reference
datum$OAlab <- as.factor(datum$OAlab) #convert Access to a factor

#set.seed(123)
#randomVector=seq(1:length(datum$OAlab))
#x=sample(randomVector,20000)
#datumSub=datum[x,]


# summary stats by OA label ---more detail than the mean calculations above, but should agree
levels(datum$OAlab)
datum$OAdes=factor(datum$OAdes)
levels(datum$OAdes)

datum$Gold=0
datum$Green=0
datum$Bronze=0
datum$Closed=0
for(i in 1:length(datum$citations)){
  if(datum$OAlab[i]=="Other Gold"){datum$Gold[i]=1}
  if(datum$OAlab[i]=="Green"){datum$Green[i]=1}
  if(datum$OAlab[i]=="Bronze"){datum$Bronze[i]=1}
  if(datum$OAlab[i]=="Access Closed"){datum$Closed[i]=1}
}

### Number of articles by access type
Table1=datum %>% group_by(field) %>%
  summarise(NumberofJournals=length(unique(jour)),
            NumberofArticles=length(citations),
            Bronze=sum(Bronze),
            ClosedAccess=sum(Closed),
            Green=sum(Green),
            OtherGold=sum(Gold))
Table1
sum(Table1$ClosedAccess)
sum(Table1$Bronze)+sum(Table1$Green)+sum(Table1$OtherGold)
sum(Table1$Bronze)
sum(Table1$Green)
sum(Table1$OtherGold)
write.csv(Table1,"Table1.csv")

jourCount=datum %>% group_by(jour) %>%
  summarise(Articles=length(citations))
mean(jourCount$Articles)
min(jourCount$Articles)
max(jourCount$Articles)

fieldCount=datum %>% group_by(field) %>%
  summarise(Articles=length(citations))
mean(fieldCount$Articles)
min(fieldCount$Articles)
max(fieldCount$Articles)







NaiveAnalysis=datum %>% group_by(OAlab) %>%
  summarise(Citations=mean(citations),
            sdCitations=sd(citations))
NaiveAnalysis
mod1.1=glm(citations~OAlab,data=datum,family=poisson)
mod1.2=glm.nb(citations~OAlab,data=datum)
anova(mod1.1,mod1.2,test="Chisq")
Anova(mod1.2)
summary(mod1.2)
NaiveResults=emmeans(mod1.2,spec=~OAlab,type="response")
predict(NaiveResults,type="response",interval="prediction")
NaiveResults
pairs(NaiveResults,adjust="bonferroni")


mod2.0.base=glm(citations~OAlab+auth_count_scaled+JCR_quart+AIS_scaled+year,
                data=datum,family=poisson)
###Only used to explore collinearity among variables
vif(mod2.0.base)


mod2.2 <- glmer(citations~OAlab+auth_count_scaled+JCR_quart+AIS_scaled+year+(1|field/jour), 
                data = datum, family = poisson)


mod2.2.1 <- glmer.nb(citations~OAlab+auth_count_scaled+JCR_quart+AIS_scaled+year+(1|field/jour), 
                data = datum)

anova(mod2.2,mod2.2.1,test='Chisq')
#Uses raw Citations; takes out GNI_class. 

#Compare both models
#anova(mod2.2.1,mod2.2)
#Significant, so negative binomial is better; poisson doesn't adequately fit the data

# ### Remove random effects to evaluate warnings
# mod2.2a <- glm(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count_scaled+JCR_quart+AIS_scaled+year+gni_class, 
#                 data = datum, family = poisson(link = "log"))
# ### Compare to negative binomial model
# mod2.2c <- glm.nb(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count_scaled+JCR_quart+AIS_scaled+year+gni_class, 
#                data = datum)
# ### Changed y variable to explore outliers
# mod2.2b <- glm(citations~relevel(OAlab, ref = "Closed Access")+auth_count_scaled+JCR_quart+AIS_scaled+year+gni_class, 
#                       data = datum, family = poisson(link = "log"))
# ### Compare to negative binomial model
#mod2.2d <- glm.nb(citations~relevel(OAlab, ref = "Closed Access")+auth_count_scaled+JCR_quart+AIS_scaled+year, 
#                   data = datum)
# ### Evaluations indicate that warnings can be ignored. 

### Test of interactions

mod2.2.1.int <- glmer.nb(citations~OAlab*auth_count_scaled+
                       JCR_quart+JCR_quart:OAlab+
                       AIS_scaled+AIS_scaled:OAlab+
                       year+year:OAlab+
                       (1|field/jour), 
                     data = datum)
anova(mod2.2.1.int,mod2.2.1)
###inclusion of interactions significantly improves the model. 

Anova(mod2.2.1.int)
#### Used to test for significance of individual interactions. 




#Below code was used to verify R output algorithms
#anova(mod2.2.1.int,test='Chisq')
#help("anova.merMod")
# mod2.2.1.int.b <- glmer.nb(citations~OAlab+auth_count_scaled+
#     JCR_quart+JCR_quart:OAlab+
#     AIS_scaled+AIS_scaled:OAlab+
#     year+year:OAlab+
#     (1|field/jour),
#     data = datum)
#anova(mod2.2.1.int,mod2.2.1.int.b,test='Chisq')


### model with all 2-way interactions including random effects
### Will not run
# mod2.2.1.int2 <- glmer.nb(citations~OAlab*auth_count_scaled+
#                            JCR_quart+JCR_quart:OAlab+
#                            AIS_scaled+AIS_scaled:OAlab+
#                            year+year:OAlab+
#                            (OAlab|field/jour), 
#                          data = datum)

### Full model with all 2-way interactions, but no random effects
### Runs
# mod2.2.1.int.glm <- glm.nb(citations~OAlab*auth_count_scaled+
#                            JCR_quart+JCR_quart:OAlab+
#                            AIS_scaled+AIS_scaled:OAlab+
#                            year+year:OAlab, 
#                          data = datum)

### Full model with full interactions
### Model will not run
# mod2.2.2.int <- glmer.nb(citations~OAlab*auth_count_scaled*
#                            JCR_quart*AIS_scaled*year+
#                            (1|field/jour), 
#                          data = datum)
# 

###Model with full interactions but without random effect
####Runs.
###Used mod2.2.2.int.glm in comparison with mod2.2.1.int.glm 
###to see importance of included full interactions on results
# mod2.2.2.int.glm <- glm.nb(citations~OAlab*auth_count_scaled*
#                            JCR_quart*AIS_scaled*year, 
#                          data = datum)
# 



###use emmeans (least-squares means) and Plots of results for easy of understanding
emmeans(mod2.2.1.int,specs=~OAlab,type="response")



####Graph for interaction between access and Authors



#emmeans(mod2.2.1.int,specs=~OAlab*auth_count_scaled)
#Interaction between author count and access
emtrends(mod2.2.1.int,pairwise~OAlab,var="auth_count_scaled")
#AccByAuth=emmeans(mod2.2.1.int,~OAlab*auth_count_scaled,type="response")
#AccByAuth
emmip(mod2.2.1.int,OAlab~auth_count_scaled,cov.reduce=range)
#m_auth_count=mean(datum$auth_count_scaled, na.rm=TRUE)
#sd_auth_count=sd(datum$auth_count_scaled,na.rm=TRUE)
x=c(1,2,4,8,16,32)
x_Scaled=(x-mean(datum$auth_count,na.rm=TRUE))/sd(datum$auth_count,na.rm=TRUE)
emmip(mod2.2.1.int,OAlab~auth_count_scaled,
      at=list(auth_count_scaled=x_Scaled),
      CIs=TRUE,level=0.95,position="jitter",type="response")
      
AuthInt=emmip(mod2.2.1.int,OAlab~auth_count_scaled,
      at=list(auth_count_scaled=x_Scaled),
      CIs=TRUE,level=0.95,type="response",
      plotit=FALSE)
Authorplot=ggplot(AuthInt,aes(color=OAlab,fill=OAlab,
                   x=auth_count_scaled,y=yvar,
                   ymin=LCL,ymax=UCL))+
      geom_line()+
      geom_pointrange(aes(shape=OAlab),position=position_dodge(width=0.1))+
      labs(y="Citations",x="Authors")+
      theme_classic()+
      scale_x_continuous(labels=x,
                      breaks=x_Scaled)#+labs(title="Relationship between authors, access type, and citations")+
      scale_color_manual(values=c("#000000","#E69F00","#009E73","#FFD700"))+
      scale_shape_manual(values=c(15:18))+
        theme(plot.title=element_text(hjust=0.5))+
      theme(legend.position=c(0.15,1))  
        


#####Explore non-linearity of auth_count
datum$AuthCat=NA
for(i in 1:length(datum$citations)){
  if(datum$auth_count[i]==1){datum$AuthCat[i]="1"}
  if(datum$auth_count[i]==2){datum$AuthCat[i]="2"}
  if(datum$auth_count[i]>=3 & datum$auth_count[i]<=4){datum$AuthCat[i]="4"}
  if(datum$auth_count[i]>=5 & datum$auth_count[i]<=8){datum$AuthCat[i]="8"}
  if(datum$auth_count[i]>=9 & datum$auth_count[i]<=16){datum$AuthCat[i]="16"}
  if(datum$auth_count[i]>=17 & datum$auth_count[i]<=32){datum$AuthCat[i]="32"}
  if(datum$auth_count[i]>=33 & datum$auth_count[i]<=64){datum$AuthCat[i]="64"}
  if(datum$auth_count[i]>=65 & datum$auth_count[i]<=128){datum$AuthCat[i]="128"}
  if(datum$auth_count[i]>=129 & datum$auth_count[i]<=256){datum$AuthCat[i]="256"}
  if(datum$auth_count[i]>=257){datum$AuthCat[i]="540"}
}

datum$AuthCat=as.factor(datum$AuthCat)
levels(datum$AuthCat)
summary(datum$AuthCat)
mod2.2.1.int.auth <- glmer.nb(citations~OAlab*AuthCat+
                           JCR_quart+JCR_quart:OAlab+
                           AIS_scaled+AIS_scaled:OAlab+
                           year+year:OAlab+
                           (1|field/jour), 
                         data = datum)
anova(mod2.2.1.int,mod2.2.1.int.auth,test="LRT")
Anova(mod2.2.1.int.auth)

levels(datum$AuthCat)
levels(datum$OAlab)

#emmip(mod2.2.1.int.auth,OAlab~as.numeric(AuthCat),type="response")

#m_auth_count=mean(datum$auth_count_scaled, na.rm=TRUE)
#sd_auth_count=sd(datum$auth_count_scaled,na.rm=TRUE)
#Authx=c(1,2,4,8,16,32,64,128,256,540)
Authx=c(1,2,4,8,16,32,64) # Truncate data due to small sample sizes at large values
AuthCatInt=emmip(mod2.2.1.int.auth,OAlab~AuthCat,
              CIs=TRUE,level=0.95,type="response",
              at=list(JCR_quart="2",year="2014",AIS_scaled=2),
              plotit=FALSE)
#AuthCatOrder=c("1","2","4","8","16","32","64","128","256","540")
AuthCatOrder=c("1","2","4","8","16","32","64")

Authorplot=ggplot(AuthCatInt,aes(color=OAlab,fill=OAlab,
                   x=as.numeric(factor(AuthCat,level=AuthCatOrder)),y=yvar,
                   ymin=LCL,ymax=UCL))+
  geom_line()+
  ylim(0,200)+
  geom_pointrange(aes(shape=OAlab),position=position_dodge(width=0.2))+
  labs(y="Citations",x="Authors")+
  theme_classic()+
  scale_x_continuous(labels=Authx,breaks=1:7)+
  labs(color="Access Type",fill="Access Type")+#title="Results of Analysis of Full Dataset")+
  scale_color_manual(values=c("#000000","#b08d57","#009E73","#E69F00"),
                     name="Access Type")+
  scale_shape_manual(values=c(15:18),name="Access Type")+
  theme(plot.title=element_text(hjust=1.5))+
  theme(legend.position=c(0.25,0.65))
Authorplot


#emmeans(mod2.2.1.int.auth,pairwise~OAlab|AuthCat)
#emmeans(mod2.2.1.int.auth,pairwise~OAlab|AuthCat,
#        at=list(JCR_quart="2",year="2014",AIS_scaled=2),type="response",
#        reverse=TRUE,infer=c(TRUE,TRUE))

compAuthCat=emmeans(mod2.2.1.int.auth,"OAlab",by="AuthCat",
        at=list(JCR_quart="2",year="2014",AIS_scaled=2))
pairs(compAuthCat,reverse=TRUE,infer=c(TRUE,TRUE),type="response",adjust="bonferroni")





####Interaction between JCR quartile and access

#resultsJCRInt=emmeans(mod2.2.1.int.auth,~OAlab*JCR_quart,type="response")
#resultsJCRInt
#plot(resultsJCRInt)
JCRInt=emmip(mod2.2.1.int.auth,OAlab~JCR_quart,type="response",
     CIs=TRUE,level=0.95,plotit=FALSE,
     at=list(AuthCat="1",year="2014",AIS_scaled=2))
JCRPlot=ggplot(JCRInt,aes(color=OAlab,fill=OAlab,
                  x=as.numeric(JCR_quart),y=yvar,ymin=LCL,
                  ymax=UCL))+
  geom_line(show.legend=FALSE)+
  geom_pointrange(aes(shape=OAlab),position=position_dodge(width=0.2),show.legend = FALSE)+
  labs(y="Citations",x="JCR Quartile")+
  theme_classic()+
  #labs(color="Access Type",fill="Access Type")+#title="Relationship between JCR quartile, access type, and citations"
  scale_color_manual(values=c("#000000","#b08d57","#009E73","#E69F00"),
                     name="Access Type")+
  scale_shape_manual(values=c(15:18),
                     name="Access Type")#+theme(plot.title=element_text(hjust=0.5))
  #+theme(legend.position=c(0.8,0.8))

JCRPlot

compJCR=emmeans(mod2.2.1.int.auth,"OAlab",by="JCR_quart",
                at=list(AuthCat="1",year="2014",AIS_scaled=2))
pairs(compJCR,reverse=TRUE,infer=c(TRUE,TRUE),type="response",adjust="bonferroni")


#Interaction between AIS_scaled and access
hist(datum$AIS_scaled)
emtrends(mod2.2.1.int,pairwise~OAlab,var="AIS_scaled")
emmip(mod2.2.1.int,OAlab~AIS_scaled,cov.reduce=range,at=list(auth_count_scaled=c(0)),CIs=TRUE,level=0.95,
      position="jitter")
AIS_range=c(0,0.5,1.0,1.5,2.0,2.5)
#AISx=AIS_range*sd(datum$AIS)+mean(datum$AIS)
AISInt=emmip(mod2.2.1.int.auth,OAlab~AIS_scaled,
              at=list(AIS_scaled=AIS_range, AuthCat="1",year="2014",JCR_quart="2"),
              CIs=TRUE,level=0.95,type="response",
              plotit=FALSE)
AISPlot=ggplot(AISInt,aes(color=OAlab,fill=OAlab,
                   x=AIS_scaled,y=yvar,
                   ymin=LCL,ymax=UCL))+
  geom_pointrange(aes(shape=OAlab),position=position_dodge(width=0.1),
                  show.legend = FALSE)+
  geom_line(show.legend = FALSE)+
  labs(y="Citations",x="AIS (Scaled)")+
  theme_classic()+#scale_x_continuous(labels=AISx,
                     #breaks=AIS_range)+
  labs(color="Access Type",fill="Access Type")+#title="Relationship between AIS, access type, and citations"
  scale_color_manual(values=c("#000000","#b08d57","#009E73","#E69F00"),
                     name="Access Type")+
  scale_shape_manual(values=c(15:18),
                     name="Access Type")+
  theme(plot.title=element_text(hjust=0.5))#+theme(legend.position=c(0.15,0.8))

compAIS=emmeans(mod2.2.1.int.auth,"OAlab",by="AIS_scaled",
                at=list(AuthCat="1",year="2014",JCR_quart="2",AIS_scaled=AIS_range))
pairs(compAIS,reverse=TRUE,infer=c(TRUE,TRUE),type="response",adjust="bonferroni")







#Explore effect of removing all articles from 'Cell' from analysis
#Cell is only journal with AIS_scaled > 2.5 (has AIS_scaled of 6.89)
### Only changes relationships minimally, mostly in citation counts of Bronze
### Therefore not used
# datumAIS=datum[which(datum$AIS_scaled<2.5),]
# datumAIS$AISCat=as.factor(datumAIS$AIS)
# ###Only journal with AIS > 3 is 'Cell'
# 
# 
# mod2.2.1.int.AIS <- glmer.nb(citations~OAlab*auth_count_scaled+
#                            JCR_quart+JCR_quart:OAlab+
#                            AIS_scaled+AIS_scaled:OAlab+
#                            year+year:OAlab+
#                            (1|field/jour), 
#                          data = datumAIS)
# 
# AISModInt=emmip(mod2.2.1.int.AIS,OAlab~AIS_scaled,
#               at=list(AIS_scaled=AIS_range,AuthCat="1",year="2014",JCR_quart="2"),
#              CIs=TRUE,level=0.95,type="response",
#              plotit=FALSE)
# ggplot(AISModInt,aes(color=OAlab,fill=OAlab,
#                   x=AIS_scaled,y=yvar,
#                   ymin=LCL,ymax=UCL))+
#   geom_pointrange(aes(shape=OAlab),position=position_dodge(width=0.1))+
#   geom_line()+
#   labs(y="Citations",x="AIS (Scaled)")+
#   theme_classic()+
#   labs(title="Relationship between AIS, access type, and citations")+
#   scale_color_manual(values=c("#000000","#E69F00","#009E73","#FFD700"))+
#   scale_shape_manual(values=c(15:18))+
#   theme(plot.title=element_text(hjust=0.5))+
#   theme(legend.position=c(0.15,0.8))





#Interaction between year and access
#resultsYearInt=emmeans(mod2.2.1.int,~OAlab*year,type="response")
#resultsYearInt
#plot(resultsYearInt)
Yearx=c(2013,2014,2015,2016,2017,2018)
YearInt=emmip(mod2.2.1.int.auth,OAlab~year,
              type="response",CIs=TRUE,level=0.95,
              plotit=FALSE,
              at=list(AuthCat="1",JCR_quart="2",AIS_scaled=2))
YearPlot=ggplot(YearInt,aes(color=OAlab,fill=OAlab,
                  x=as.numeric(year),y=yvar,
                  ymin=LCL,ymax=UCL))+
  geom_pointrange(aes(shape=OAlab),position=position_dodge(width=0.1),
                  show.legend = FALSE)+
  geom_line(show.legend = FALSE)+
  labs(y="Citations",x="Year")+
  theme_classic()+
  labs(color="Access Type",fill="Access Type")+#title="Relationship between year, access type, and citations"
  scale_color_manual(values=c("#000000","#b08d57","#009E73","#E69F00"),
                     name="Access Type")+
  scale_shape_manual(values=c(15:18),
                     name="Access Type")+
  theme(plot.title=element_text(hjust=0.5))+
  #theme(legend.position=c(0.8,0.8))+
  scale_x_continuous(labels=Yearx,breaks=1:6)
 

compYear=emmeans(mod2.2.1.int.auth,"OAlab",by="year",
                at=list(AuthCat="1",JCR_quart="2",AIS_scaled=2))
pairs(compYear,reverse=TRUE,infer=c(TRUE,TRUE),type="response",adjust="bonferroni")




ggarrange(Authorplot,JCRPlot,AISPlot,YearPlot,
          labels=c("A","B","C","D"),
          ncol=2,nrow=2, label.x=0.2, label.y=0.95)

###

# Putting model coefficients into a dataframe
#Mod2.2 <- as.data.frame(coef(summary(mod2.2)))
# Adding column with interpretable betas
#Mod2.2 <- Mod2.2 %>% mutate(Exp_Estim =exp(Estimate))

# Previous message (before 8/30/21)
#Warning messages:
  #1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  #                    Model failed to converge with max|grad| = 0.00371365 (tol = 0.002, component 1)
  #2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  #                                    Model is nearly unidentifiable: very large eigenvalue
  #- Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
  #- Rescale variables?
# Warning message as of 8/30/21
#Warning message:
 #  In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
 #                 Model is nearly unidentifiable: very large eigenvalue
 #               - Rescale variables?
# export results from anova to .txt file

#sink("clean_results_anova_WOS2.txt")
#print(summary(model))
#print(confint(model))
#sink()  # returns output to the console


#### ARCHIVED MODEL BUILDING ####
#### general linear model using norm_cite as the response
##mod1 <- lmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count+field+JCR_quart+AIS+APC+year+(1|field:jour), 
##             data = datum)
##summary(mod1)
#anova(mod1)
#
## general linear model using ln-transformed norm_cite as the response
#mod1.log <- lmer(norm_cit_log~relevel(OAlab, ref = "Closed Access")+auth_count+field+JCR_quart+AIS+scale(APC)+year+pub+(1|field:jour), 
#                 data = datum)
#summary(mod1.log)
#anova(mod1.log)
##Anova(mod1.log)
### Note: mod1 and mod1 with the log-transformed norm_cit response both recover the fit warning:
### some predictor variables are on very different scales
#
#
## general linear model using norm_cite as the response. Including interactions for access by field and author count by APC
#mod1.2 <- lmer(norm_cit~relevel(OAlab, ref = "Closed Access")*field+auth_count*scale(APC)+JCR_quart+AIS+year+(1|field:jour), 
#               data = datum)
#summary(mod1.2)
#Anova(mod1.2, type = 3)
#
## general linear model using norm_cit_log as the response. Including interactions for access by field and author count by APC
#mod1.2.log <- lmer(norm_cit_log~relevel(OAlab, ref = "Closed Access")*field+auth_count*scale(APC)+JCR_quart+AIS+year+(1|field:jour), 
#                   data = datum)
#summary(mod1.2.log)
#Anova(mod1.2.log, type = 3)
## Pub is not significant-- removed from model.
#
## Comparing norm_cit_log models with and without interactions
#anova(mod1.log,mod1.2.log)
#
#mod2.3 <-  glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count*scale(APC)+field+JCR_quart+AIS+year+(1|field:jour), 
#                 data = datum, family = poisson(link = "log"))
#summary(mod2.3)
##Warning messages:
##  1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
##                    Model failed to converge with max|grad| = 1.75564 (tol = 0.002, component 1)
##  2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
##                   Model is nearly unidentifiable: very large eigenvalue
##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
##  - Rescale variables?
#
## basic model of most factors (numerical factors scaled) plus interaction of auth_count and APC,with random effects of field and journal nested in field 
#mod2.4 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count*scale(APC)+JCR_quart+AIS+year+(1|field/jour), 
#                data = datum, family = poisson(link = "log"))
#summary(mod2.4)
## basic model of most factors (numerical factors scaled) plus interaction of auth_count and APC,with random effects of field and journal nested in field 
## increased the number of iterations using maxfun = 200,000 (started with 10, 50, 80,000; 100, 150,000)
#mod2.5 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+scale(auth_count)*scale(APC)+JCR_quart+scale(AIS)+(1|field/jour), 
#                data = datum, family = poisson(link = "log"), control = glmerControl( optCtrl = list(maxfun = 200000)))
#summary(mod2.5)
## model with only terms that we are interested in interactions between, with random effect of journal nested in field
#mod2.6 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")*field+scale(auth_count)*scale(APC)+(1|field:jour), 
#                data = datum, family = poisson(link = "log"))
#summary(mod2.6)
#  # basic model of all factors (numerical factors scaled) plus interaction of auth_count and APC, with a random effect of journal nested in field
## interaction with gni_class & access level
#mod2.7 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")*gni_class+auth_count_scaled+JCR_quart+AIS_scaled+year+(1|field/jour), 
#                data = datum, family = poisson(link = "log"))

#Mod2.7 <- as.data.frame(coef(summary(mod2.3)))
#Mod2.7 <- Mod2.3 %>% mutate(Exp_Estim =exp(Estimate))
#
## Tried starting the model from where it failed to converge
#ss <- getME(mod2.5,c("theta","fixef"))
#model_2 <- update(mod2.5,start=ss)
#
##Checking for singularity (want this value to not be close to zero)
## https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
#tt <- getME(mod2.5,"theta")
#ll <- getME(mod2.5,"lower")
#min(tt[ll==0])
#
##
#ranef(mod2.5)
#fixef(mod2.5)
#
## Plotting estimates
#theme_set(theme_sjplot())
#plot_model(mod2.5,sort.est = TRUE,show.values = TRUE,
#           value.offset = .4,vline.color = "green")
#plot_model(mod2.5, type = 're')
#
#range(datum$AIS)

#datum$norm_cit_log <- log(datum$norm_cit + 1) 

#hist(datum$norm_cit_log)
#mean(datum$norm_cit_log)
#var(datum$norm_cit_log)

#### mean calculations
# grand mean of norm_cit
#mean(datum$norm_cit)
# means for norm_cit across levels of categorical variables
#myr <- with(datum, tapply(norm_cit, year, mean))
#moa <- with(datum, tapply(norm_cit, OAlab, mean))
#mjcr <- with(datum, tapply(norm_cit, JCR_quart, mean))
#mgni <- with(datum, tapply(norm_cit, gni_class, mean))
# differences between levels for each categ. var.
#successive when level > 2
#dyr <- diff(myr)
#doa <- diff(moa)
#djcr <- diff(mjcr)
#dgni <- diff(mgni)
#means of each combination of all categorical variables
#m2var <- with(datum,tapply(norm_cit,list(year,gni_class,OAlab,JCR_quart),mean))
#mean of year when all other categories are reference values
#mean(m2var[,"High", "Closed Access", 1])

##### coding contrasts
# Recode year field (deviation- compares level to grand mean)
#contrasts(datum$year) = contr.sum(6)
# contrasts(datum$field) = contr.sum(12) no longer relevant with field as random effect

##### Models that didn't converge :( #####
# Poisson regression using norm_cit as response
# basic model of all factors, with a random effect of journal nested in field
#mod2.1 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count+JCR_quart+AIS+year+gni_class+
#                  (1|field/jour), 
#              data = datum, family = poisson(link = "log"))
#summary(mod2.1)
#Anova(mod2.1)
# Previous Warning (before 8/30/21)
#Warning messages:
#  1: Some predictor variables are on very different scales: consider rescaling 
#  2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                  Model failed to converge with max|grad| = 0.0449433 (tol = 0.002, component 1)
#                3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                  Model is nearly unidentifiable: very large eigenvalue
#                                - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
#                                - Rescale variables?
# Warning message as of 8/30/21
#Warning messages:
#  1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                    Model failed to converge with max|grad| = 0.0166963 (tol = 0.002, component 1)
#                  2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                    Model is nearly unidentifiable: very large eigenvalue
#                                  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
#                                  - Rescale variables?
# basic model of all factors (numerical factors scaled), with a random effect of journal nested in field

#mod2.2 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+auth_count_scaled+JCR_quart+AIS_scaled+gni_class+year+(1|field/jour), 
#                data = datum, family = poisson(link = "log"))
