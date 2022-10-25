##### Script for analyzing the matched dataset of citations
##### Data are matched in that each issue has articles of multiple Access types


library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(MASS)
library(car)


###May need to change working directory 
#setwd("C:/Users/tds0009/Documents/GitHub/OA_2021_AU")
#setwd("C:/Users/tds0009/OneDrive - Auburn University/Student Research/Amanda Clark")



### Get data
datum <- read_csv("data/OA_data_fin.csv", col_names = TRUE)
#datum=read.csv("C:/Users/TDS0009/Documents/GitHub/OA_2021_AU/data/matched_OA_data_fin.csv")
head(datum)
#datum$OAlab=as.factor(datum$OAlab)
#levels(datum$OAlab)

#Remove those samples that do not have an issue
Missing.Issue=which(is.na(datum$Issue)==TRUE)
Missing.Issue
#datum[4206,]
datum=datum[-Missing.Issue,]
which(is.na(datum$Issue)==TRUE)

datum=filter(datum,OAlab=="Closed Access" | OAlab=="Other Gold")
datumMatch=datum %>%
  group_by(jour) %>%
  group_by(Volume,.add=TRUE) %>%
  group_by(Issue,.add=TRUE) %>%
  filter(any(OAlab=="Other Gold")&any(OAlab=="Closed Access"))
datumMatch

CountTable=datumMatch %>%
  group_by(jour) %>%
  group_by(Volume,.add=TRUE) %>%
  group_by(Issue,.add=TRUE) %>%
  group_by(OAlab,.add=TRUE) %>%
  count()

CountTable
#write.csv(CountTable,"MatchedSamples.csv")

#####convert categorical variables to factors and explore levels

#Access
#datum$OAlab[datum$OAlab=="Closed Accesr"]="Closed Access"
datumMatch$OAlab[datumMatch$OAlab=="Closed Access"]="Access Closed"
datumMatch$OAlab <- as.factor(datumMatch$OAlab)
levels(datumMatch$OAlab)
summary(datumMatch$OAlab)

#year
datumMatch$year=as.factor(datumMatch$year)
levels(datumMatch$year)
summary(datumMatch$year)

#JCR Quartile
datumMatch$JCR_quart=as.factor(datumMatch$JCR_quart)
levels(datumMatch$JCR_quart)
summary(datumMatch$JCR_quart)

#journal
datumMatch$jour=as.factor(datumMatch$jour)
levels(datumMatch$jour)
summary(datumMatch$jour)
#print range of records by journal
range(summary(datumMatch$jour))
#number of journals
length(unique(sort(datumMatch$jour)))


#field
datumMatch$field=as.factor(datumMatch$field)
levels(datumMatch$field)
summary(datumMatch$field)
#number of fields
length(unique(sort(datumMatch$field)))

#Volumes
datumMatch$Volume=as.factor(datumMatch$Volume)
levels(datumMatch$Volume)
summary(datumMatch$Volume)


#issues
datumMatch$Issue=as.factor(datumMatch$Issue)
levels(datumMatch$Issue)
summary(datumMatch$Issue)





### Examine continuous variables

#author count
summary(as.factor(datumMatch$auth_count))
hist(datumMatch$auth_count)

#citations
# Plot histogram; estimate mean & variance for response variables
hist(datumMatch$citations)#hist(datum$clean_citations)
mean(datumMatch$citations) #mean(datum$clean_citations)
var(datumMatch$citations) #var(datum$clean_citations)
summary(as.factor(datumMatch$citations))

#AIS
summary(datumMatch$AIS)
hist(datumMatch$AIS)



##### Scaling continuous variables

#Author count
datumMatch$auth_count_scaled <- scale(datumMatch$auth_count)[,1]
summary(datumMatch$auth_count_scaled)

#AIS
datumMatch$AIS_scaled <- scale(datumMatch$AIS)[,1] 
summary(datumMatch$AIS_scaled)
hist(datumMatch$AIS_scaled)



##### Examine citations by variables

#Access
datumMatch$Gold=0
datumMatch$Closed=0
for(i in 1:length(datumMatch$citations)){
  if(datumMatch$OAlab[i]=="Other Gold"){datumMatch$Gold[i]=1}
  if(datumMatch$OAlab[i]=="Access Closed"){datumMatch$Closed[i]=1}
}


Table1=datumMatch %>% group_by(field) %>%
  summarise(NumberofJournals=length(unique(jour)),
            NumberofArticles=length(citations),
            ClosedAccess=sum(Closed),
            OtherGold=sum(Gold))
Table1
sum(Table1$ClosedAccess)
sum(Table1$OtherGold)

#Count of samples by field
fieldCount=datumMatch %>% group_by(field) %>%
  summarise(Articles=length(citations))
mean(fieldCount$Articles)
min(fieldCount$Articles)
max(fieldCount$Articles)

#Counts of samples by jour
jourCount=datumMatch %>% group_by(jour) %>%
  summarise(Articles=length(citations))
mean(jourCount$Articles)
min(jourCount$Articles)
max(jourCount$Articles)

#Total number of samples
length(datumMatch$citations)


##### Naive analysis of results
NaiveAnalysis=datumMatch %>% group_by(OAlab) %>%
  summarise(Citations=mean(citations),
            sdCitations=sd(citations))
NaiveAnalysis
mod1.1=glm(citations~OAlab,data=datum,family=poisson)
mod1.2=glm.nb(citations~OAlab,data=datum)
anova(mod1.1,mod1.2,test='Chisq')
summary(mod1.2)
NaiveResults=emmeans(mod1.2,spec=~OAlab,type="response")
pairs(NaiveResults,adjust="bonferroni")


##### Explore collinearity among variables

mod2.0.base=glm(citations~OAlab+auth_count_scaled+JCR_quart+AIS_scaled+year,
                data=datum,family=poisson)
###Only used to explore collinearity among variables
vif(mod2.0.base)



#####Full analysis with matched pairs
results1 <- glmer(citations~OAlab+auth_count_scaled+JCR_quart+AIS_scaled+year+(1|field/jour/Volume/Issue), 
                data = datum, family = poisson)


results2 <- glmer.nb(citations~OAlab+auth_count_scaled+JCR_quart+AIS_scaled+year+(1|field/jour/Volume/Issue), 
                     data = datum)

anova(results2,results1,test='Chisq')
summary(results2)
exp(0.18870)



#####Test of including all interactions

results3 <- glmer.nb(citations~OAlab*auth_count_scaled+
                           JCR_quart+JCR_quart:OAlab+
                           AIS_scaled+AIS_scaled:OAlab+
                           year+year:OAlab+
                           (1|field/jour/Volume/Issue), 
                         data = datum)
anova(results3,results2)
###inclusion of interactions DOES NOT significantly improves the model. 

Anova(results3)
summary(results2)

exp(0.18870)

##### Analysis of results


