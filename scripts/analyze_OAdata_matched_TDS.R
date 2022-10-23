##### Script for analyzing the matched dataset of citations
##### Data are matched in that each issue has articles of multiple Access types


library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(MASS)
library(car)


###May need to change working directory 
setwd("C:/Users/TDS0009/Documents/GitHub/OA_2021_AU")


### Get data
datum=read.csv("C:/Users/TDS0009/Documents/GitHub/OA_2021_AU/data/matched_OA_data_fin.csv")
head(datum)


#####convert categorical variables to factors and explore levels

#Access
datum$OAlab[datum$OAlab=="Closed Accesr"]="Closed Access"
datum$OAlab[datum$OAlab=="Closed Access"]="Access Closed"
datum$OAlab <- as.factor(datum$OAlab)
levels(datum$OAlab)
summary(datum$OAlab)

#year
datum$year=as.factor(datum$year)
levels(datum$year)
summary(datum$year)

#JCR Quartile
datum$JCR_quart=as.factor(datum$JCR_quart)
levels(datum$JCR_quart)
summary(datum$JCR_quart)

#journal
datum$jour=as.factor(datum$jour)
levels(datum$jour)
summary(datum$jour)
#print range of records by journal
range(summary(datum$jour))
#number of journals
length(unique(sort(datum$jour)))


#field
datum$field=as.factor(datum$field)
levels(datum$field)
summary(datum$field)
#number of fields
length(unique(sort(datum$field)))

#Volumes
datum$Volume=as.factor(datum$Volume)
levels(datum$Volume)
summary(datum$Volume)


#issues
datum$Issue=as.factor(datum$Issue)
levels(datum$Issue)
summary(datum$Issue)
#Remove those samples that do not have an issue
Missing.Issue=which(is.na(datum$Issue)==TRUE)
#datum[103993,]
datum=datum[-Missing.Issue,]
which(is.na(datum$Issue)==TRUE)




### Examine continuous variables

#author count
summary(as.factor(datum$auth_count))
hist(datum$auth_count)

#citations
# Plot histogram; estimate mean & variance for response variables
hist(datum$citations)#hist(datum$clean_citations)
mean(datum$citations) #mean(datum$clean_citations)
var(datum$citations) #var(datum$clean_citations)
summary(as.factor(datum$citations))

#AIS
summary(datum$AIS)
hist(datum$AIS)



##### Scaling continuous variables

#Author count
datum$auth_count_scaled <- scale(datum$auth_count)[,1]
summary(datum$auth_count_scaled)

#AIS
datum$AIS_scaled <- scale(datum$AIS)[,1] 
summary(datum$AIS_scaled)
hist(datum$AIS_scaled)



##### Examine citations by variables

#Access
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

#Count of samples by field
fieldCount=datum %>% group_by(field) %>%
  summarise(Articles=length(citations))
mean(fieldCount$Articles)
min(fieldCount$Articles)
max(fieldCount$Articles)

#Counts of samples by jour
jourCount=datum %>% group_by(jour) %>%
  summarise(Articles=length(citations))
mean(jourCount$Articles)
min(jourCount$Articles)
max(jourCount$Articles)

#Total number of samples
length(datum$citations)


##### Naive analysis of results
NaiveAnalysis=datum %>% group_by(OAlab) %>%
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

exp(0.19856)

##### Analysis of results


