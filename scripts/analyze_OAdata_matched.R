##### Script for analyzing the matched dataset of citations
##### Data are matched in that each issue has articles of multiple Access types


library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(MASS)
library(car)


###May need to change working directory 
#setwd("C:/Users/")
#setwd("C:/Users/")



### Get data
datum <- read.csv("data/OA_data_fin.csv")

#datum <- read_csv("data/OA_data_fin.csv", col_names = TRUE)
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
                data=datumMatch,family=poisson)
###Only used to explore collinearity among variables
vif(mod2.0.base)



#####Full analysis with matched pairs
results1 <- glmer(citations~OAlab+auth_count_scaled+JCR_quart+AIS_scaled+year+(1|field/jour/Volume/Issue), 
                data = datumMatch, family = poisson)


results2 <- glmer.nb(citations~OAlab+auth_count_scaled+JCR_quart+AIS_scaled+year+(1|field/jour/Volume/Issue), 
                     data = datumMatch)

anova(results1,results2,test='Chisq')
summary(results2)
exp(0.18870)



#####Test of including all interactions

results3 <- glmer.nb(citations~OAlab*auth_count_scaled+
                           JCR_quart+JCR_quart:OAlab+
                           AIS_scaled+AIS_scaled:OAlab+
                           year+year:OAlab+
                           (1|field/jour/Volume/Issue), 
                         data = datumMatch)
anova(results3,results2)
###inclusion of interactions DOES NOT significantly improves the model. 

Anova(results3)
summary(results3)

exp(0.18870)

##### Analysis of results
plot(residuals(results3))



#### Test non-linearity of author count


datumMatch$AuthCat=NA
for(i in 1:length(datumMatch$citations)){
  if(datumMatch$auth_count[i]==1){datumMatch$AuthCat[i]="1"}
  if(datumMatch$auth_count[i]==2){datumMatch$AuthCat[i]="2"}
  if(datumMatch$auth_count[i]>=3 & datumMatch$auth_count[i]<=4){datumMatch$AuthCat[i]="4"}
  if(datumMatch$auth_count[i]>=5 & datumMatch$auth_count[i]<=8){datumMatch$AuthCat[i]="8"}
  if(datumMatch$auth_count[i]>=9 & datumMatch$auth_count[i]<=16){datumMatch$AuthCat[i]="16"}
  if(datumMatch$auth_count[i]>=17 & datumMatch$auth_count[i]<=32){datumMatch$AuthCat[i]="32"}
  if(datumMatch$auth_count[i]>=33 & datumMatch$auth_count[i]<=64){datumMatch$AuthCat[i]="64"}
  if(datumMatch$auth_count[i]>=65 & datumMatch$auth_count[i]<=128){datumMatch$AuthCat[i]="128"}
  if(datumMatch$auth_count[i]>=129 & datumMatch$auth_count[i]<=256){datumMatch$AuthCat[i]="256"}
  if(datumMatch$auth_count[i]>=257){datumMatch$AuthCat[i]="540"}
}

datumMatch$AuthCat=as.factor(datumMatch$AuthCat)
levels(datumMatch$AuthCat)

summary(datumMatch$AuthCat)
results4 <- glmer.nb(citations~OAlab*AuthCat+
                       JCR_quart+JCR_quart:OAlab+
                       AIS_scaled+AIS_scaled:OAlab+
                       year+year:OAlab+
                       (1|field/jour/Volume/Issue), 
                     data = datumMatch)
anova(results4,results3,test="LRT")
Anova(results4)

#### Categorical is not a significant improvement in fit



####Plots


####Author Count

Authx=c(1,2,4,8,16,32,64)
x_Scaled=(Authx-mean(datumMatch$auth_count,na.rm=TRUE))/sd(datum$auth_count,na.rm=TRUE)
#emmip(mod2.2.1.int,OAlab~auth_count_scaled,
#      at=list(auth_count_scaled=x_Scaled),
#      CIs=TRUE,level=0.95,position="jitter",type="response")
#
AuthInt=emmip(results3,OAlab~auth_count_scaled,
              at=list(JCR_quart="2",year="2014",AIS_scaled=2,
                      auth_count_scaled=x_Scaled),
              CIs=TRUE,level=0.95,type="response",
              plotit=FALSE)
Authplot=ggplot(AuthInt,aes(color=OAlab,fill=OAlab,
                   x=auth_count_scaled,y=yvar,
                   ymin=LCL,ymax=UCL))+
  geom_line()+
  geom_pointrange(aes(shape=OAlab),position=position_dodge(width=0.1))+
  labs(y="Citations",x="Authors")+
  theme_classic()+
  scale_x_continuous(labels=Authx,
                     breaks=x_Scaled)+
  labs(color="Access Type",fill="Access Type")+
  #labs(title="Relationship between authors, access type, and citations")+
  scale_color_manual(values=c("#000000","#E69F00","#009E73","#FFD700")
                     ,name="Access Type")+
  scale_shape_manual(values=c(15:18),name="Access Type")+
  theme(plot.title=element_text(hjust=0.5))+
  theme(legend.position=c(0.25,0.65))  
Authplot


compAuth=emmeans(results3,"OAlab",by="auth_count_scaled",
                    at=list(JCR_quart="2",year="2014",AIS_scaled=2,
                            auth_count_scaled=x_Scaled))
pairs(compAuth,reverse=TRUE,infer=c(TRUE,TRUE),type="response",adjust="bonferroni")



####Interaction between JCR quartile and access

JCRInt=emmip(results3,OAlab~JCR_quart,type="response",
             CIs=TRUE,level=0.95,plotit=FALSE,
             at=list(auth_count_scaled=-0.9496011,year="2014",AIS_scaled=2))
JCRPlot=ggplot(JCRInt,aes(color=OAlab,fill=OAlab,
                  x=as.numeric(JCR_quart),y=yvar,ymin=LCL,
                  ymax=UCL))+
  geom_line(show.legend=FALSE)+
  geom_pointrange(aes(shape=OAlab),position=position_dodge(width=0.2),
                  show.legend=FALSE)+
  labs(y="Citations",x="JCR Quartile")+
  theme_classic()+
  #labs(title="Relationship between JCR quartile, access type, and citations")+
  scale_color_manual(values=c("#000000","#E69F00","#009E73","#FFD700"))+
  scale_shape_manual(values=c(15:18))+
  theme(plot.title=element_text(hjust=0.5))+
  theme(legend.position=c(0.8,0.8))
JCRPlot

compJCR=emmeans(results3,"OAlab",by="JCR_quart",
                at=list(auth_count_scaled=-0.9496011,year="2014",AIS_scaled=2))
pairs(compJCR,reverse=TRUE,infer=c(TRUE,TRUE),type="response",adjust="bonferroni")



#Interaction between AIS_scaled and access
#hist(datum$AIS_scaled)
#emtrends(mod2.2.1.int,pairwise~OAlab,var="AIS_scaled")
#emmip(mod2.2.1.int,OAlab~AIS_scaled,cov.reduce=range,at=list(auth_count_scaled=c(0)),CIs=TRUE,level=0.95,
#      position="jitter")
AIS_range=c(0,0.5,1.0,1.5,2.0,2.5)
AISInt=emmip(results3,OAlab~AIS_scaled,
             at=list(AIS_scaled=AIS_range, year="2014",JCR_quart="2",
                     auth_count_scaled=-0.9496011),
             CIs=TRUE,level=0.95,type="response",
             plotit=FALSE)
AISPlot=ggplot(AISInt,aes(color=OAlab,fill=OAlab,
                  x=AIS_scaled,y=yvar,
                  ymin=LCL,ymax=UCL))+
  geom_pointrange(aes(shape=OAlab),position=position_dodge(width=0.1),
                  show.legend=FALSE)+
  geom_line(show.legend=FALSE)+
  labs(y="Citations",x="AIS (Scaled)")+
  theme_classic()+
  #labs(title="Relationship between AIS, access type, and citations")+
  scale_color_manual(values=c("#000000","#E69F00"))+
  scale_shape_manual(values=c(15:18))+
  theme(plot.title=element_text(hjust=0.5))+
  theme(legend.position=c(0.15,0.8))
AISPlot

compAIS=emmeans(results3,"OAlab",by="AIS_scaled",
                at=list(year="2014",JCR_quart="2",AIS_scaled=AIS_range,
                        auth_count_scaled=-0.9496011))
pairs(compAIS,reverse=TRUE,infer=c(TRUE,TRUE),type="response",adjust="bonferroni")



ggarrange(Authplot,JCRPlot,AISPlot,
          labels=c("A","B","C"),
          ncol=2,nrow=2, label.x=0.2, label.y=0.95)
