###############################################
############# Analysis of APC charges##################
###########################################################


### import libraries
library(MASS)
library(lme4)
library(lmerTest)
library(tidyverse)
library(emmeans)
library(car)


### import Data

#setwd("C:/Users/")
datumAPC=read.csv("data/OA_data_othergoldONLY.csv")
#view(datumAPC)



#need to id variables we want as a factor
datumAPC$jour <- as.factor(datumAPC$jour)
datumAPC$field <- as.factor(datumAPC$field)
datumAPC$JCR_quart <- as.factor(datumAPC$JCR_quart)
datumAPC$year <- as.factor(datumAPC$year)
datumAPC$Issue=as.factor(datumAPC$Issue)


# Rescaled numerical fields, into new columns
# datum <- datum %>% 
#   mutate(auth_count_scaled = scale(auth_count,center = TRUE, scale = TRUE),
#          AIS_scaled = scale(AIS,center = TRUE, scale = TRUE))
datumAPC$auth_count_scaled <- scale(datumAPC$auth_count)[,1]
summary(datumAPC$auth_count)
summary(datumAPC$auth_count_scaled)


datumAPC$AIS_scaled <- scale(datumAPC$AIS)[,1] 
summary(datumAPC$AIS_scaled)
summary(datumAPC$AIS)


datumAPC$OAlab[datumAPC$OAlab=="Closed Accesr"]="Closed Access"
datumAPC$OAlab[datumAPC$OAlab=="Closed Access"]="Access Closed"
datumAPC$OAlab <- as.factor(datumAPC$OAlab)
levels(datumAPC$OAlab)


datumAPC$APC_scaled=scale(datumAPC$APC)[,1]
summary(datumAPC$APC)
summary(datumAPC$APC_scaled)

head(datumAPC)


#Some basic plots
plot(APC~field,data=datumAPC)
plot(citations~APC,data=datumAPC)
plot(APC~year,data=datumAPC)
plot(APC~auth_count_scaled,data=datumAPC)
plot(APC~AIS_scaled,data=datumAPC)
plot(APC~JCR_quart,data=datumAPC)


###Basic summary statistics
datumAPC %>%
  group_by(field) %>%
  summarise(n=n_distinct(jour))
###APC charges for Biochemistry and Cellular Biology are causing a problem
###because there is only one journal
###Remove that field
levels(datumAPC$field)
BiochemSample=which(grepl("Biochemistry",datumAPC$field))
BiochemSample
datumAPC=datumAPC[-BiochemSample,]

##### Examine citations by variables

#journal
levels(datumAPC$jour)
summary(datumAPC$jour)
#print range of records by journal
range(summary(datumAPC$jour))
#number of journals
length(unique(sort(datumAPC$jour)))

#Count of samples by field
fieldCount=datumAPC %>% group_by(field) %>%
  summarise(Articles=length(citations))
mean(fieldCount$Articles)
min(fieldCount$Articles)
max(fieldCount$Articles)

#Counts of samples by jour
jourCount=datumAPC %>% group_by(jour) %>%
  summarise(Articles=length(citations))
mean(jourCount$Articles)
min(jourCount$Articles)
max(jourCount$Articles)

#Total number of samples
length(datumAPC$citations)


#### Modeling of results


#Poisson regression of basic results
resultsAPC1=glm(citations~APC,data=datumAPC)
summary(resultsAPC1)

#Neg Binomial regression of basic results
resultsAPC2=glm.nb(citations~APC,data=datumAPC)
summary(resultsAPC2)

anova(resultsAPC1,resultsAPC2)



resultsAPC1.1=glmer(citations~APC_scaled+(1|field),data=datumAPC,family=poisson)
summary(resultsAPC1.1)


resultsAPC2.1=glmer.nb(citations~APC_scaled+(1|field),data=datumAPC)
anova(resultsAPC1.1,resultsAPC2.1)
summary(resultsAPC2.1)



resultsAPC2.2=glmer.nb(citations~APC_scaled+auth_count_scaled+
                    JCR_quart+
                      AIS_scaled+
                      year+
                      (1|field/jour), 
                    data = datumAPC,na.action=na.omit)
anova(resultsAPC2.2,resultsAPC2.1)
summary(resultsAPC2.2)
Anova(resultsAPC2.2)

resultsAPC.int=glmer.nb(citations~APC_scaled*auth_count_scaled+
                          JCR_quart+JCR_quart:APC_scaled+
                          AIS_scaled+AIS_scaled:APC_scaled+
                          year+year:APC_scaled+
                          (1|field/jour), 
                        data = datumAPC,na.action=na.omit)
anova(resultsAPC.int,resultsAPC2.2)
Anova(resultsAPC.int)
summary(resultsAPC.int)

### plot results from AIS interaction


AIS_range=c(0.5,1,1.5,2,2.5)
#AISScaled=(AIS_range-mean(datumAPC$AIS,na.rm=TRUE))/sd(datumAPC$AIS,na.rm=TRUE)
APC_range=c(100,1000,2000,3000,4000,5000)
APCScaled=(APC_range-mean(datumAPC$APC,na.rm=TRUE))/sd(datumAPC$APC,na.rm=TRUE)
#emmip(resultsAPC.int,AIS_scaled~APC_scaled,
#      cov.reduce=range)
AISInt=emmip(resultsAPC.int,AIS_scaled~APC_scaled,
             at=list(AIS_scaled=AIS_range,
                     APC_scaled=APCScaled),
             CIs=TRUE,level=0.95,type="response",
            plotit=FALSE)
AISPlot=ggplot(AISInt,aes(color=as.factor(AIS_scaled),
                  x=APC_scaled,y=yvar,
                  ymin=LCL,ymax=UCL))+
  geom_pointrange(position=position_dodge(width=0.1))+
  geom_line()+
  labs(y="Citations",x="APC")+
  theme_classic()+
  scale_x_continuous(labels=APC_range,
                     breaks=APCScaled)+
  scale_color_discrete(labels=AIS_range,
                      breaks=AIS_range)+
  labs(color="AIS_Scaled")+#title="Relationship between AIS, access type, and citations",
  theme(plot.title=element_text(hjust=0.5))+
  theme(legend.position=c(0.875,0.8))+
  scale_color_discrete(name="AIS (Scaled)")+
  coord_cartesian(ylim = c(0, 200))
AISPlot
  
  
  # scale_color_manual(values=c("#000000","#E69F00","#009E73","#FFD700"))+
  # scale_shape_manual(values=c(15:18))+

summary(datumAPC$APC_scaled)
summary(datumAPC$APC)
summary(datumAPC$AIS_scaled)
summary(datumAPC$AIS)



#Interaction between year and access
x=c(2013,2014,2015,2016,2017,2018)
APC_range=c(100,1000,2000,3000,4000,5000)
APCScaled=(APC_range-mean(datumAPC$APC,na.rm=TRUE))/sd(datumAPC$APC,na.rm=TRUE)
emmip(resultsAPC.int,year~APC_scaled,
      at=list(APC_scaled=APCScaled,AIS_scaled=2.5))
YearInt=emmip(resultsAPC.int,year~APC_scaled,
              type="response",CIs=TRUE,level=0.95,
              plotit=FALSE,
              at=list(APC_scaled=APCScaled,AIS_scaled=2.5))
YearPlot=ggplot(YearInt,aes(color=year,fill=year,
                   x=APC_scaled,y=yvar,
                   ymin=LCL,ymax=UCL))+
  geom_pointrange(position=position_dodge(width=0.1))+
  geom_line()+
  labs(y="Citations",x="APC")+
  theme_classic()+#labs(title="Relationship between year, APC, and citations")+
  scale_x_continuous(labels=APC_range,
                        breaks=APCScaled)+
  theme(plot.title=element_text(hjust=0.5))+
  scale_color_discrete(name="Year")+
  scale_fill_discrete(name="Year")+
  theme(legend.position=c(0.85,0.78))
YearPlot

ggarrange(AISPlot,YearPlot,
          labels=c("A","B"),
          ncol=2,nrow=1, label.x=0.2, label.y=0.95)
