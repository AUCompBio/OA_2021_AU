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

setwd("C:/Users/tds0009/Documents/GitHub/OA_2021_AU/")
datumAPC=read.csv("data/OA_data_othergoldONLY.csv")




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
datumAPC=datumAPC[-BiochemSample,]



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

emmip(resultsAPC.int,AIS_scaled~APC_scaled,cov.reduce=range)
#       at=list(APC_scaled=auth_count_scaled=c(0)),CIs=TRUE,level=0.95,
#       position="jitter")
# AIS_range=c(0,0.5,1.0,1.5,2.0,2.5)
# AISInt=emmip(mod2.2.1.int,OAlab~AIS_scaled,
#              at=list(AIS_scaled=AIS_range),
#              CIs=TRUE,level=0.95,type="response",
#              plotit=FALSE)
# ggplot(AISInt,aes(color=OAlab,fill=OAlab,
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

summary(datumAPC$APC_scaled)

