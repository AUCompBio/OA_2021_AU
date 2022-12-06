#setwd("C:/Users")
datum=read.csv("data/OA_data_othergoldONLY.csv")
head(datum)

###libraries
library(tidyverse)
library(car)
library(lme4)

###Check distribution of y-variable
hist(datum$citations)
hist(log(datum$citations))

### Tell R that categorical variables are categorical
datum$jour <- as.factor(datum$jour)
datum$field <- as.factor(datum$field)
#datum$jour_loc <- as.factor(datum$jour_loc)
datum$JCR_quart <- as.factor(datum$JCR_quart)
#datum$pub <- as.factor(datum$pub)
datum$year <- as.factor(datum$year)
#datum$gni_class <- as.factor(datum$gni_class)

### Check distribution of new APC variable
summary(datum$APC)
hist(datum$APC)


###Create scaled variables

datum$auth_count_scaled <- scale(datum$auth_count)
datum$AIS_scaled <- scale(datum$AIS) #why are we doing this twice?
# this makes a matrix inside of our dataframe
# head(datum) #Make sure it worked


###Run simple model without random effects

SubModel=glm(citations~APC+auth_count_scaled+JCR_quart+AIS_scaled+year, 
             data = datum,family=poisson)
summary(SubModel)
vif(SubModel)


#### Run main model with random effects
#### Journal not included as APC is almost perfectly corrected with journal



MainModel=glmer(citations~APC+auth_count_scaled+JCR_quart+AIS_scaled+year+(1|field), 
                               data = datum,family=poisson)
summary(MainModel)




###Check if scaling provides different/better results

datum$APC_scaled=scale(datum$APC)

MainModelScale=glmer(citations~APC_scaled+auth_count_scaled+JCR_quart+AIS_scaled+year+(1|field), 
                data = datum,family=poisson)
summary(MainModelScale)



###Compare poisson fit to negative binomial
MainModelNB=glmer.nb(citations~APC_scaled+auth_count_scaled+JCR_quart+AIS_scaled+year+(1|field), 
                     data = datum)
summary(MainModelNB)
anova(MainModelNB,MainModelScale)



#check for interactions
MainModelNB_Int=glmer.nb(citations~APC_scaled*auth_count_scaled+
                           JCR_quart+APC_scaled:JCR_quart+
                         AIS_scaled+AIS_scaled:APC_scaled+
                           year+year:APC_scaled+(1|field), 
                         data = datum)
summary(MainModelNB_Int)
Anova(MainModelNB_Int)

#check for all interactions
MainModelNM_Full=glmer.nb(citations~APC_scaled*auth_count_scaled*
                            JCR_quart*AIS_scaled*year+(1|field),data=datum,control=glmerControl(optimizer=c("bobyqa")))

