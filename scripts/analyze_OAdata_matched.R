# install.packages('ggplot2') # run once
library(ggplot2)
# install.packages('tidyverse') # run once
library(tidyverse)
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
library(dplyr)
#install.packages('lmerTest')
library(lmerTest)
#install.packages('car')
library(car)


# the following code takes matched data
# from combine_OAdata.R script
# and calculated citation advantage
# using an lm with various factors


#explore matched data a little more
matched <- read_csv("data/matched_OA_data_fin.csv", col_names = TRUE)

#combine volume and issue into single column
matched$vol_issue=paste(matched$Volume,matched$Issue,sep=".")

# check data
names(matched)
head(matched)
summary(matched)

#try to summarize 
matched2=summaryBy(norm_cit~jour+OAlab+vol_issue, data=matched,FUN=c(mean,sum,length))

#reshape data to have column for each type of access
m3=dcast(matched2,vol_issue+jour~OAlab,value.var="norm_cit.sum")
m4=dcast(matched2,vol_issue+jour~OAlab,value.var="norm_cit.length")
m5=dcast(matched2,vol_issue+jour~OAlab,value.var="norm_cit.mean")

#add count columns to m5 dataframe
m5$CA.count=m4$`Closed Access`
m5$OA.count=m4$`Other Gold`

#sum values for Bronze and Green
m3=m3 %>% rowwise() %>% mutate(free_sum = sum(Bronze, Green, na.rm = TRUE))
m4=m4 %>% rowwise() %>% mutate(free_count = sum(Bronze, Green, na.rm = TRUE))

#add free sum column to m5 dataframe
m5$free.count=m4$free_count

#re-calculate mean of free OA articles
m5$free_cit_mean=ifelse(m4$free_count==0,NA,m3$free_sum/m4$free_count)

#compute difference between Closed Access and OA paid/free
m5=m5 %>% rowwise() %>% mutate(free_cit_diff = free_cit_mean - `Closed Access`)
m5=m5 %>% rowwise() %>% mutate(paid_cit_diff = `Other Gold` - `Closed Access`)
m5=m5 %>% rowwise() %>% mutate(green_cit_diff = Green - `Closed Access`)

#add column with the difference between paid vs free cit difference
m5=m5 %>% rowwise() %>% mutate(paid_cit_adv = paid_cit_diff - green_cit_diff)

#summary of the citation difference between nonOA and paid vs. free OA options
summary(m5$paid_cit_diff)
#summary(m5$free_cit_diff)
summary(m5$green_cit_diff)

#summary of the difference between paid and free OA advantages
summary(m5$paid_cit_adv)


#combine with metadata again
md <- read_csv("data/oa_metadata.csv", col_names = TRUE)
md$APC <- as.numeric(gsub("[\\$,]", "", md$APC))
#remove weird columns
md=md[,c(1:8)]

#join metadata with clean data
merged <- merge(m5, md, by="jour")

#summarize by field (this is an absolute mess!)
green=tapply(merged$green_cit_diff,merged$field,mean,na.rm=T)
paid=tapply(merged$paid_cit_diff,merged$field,mean,na.rm=T)
w=as.data.frame(cbind(green,paid))
w$paid_rank=rank(w$paid,na.last="keep")
w$green_rank=rank(w$green,na.last="keep")
plot(w$paid_rank,w$green_rank)
text(w$paid_rank, w$green_rank, labels = row.names(w[1:12,]), pos = 4)
abline(0.5,0.8)
#now we are ready to do some stats!
# Recode categorical fields (deviation-compares level to grand mean)
#contrasts(matched$field) = contr.sum(12)
#contrasts(matched$jour_loc) =contr.sum(12)
#contrasts(matched$OAlab) =contr.sum(4)
matched$OAlab=as.factor(matched$OAlab)

#do NOT run this! need to work on the model a bit!
#fit <- glmer(norm_cit~OAlab+field+JCR_quart+jour_loc+(1|field:jour), #+(1|journal:vol_issue), 
 #          data = matched, family=poisson)
#summary(fit)

#from Amanda's code
mod2.1 <- glmer(norm_cit~relevel(OAlab, ref = "Closed Access")+JCR_quart+AIS+(1|jour:vol_issue), 
                data = matched, family = poisson)
summary(mod2.1)
Anova(mod2.1)


#quick plots
fun <- function(x){
  c(m=mean(x), v=var(x), n=length(x))
}

#compute data summary to add to plot
data_summary <- function (datum) {
  m = mean(datum)
  ymin = m-sd(datum)
  ymax = m+sd(datum)
  return(c(y=m,ymin=ymin,ymax=ymax))
}
# violin plot: Citations by JIF quartile PAID

vplot <- ggplot(merged,aes(x=as.factor(JCR_quart),y=paid_cit_diff,fill=as.factor(JCR_quart))) +
  geom_violin(trim=FALSE) 
vplot <- vplot + ggtitle("Paid Citation Advantage by JIF Quartile") +
  xlab("Journal Impact Factor Quartile (Q1>Q4)") + ylab("Citation Advantage Paid") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)
vplot

# violin plot: Citations by JIF quartile FREE

vplot <- ggplot(merged,aes(x=as.factor(JCR_quart),y=free_cit_diff,fill=as.factor(JCR_quart))) +
  geom_violin(trim=FALSE) 
vplot <- vplot + ggtitle("Free Citation Advantage by JIF Quartile") +
  xlab("Journal Impact Factor Quartile (Q1>Q4)") + ylab("Citation Advantage Free") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)
vplot


#paid citation advantage per field
vplot <- ggplot(merged,aes(x=field,y=paid_cit_diff)) +
  geom_violin(trim=FALSE) 
vplot <- vplot + ggtitle("Citation Advantage per Field") +
  xlab("Field") + ylab("Paid Citation Difference") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)
vplot

#green citation advantage per field
vplot <- ggplot(merged,aes(x=field,y=green_cit_diff)) +
  geom_violin(trim=FALSE) 
vplot <- vplot + ggtitle("Citation Advantage per Field") +
  xlab("Field") + ylab("Green Citation Difference") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)
vplot
