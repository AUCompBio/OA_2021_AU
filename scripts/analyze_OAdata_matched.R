
library(dplyr)
library(ggplot2)

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
matched2=summaryBy(norm_cit~journal+OAlab+vol_issue, data=matched,FUN=c(mean,sum,length))

#reshape data to have column for each type of access
m3=dcast(matched2,vol_issue+journal~OAlab,value.var="norm_cit.sum")
m4=dcast(matched2,vol_issue+journal~OAlab,value.var="norm_cit.length")
m5=dcast(matched2,vol_issue+journal~OAlab,value.var="norm_cit.mean")

#add count columns to m5 dataframe
m5$CA.count=m4$`Closed Access`
m5$OA.count=m4$`Other Gold`

#sum values for Bronze and Green
m3=m3 %>% rowwise() %>% mutate(free_sum = sum(Bronze, Green, na.rm = TRUE))
m4=m4 %>% rowwise() %>% mutate(free_count = sum(Bronze, Green, na.rm = TRUE))
m5$free.count=m4$free_count
m5$free_cit_mean=ifelse(m4$free_count==0,NA,m3$free_sum/m4$free_count)
m5=m5 %>% rowwise() %>% mutate(free_cit_diff = free_cit_mean - `Closed Access`)
m5=m5 %>% rowwise() %>% mutate(paid_cit_diff = `Other Gold` - `Closed Access`)
m5=m5 %>% rowwise() %>% mutate(paid_cit_adv = paid_cit_diff - free_cit_diff)

#summary of the citation difference between nonOA and paid vs. free OA options
summary(m5$paid_cit_diff)
summary(m5$free_cit_diff)

#summary of the difference between paid and free OA advantages
summary(m5$paid_cit_adv)


#combine with metadata again
md <- read_csv("data/oa_metadata.csv", col_names = TRUE)

#join metadata with clean data
m5 <- left_join(m5, md, by = "journal")


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

vplot <- ggplot(m5,aes(x=as.factor(JCR_quart),y=paid_cit_diff,fill=as.factor(JCR_quart))) +
  geom_violin(trim=FALSE) 
vplot <- vplot + ggtitle("Paid Citation Advantage by JIF Quartile") +
  xlab("Journal Impact Factor Quartile (Q1>Q4)") + ylab("Citation Advantage Paid") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)
vplot

# violin plot: Citations by JIF quartile FREE

vplot <- ggplot(m5,aes(x=as.factor(JCR_quart),y=free_cit_diff,fill=as.factor(JCR_quart))) +
  geom_violin(trim=FALSE) 
vplot <- vplot + ggtitle("Free Citation Advantage by JIF Quartile") +
  xlab("Journal Impact Factor Quartile (Q1>Q4)") + ylab("Citation Advantage Free") + 
  theme(legend.position="none", plot.title=element_text(hjust = 0.5)) + 
  stat_summary(fun.data=data_summary)
vplot
