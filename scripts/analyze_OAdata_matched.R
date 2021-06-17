

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
matched2=summaryBy(norm_cit~journal+OAlab+vol_issue, data=matched,FUN=mean)

#reshape data to have column for each type of access
m3=dcast(matched2,vol_issue+journal~OAlab)

m3$paid_cit_adv=m3$`Other Gold`-m3$`Closed Access`
m3$free_cit_adv=mean(m3$Bronze+m3$Green,na.rm=T)-m3$`Closed Access`

summary(m3$paid_cit_adv)
summary(m3$free_cit_adv)
