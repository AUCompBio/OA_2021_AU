## Identify journals lacking publications in all years of timeframe.
## If a journal lacks publications in all years of the timeframe, this
## could suggest that the journal changed names or started/ceased publishing open-access

# Change to your working directory
setwd("~/Dropbox/PhD/Classes/20_Fall/Intro-Comp-Biol/OA_publishing_project/OA_2021_AU")

# Load libraries
library(tidyverse)

# Load dataset
datum <- read_csv("data/OA_data_fin.csv", col_names = TRUE)

# Create empty dataframe to populate
df <- data.frame()

# Loop over the journals in 'datum'
for (i in unique(datum$journal)){
    # Subset 'datum' object by journal
    journal <- filter(datum, journal == i) 
    # Count the number of years for each journal with publications. Max possible is 6
        df <- rbind(df,
                    data.frame(x=i, # First column will include journal names
                    y=length(unique(journal$year)), # Second column will be number of years with publications
                    stringsAsFactors=FALSE))
    }

# Designate column names
colnames(df) <- c("Journal", "YearsInDataset")
str(df)

# Subset dataframe by journals with less than 6 years of publications
df2 <- df[df$YearsInDataset<6,]

write.csv(x=df2, file="data/Missing-year-journals.csv", row.names=FALSE, quote=FALSE)
