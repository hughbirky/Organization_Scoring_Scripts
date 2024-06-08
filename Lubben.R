library(tidyverse)
library(readxl)
library(Rcpp)
library(readxl)
library(writexl)
library(tidyverse)
library(dplyr)
library(purrr)
library(xlsx)
library(svDialogs)
library(stringr)
library(tibble)
library(stringdist)



### For this script, you will need to do a few things. The first thing is 

# Clearing the environment of previous variables
rm(list=ls()) 

# Clearing the console of previous junk
shell("cls")

# Participant folder name
participant <- "05.17.2024"

# Making the current path
path <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Analysis/Code/R01R21 Scripts/Lubben"


# Setting the working directory to the path
setwd(path)
# Getting a list of all of the excel files
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")
# Filtering out everything but the file we are trying to score
files <- files[!grepl("Scored", files)]
files <- files[!grepl("Old",files)]

# For renaming the record IDs to their actual names
recordID <- c("Test2","CI200","CI201","CI202","CI203","CI204","CI205","CI206_incomplete","CI207","CI208","CI209","CI210","CI211","CI212","CI213","CI214","CI215","CI216","CI217","CI218","CI219","CI220")


# Import csv data
Data <- read.csv(files[1])


# Renaming parts of the data frame that are blank
Data <- Data %>% rename("Total_Score" = "redcap_repeat_instrument")


# Pre-allocate ScorePre and Score columns
Data$Total_Score <- rep(NA, nrow(Data))


# Renaming the recordID colummn
for(d in 1:nrow(Data)){
  for(r in 1:length(recordID)){
    if(Data$record_id[d] == r){
      Data$record_id[d] <- paste0(recordID[r],"_",Data$visit_lecimplant[d])
    }
  }
}




for(l in 1:length(Data$record_id)){
  
  # Resetting the sum to 0
  sum <- 0
  
  # Running this to sum all of the nonReversed numbers normally
  for(i in 1:18){
    # Checking to make sure that the data is not blank
    if(!is.na(Data[l,paste0("lubb",i)])){
      # Summing by getting the column that corresponds to the number in the vector
      sum <- sum + Data[l,paste0("lubb",i)]
    }
  }
  
  
  if(sum != 0){
    # Adding the raw score to the blank column
    Data$Total_Score[l] <- sum

  }
}



# Writing the new excel sheet to the other folder
write.xlsx(Data, paste0("Lubben_Scored_",participant,".xlsx"),showNA = F)
setwd("C:/Users/hughm/Desktop")
