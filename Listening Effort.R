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
path <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Analysis/Code/R01R21 Scripts/Listening Effort"


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
recordID <- c("Test2","CI200","CI201","CI202","CI203","CI204","CI205","CI206_incomplete","CI207","CI208","CI209","CI210","CI211","CI212","CI213","CI214","CI215","CI216","CI217","CI218","CI219")

# Making scaled data frame for scaled scoring
scale <- c(0,9,15,18,21,23,25,27,28,30,31,33,34,35,36,38,39,40,41,42,43,44,45,46,47,48,49,50,
           51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,68,69,71,72,74,76,78,81,85,91,100)
raw <- 21:75
scaled <- data.frame(raw,scale)




# Import csv data
Data <- read.csv(files[1])

# Making the lists that need to be reversed or not
nonReversed <- c(1,2,3,4,5,6,7,9,11,12,13,14,15,17,18,19)
reversed4 <- c(8,10)
reversed3 <- c(16,20,21)

# Renaming parts of the data frame that are blank
Data <- Data %>% rename("Score_Raw" = "redcap_repeat_instrument")
Data <- Data %>% rename("Final_Score" = "redcap_repeat_instance")

# Pre-allocate ScorePre and Score columns
Data$Score_Raw <- rep(NA, nrow(Data))
Data$Final_Score <- rep(NA, nrow(Data))

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
  for(i in 1:length(nonReversed)){
    # Checking to make sure that the data is not blank
    if(!is.na(Data[l,paste0("leqci_",nonReversed[i])])){
      # Summing by getting the column that corresponds to the number in the vector
      sum <- sum + Data[l,paste0("leqci_",nonReversed[i])]
    }
  }
  
  for(i in 1:length(reversed4)){
    # Checking to make sure that the data is not blank
    if(!is.na(Data[l,paste0("leqci_",reversed4[i])])){
      # Summing by getting the column that corresponds to the number in the vector
      # and reversing it
      sum <- sum + abs(Data[l,paste0("leqci_",reversed4[i])] - 5)
    }
  }
  for(i in 1:length(reversed3)){
    # Checking to make sure that the data is not blank
    if(!is.na(Data[l,paste0("leqci_",reversed3[i])])){
      # Summing by getting the column that corresponds to the number in the vector
      # and reversing it
      sum <- sum + abs(Data[l,paste0("leqci_",reversed3[i])] - 4)
    }
  }
  if(sum != 0){
  # Adding the raw score to the blank column
  Data$Score_Raw[l] <- sum
  # Pulling the corresponding scaled score given what their raw score was
  Data$Final_Score[l] <- scaled$scale[as.numeric(which(scaled$raw == Data$Score_Raw[l]))]
  }
}



# Writing the new excel sheet to the other folder
write.xlsx(Data, paste0("Listening_Effort_Scored_",participant,".xlsx"),showNA = F)
setwd("C:/Users/hughm/Desktop")
