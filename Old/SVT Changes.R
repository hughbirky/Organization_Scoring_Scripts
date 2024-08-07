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


shell("cls")
# shell("clear")
# Clearing the environment of previous variables
rm(list=ls()) 


# Participant folder name
participant <- c("CI200","CI201","CI202","CI203","CI204","CI205","CI207","CI208","CI209","CI210","CI211","CI213","CI214","CI215","CI216","CI217")
# participant <- c("CI200")
date <- c("preop","1 mo","3 mo","6 mo")
# date <- c("preop")
# date <- c("1 mo")
# date <- c("3 mo")
# date <- c("6 mo")


# Finding who's computer we are on
origin <- "C:/Users"
# Setting the working path for data collection
setwd(origin)
# Getting a list of all of the excel files
files_origin = list.files(full.names = T)
# Getting rid of the ./
files_origin <- gsub(x = files_origin, pattern = "./", replacement = "")
# Getting the folder we need for the participant
files_origin <- files_origin[grepl("hughm", files_origin)]

if(files_origin == "hughm"){
  path <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Data Collection/Subject testing/Cochlear Implant"
  analysis <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Analysis/Scoring/Completed scoring"
} else{
  path <- "f"
}


p = 1
d = 1
for(p in 1:length(participant)){
  for(d in 1:length(date)){
    # Setting origin
    if(files_origin == "hughm"){
      path <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Data Collection/Subject testing/Cochlear Implant"
      analysis <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Analysis/Scoring/Completed scoring"
    } else{
      path <- "f"
    }
    
    
    
    # Setting the working path for analysis scoring
    setwd(analysis)
    # Getting a list of all of the excel files
    files1 = list.files(full.names = T)
    # Getting rid of the ./
    files1 <- gsub(x = files1, pattern = "./", replacement = "")
    # Getting the folder we need for the participant[p]
    files1 <- files1[grepl(participant[p], files1)]
    # Writing the new path with the folder we just got
    analysis <- paste0(analysis,"/",files1[1])
    # Setting the working directory to that
    setwd(analysis)
    # Getting a list of all of the folders
    files1 = list.files(full.names = T)
    # Getting rid of the ./
    files1 <- gsub(x = files1, pattern = "./", replacement = "")
    # Getting the folder we need for the visit type
    if(length(files1) > 1){
      files1 <- files1[grepl(date[d], files1)]
    } else if(length(files1) == 1 && d > 1){
      next
    }
    
    # Recording for naming
    date_actual <- gsub(x = files1[1], pattern = " ", replacement = "")
    # Writing our new path to the talker discrimination
    analysis <- paste0(analysis,"/",files1[1],"/Matlab Tasks/Sentence Verification Task")
    if(!dir.exists(analysis) ){
      next
    } 
    
    
    
    
    setwd(analysis)
    # Getting a list of all of the excel files
    files = list.files(full.names = T)
    # Getting rid of the ./
    files <- gsub(x = files, pattern = "./", replacement = "")
    # Getting the folder we need for the participant
    files <- files[!grepl("V1", files)]
    files <- files[!grepl("V2", files)]
    
    
    # Import excel data
    Data <- read_excel(files[1])
    
    if("Value" %in% colnames(Data)){
      
    } else{
      Data <- mutate(Data, Value = Time) %>%
        relocate(Value, .before = 7)
    }
    
    
    names(Data)[10] <- "RT_filter"
    # Getting rid of possible extra columns
    Data <- Data[,1:10]
    
    # Getting rid of empty cells
    Data1 <- Data %>% 
      filter(!is.na(TrueAnswer))
    
    # Adding in the extra columns
    Data1$Accuracy_TRUE <- NA
    Data1$RT_TRUE <- NA
    Data1$Accuracy_FALSE <- NA
    Data1$RT_FALSE <- NA
    Data1$Accuracy_Total <- NA
    Data1$RT_Total <- NA
    
    
    # Making sure that the values are numeric
    Data1$Time <- as.numeric(Data1$Time)
    
    
    # Computing length of original column
    length <- length(Data1$ParticipantAnswer)
    
    
    # Changing the 1s and 0s to TRUE and FALSE
    if("true" %in% Data1$TrueAnswer){
      Data1 <- Data1 %>%
        mutate(TrueAnswer = ifelse(TrueAnswer == "true", TRUE, FALSE))
    } else if(1 %in% Data1$TrueAnswer){
      Data1 <- Data1 %>%
        mutate(TrueAnswer = ifelse(TrueAnswer == 1, TRUE, FALSE))
    } else if("TRUE" %in% Data1$TrueAnswer){
      Data1 <- Data1 %>%
        mutate(TrueAnswer = ifelse(TrueAnswer == "TRUE", TRUE, FALSE))
    }
    
    # Data1$Condition <- Data1$Correct
    
    # Scoring
    Data1 <- Data1 %>%
      mutate(Correct = ifelse(TrueAnswer == ParticipantAnswer, 1, 0))
    
    
    # Data1$RT_filter <- NA
    # Filtering for RT_Filter
    i = 1
    for(i in 1:length){
      if(!is.na(Data1$Correct[i]) && !is.na(Data1$Time[i])){
        if(Data1$Correct[i] == 1 && Data1$Time[i] < 3.501)
        Data1$RT_filter[i] = Data1$Time[i]
      } else {}
    }
    
    # Filtering for Condition
    Data1_True <- Data1 %>%
      filter(TrueAnswer == TRUE) 
    Data1_False <- Data1 %>%
      filter(TrueAnswer == FALSE) 
    
    # Calculating mean score
    Data1$Accuracy_TRUE[1] <- mean(Data1_True$Correct)*100
    Data1$Accuracy_FALSE[1] <- mean(Data1_False$Correct)*100
    Data1$Accuracy_Total[1] <- mean(Data1$Correct)*100
    
    # Filtering for wanted reaction time
    Data1_True_RT <- Data1_True %>%
      filter(Correct == 1) 
    Data1_True_RT <- Data1_True_RT %>%
      filter(Time < 3.501)
    
    Data1_False_RT <- Data1_False %>%
      filter(Correct == 1) 
    Data1_False_RT <- Data1_False_RT %>%
      filter(Time < 3.501)
    
    
    # Calculating Reaction Time
    Data1$RT_TRUE[1] <- mean(Data1_True_RT$Time)
    
    Data1$RT_FALSE[1] <- mean(Data1_False_RT$Time)
    
    
    
    # Filtering for total reaction time
    Total_Filtered <- Data1 %>%
      filter(Correct == 1) %>%
      filter(Time < 3.501)
    
    Data1$RT_Total[1] <- mean(Total_Filtered$Time)
    
    setwd(analysis)
    # if(!file.exists(paste0(participant[p],"_",date_actual,"_SVT_V1.xlsx"))){
      write.xlsx(Data1, paste0(participant[p],"_",date_actual,"_SVT_V2.xlsx"),showNA = F)
    # }
    
  }
}

