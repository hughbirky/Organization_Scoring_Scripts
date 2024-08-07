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
participant <- c("CI222")
date <- c("1 mo")
calDate <- "07.22.2024"
# date <- c("preop")
# date <- c("1 mo")
# date <- c("3 mo")
# date <- c("6 mo")


# Finding who's computer we are on
origin <- "C:/Users"
# Setting the working path for data collection
setwd(origin)
# Getting a list of all of the excel files
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")
# Getting the folder we need for the participant
files <- files[grepl("hughm", files)]

if(files == "hughm"){
  path <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Data Collection/Subject testing/Cochlear Implant"
  analysis <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Analysis/Scoring/Completed scoring"
} else{
  path <- "f"
}






# Setting the working path for data collection
setwd(path)
# Getting a list of all of the excel files
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")
# Getting the folder we need for the participant
files <- files[grepl(participant, files)]
# Writing the new path with the folder we just got
path <- paste0(path,"/",files[1])
# Setting the working directory to that
setwd(path)
# Getting a list of all of the folders
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")
# Getting the folder we need for the visit type
files <- files[grepl(date, files)]
# Writing our new path to the talker discrimination
path <- paste0(path,"/",files[1],"/Matlab Tasks/Sentence Verification Task")
# Recording for naming
date_actual <- files[1]
# Setting the new working directory
setwd(path)





# Setting the working path for analysis scoring
setwd(analysis)
# Getting a list of all of the excel files
files1 = list.files(full.names = T)
# Getting rid of the ./
files1 <- gsub(x = files1, pattern = "./", replacement = "")
# Getting the folder we need for the participant[p]
files1 <- files1[grepl(participant, files1)]
# Writing the new path with the folder we just got
analysis <- paste0(analysis,"/",files1[1])
# Setting the working directory to that
setwd(analysis)
# Getting a list of all of the folders
files1 = list.files(full.names = T)
# Getting rid of the ./
files1 <- gsub(x = files1, pattern = "./", replacement = "")
# Getting the folder we need for the visit type
files1 <- files1[grepl(date, files1)]
# Writing our new path to the talker discrimination
analysis <- paste0(analysis,"/",files1[1],"/Matlab Tasks/Sentence Verification Task")
if(!dir.exists(analysis)){
  dir.create(analysis)
}




setwd(path)
# Getting a list of all of the excel files
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")


# Import excel data
Data <- read_excel(files[1])

# Getting rid of empty cells
Data1 <- Data %>% 
  filter(!is.na(TrueAnswer))
Data1$Time <- as.numeric(Data1$Time)


# Computing length of original column
length <- length(Data1$ParticipantAnswer)


# Adding in the extra columns
Data1$Correct <- NA
Data1$RT_filter <- NA
Data1$Accuracy_TRUE <- NA
Data1$RT_TRUE <- NA
Data1$Accuracy_FALSE <- NA
Data1$RT_FALSE <- NA
Data1$Accuracy_Total <- NA
Data1$RT_Total <- NA

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


# Scoring
Data1 <- Data1 %>%
  mutate(Correct = ifelse(TrueAnswer == ParticipantAnswer, 1, 0))

# Filtering for RT_Filter
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
write.xlsx(Data1, paste0(participant,"_",date_actual,"_SVT.xlsx"),showNA = F)









