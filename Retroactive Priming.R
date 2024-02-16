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

# C:/Users/hughm/Dropbox
# Clearing the console of previous junk

shell("cls")
# shell("clear")
# Clearing the environment of previous variables
rm(list=ls()) 


participant <- c("CI200")
date <- c("6 month")
move_to_analysis <- T

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
path <- paste0(path,"/",files[1],"/Matlab Tasks/Retroactive Priming")
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
analysis <- paste0(analysis,"/",files1[1],"/Matlab Tasks/Retroactive Priming")
if(!dir.exists(analysis)){
  dir.create(analysis)
}



setwd(path)
# Getting a list of all of the excel files
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")


# Import excel data
Data1 <- read_excel(files[1])

# Duplicating because I'm lazy
Data2 <- Data1


# Turing correct column into numbers
for(c in 1:length(Data2$Correct)){
  if(Data2$Correct[c] == TRUE){
    Data2$Correct[c] <- 1
  } 
  if(Data2$Correct[c] == FALSE){
    Data2$Correct[c] <- 0
  }
}

# Sorting in alphabetical order
Data2 <- Data2 %>% arrange(Prime)

# Reading in the template
template <- read_excel("C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Analysis/Code/Retroactive Priming/RetroPriming_Template.xlsx")

# Adding in extra columns
Data2$Noise <- template$Noise
Data2$Condition <- template$Condition

# Making scoring column
Data2$Correct_Quiet_Unrelated <- NA
Data2$RT_Quiet_Unrelated <- NA
Data2$Correct_Quiet_Related <- NA
Data2$RT_Quiet_Related <- NA
Data2$Correct_Noise1_Unrelated <- NA
Data2$RT_Noise1_Unrelated <- NA
Data2$Correct_Noise1_Related <- NA
Data2$RT_Noise1_Related <- NA
Data2$Correct_Noise2_Unrelated <- NA
Data2$RT_Noise2_Unrelated <- NA
Data2$Correct_Noise2_Related <- NA
Data2$RT_Noise2_Related <- NA

# Filtering (p5 is quiet, zero is noise1, n5 is noise2)
Quiet_Unrelated <- Data2 %>% 
  filter(Noise == "p5") %>%
  filter(Condition == "U")
Quiet_Related <- Data2 %>% 
  filter(Noise == "p5") %>%
  filter(Condition == "R")
Noise1_Unrelated <- Data2 %>% 
  filter(Noise == "zero") %>%
  filter(Condition == "U")
Noise1_Related <- Data2 %>% 
  filter(Noise == "zero") %>%
  filter(Condition == "R")
Noise2_Unrelated <- Data2 %>% 
  filter(Noise == "n5") %>%
  filter(Condition == "U")
Noise2_Related <- Data2 %>% 
  filter(Noise == "n5") %>%
  filter(Condition == "R")


# Scoring column
Data2$Correct_Quiet_Unrelated[1] <- mean(Quiet_Unrelated$Correct)
Data2$RT_Quiet_Unrelated[1] <- mean(Quiet_Unrelated$`Response time`)
Data2$Correct_Quiet_Related[1] <- mean(Quiet_Related$Correct)
Data2$RT_Quiet_Related[1] <- mean(Quiet_Related$`Response time`)
Data2$Correct_Noise1_Unrelated[1] <- mean(Noise1_Unrelated$Correct)
Data2$RT_Noise1_Unrelated[1] <- mean(Noise1_Unrelated$`Response time`)
Data2$Correct_Noise1_Related[1] <- mean(Noise1_Related$Correct)
Data2$RT_Noise1_Related[1] <- mean(Noise1_Related$`Response time`)
Data2$Correct_Noise2_Unrelated[1] <- mean(Noise2_Unrelated$Correct)
Data2$RT_Noise2_Unrelated[1] <- mean(Noise2_Unrelated$`Response time`)
Data2$Correct_Noise2_Related[1] <- mean(Noise2_Related$Correct)
Data2$RT_Noise2_Related[1] <- mean(Noise2_Related$`Response time`)





setwd(analysis)
write.xlsx(Data2, paste0("Retro ",participant," Ordered_Scored.xlsx"),showNA = F)


setwd(path)


# Writing the new excel sheet to the other folder
write.xlsx(Data2, paste0("Retro ",participant," Ordered_Scored.xlsx"),showNA = F)
