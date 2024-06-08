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
participant <- c("CI216")
date <- c("3 mo")
calDate <- "06.06.2024"
# date <- c("preop")
# date <- c("1 mo")
# date <- c("3 mo")
# date <- c("6 mo")=

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
path <- paste0(path,"/",files[1],"/Matlab Tasks/Lexical Decision Task")
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
analysis <- paste0(analysis,"/",files1[1],"/Matlab Tasks/Lexical Decision Task")
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
# Computing length of original column
length <- length(Data1$Word_num)

# Adding in extra rows to make it 160
if(length < 160){
  for(z in 1:(160 - length)){
    Data1[nrow(Data1) + 1,] <- NA
  }
}

# Creating# Creating a copy of the data frame
Data2 <- Data1
# Shifting rows around


for(i in 1:length){
  Data2[Data1$Word_num[i],] <- Data1[i,]
}

# Making rows blank otherwise
for(f in 1:160){
  if(f != Data2$Word_num[f]){
    Data2[f,] <- NA
  }
}

Data2$Correct <- NA

# Scoring 
for(c in 1:length(Data2$ParticipantAnswer)){
  if(!is.na(Data2$ParticipantAnswer[c])){
    if(Data2$ParticipantAnswer[c] == Data2$TrueAnswer[c]){
      Data2$Correct[c] <- 1
    } else{
      Data2$Correct[c] <- 0
    }
  }
}


# Filtering by group
group1 <- Data2[Data2$Group_num==1,] 
group1 <- group1[!is.na(group1$Group_num),] 
group2 <- Data2[Data2$Group_num==2,] 
group2 <- group2[!is.na(group2$Group_num),] 
group3 <- Data2[Data2$Group_num==3,] 
group3 <- group3[!is.na(group3$Group_num),] 
group4 <- Data2[Data2$Group_num==4,] 
group4 <- group4[!is.na(group4$Group_num),] 
group5 <- Data2[Data2$Group_num==5,] 
group5 <- group5[!is.na(group5$Group_num),] 

total <- Data2[!is.na(Data2$Group_num),]

# Making scoring columns
Data2$Total_Correct <- NA
Data2$Total_RT <- NA
Data2$Category1_Correct <- NA
Data2$Category1_RT <- NA
Data2$Category2_Correct <- NA
Data2$Category2_RT <- NA
Data2$Category3_Correct <- NA
Data2$Category3_RT <- NA
Data2$Category4_Correct <- NA
Data2$Category4_RT <- NA
Data2$Category5_Correct <- NA
Data2$Category5_RT <- NA

# Scoring
Data2$Total_Correct[1] <- mean(total$Correct)*100
Data2$Total_RT[1] <- mean(total$Time)
Data2$Category1_Correct[1] <- mean(group1$Correct)*100
Data2$Category1_RT[1] <- mean(group1$Time)
Data2$Category2_Correct[1] <- mean(group2$Correct)*100
Data2$Category2_RT[1] <- mean(group2$Time)
Data2$Category3_Correct[1] <- mean(group3$Correct)*100
Data2$Category3_RT[1] <- mean(group3$Time)
Data2$Category4_Correct[1] <- mean(group4$Correct)*100
Data2$Category4_RT[1] <- mean(group4$Time)
Data2$Category5_Correct[1] <- mean(group5$Correct)*100
Data2$Category5_RT[1] <- mean(group5$Time)


setwd(analysis)
write.xlsx(Data2, paste0("LD ",participant," Ordered_Scored.xlsx"),showNA = F)


setwd(path)


# Writing the new excel sheet to the other folder
write.xlsx(Data2, paste0("LD ",participant,"_",date," Ordered_Scored.xlsx"),showNA = F)
