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



participant <- c("CI216")
date <- c("preop")
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
} else{
  path <- "f"
}


for(p in 1:length(participant)){
  
  analysis <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Analysis/Scoring/Completed scoring"
  # Setting the unwanted columns shared between spreadsheets
  gorillaColumns <- c("Event Index","UTC Timestamp","UTC Date and Time","Local Timezone","Experiment ID","Experiment Version",
                      "Tree Node Key","Repeat Key","Schedule ID","Participant Private ID","Participant Starting Group",
                      "Participant Status","Participant Completion Code","Participant External Session ID",
                      "Participant Device Type","Participant Device","Participant OS","Participant Browser",
                      "Participant Monitor Size","Participant Viewport Size","Checkpoint","Room ID","Room Order","Task Version",
                      "checkpoint-ncck",	"checkpoint-x5ti",	"checkpoint-vh38",	"checkpoint-73pu",	"checkpoint-czdn",
                      "checkpoint-vx9c",	"checkpoint-vpap",	"branch-r1ry",	"branch-usxa",	"branch-r7nz",	"randomiser-spvu",
                      "checkpoint-ylq9",	"checkpoint-cppz",	"randomiser-3ddq","Spreadsheet Name","X Coordinate","Y Coordinate",
                      "Timed Out","display","d","Vocoded","Spreadsheet Row","Screen Number","Screen Name")
  # Setting the working path for data collection
  setwd(path)
  # Getting a list of all of the excel files
  files = list.files(full.names = T)
  # Getting rid of the ./
  files <- gsub(x = files, pattern = "./", replacement = "")
  # Getting the folder we need for the participant
  files <- files[grepl(participant[p], files)]
  # Writing the new path with the folder we just got
  path <- paste0(path,"/",files[1])
  # Setting the working directory to that
  setwd(path)
  # Getting a list of all of the folders
  files = list.files(full.names = T)
  # Getting rid of the ./
  files <- gsub(x = files, pattern = "./", replacement = "")
  # Getting the folder we need for the visit type
  files <- files[grepl(date[p], files)]
  # Writing our new path to the talker discrimination
  path <- paste0(path,"/",files[1],"/Gorilla Tasks/Consonant")
  # Setting the new working directory
  setwd(path)
  # Getting a list of all of the files
  files = list.files(full.names = T)
  # Getting rid of the ./
  files <- gsub(x = files, pattern = "./", replacement = "")
  # Getting the files that we need
  files <- files[!grepl("Scored", files)]
  
  
  
  
  
  
  
  
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
  files1 <- files1[grepl(date[p], files1)]
  # Writing our new path to the talker discrimination
  analysis <- paste0(analysis,"/",files1[1],"/Gorilla Tasks/Consonant")
  if(!dir.exists(analysis)){
    dir.create(analysis)
  }
  
  
  
  # Setting the working directory to the path
  setwd(path)
  # Getting a list of all of the excel files
  files = list.files(full.names = T)
  # Getting rid of the ./
  files <- gsub(x = files, pattern = "./", replacement = "")
  
  # Filtering out files we don't want
  files <- grep(paste("fk1l", collapse = '|'),
                files, value = TRUE, invert = TRUE)
  files <- grep(paste("57kv", collapse = '|'),
                files, value = TRUE, invert = TRUE)
  files <- grep(paste("lt8f", collapse = '|'),
                files, value = TRUE, invert = TRUE)
  files <- grep(paste("yt6f", collapse = '|'),
                files, value = TRUE, invert = TRUE)
  
  # Import excel data
  Data <- read_excel(files[1])
  Data1 <- read_excel(files[2])
  
  # Removing unwanted shared columns from all spreadsheets
  Data <- Data[,!names(Data) %in% gorillaColumns]
  # Removing unwanted shared columns from all spreadsheets
  Data1 <- Data1[,!names(Data1) %in% gorillaColumns]
  
  # Fixing error with UTC Timestamp and Date
  Data <- Data[,!names(Data) %in% c("UTC Date","Local Date")]
  # Removing unwanted shared columns from all spreadsheets
  Data1 <- Data1[,!names(Data1) %in% c("UTC Date","Local Date")]
  
  # Fixing reaction time issue
  Data1$`Reaction Time` <- as.numeric(Data1$`Reaction Time`)
  Data$`Reaction Time` <- as.numeric(Data$`Reaction Time`)
  
  
  # Binding rows
  Data2 <- bind_rows(Data1,Data)
  
  
  
  
  # Actually removing the rows
  Data2 <- Data2 %>% filter(!grepl('AUDIO PLAY REQUESTED', Response))
  Data2 <- Data2 %>% filter(!is.na(Response))
  Data2 <- Data2 %>% filter(!is.na(Consonant))
  
  # Making list of consonants
  consonants <- c("K","Z","M","N","DH","SH","B","ZS","T","V","D","S","TH","G","P","F")
  
  # Counter
  total <- 0
  overallTotal <- 0
  count <- 0
  # Checking which vowel it is and summing
  for(v in 1:length(consonants)){
    # Adding the vowel total column
    for(i in 1:length(Data2$Consonant)){
      if(Data2$Consonant[i] == consonants[v]){
        # Adding on value if it meets the condition
        total <- total + Data2$Correct[i]
        count <- count + 1
      }
    }
    
    # Adding total to the corresponding section
    Data2[1,paste0(consonants[v], " Total")] <- (total/count)*100
    
    # Adding percentages to overall total
    overallTotal <- overallTotal + (total/count)*100
    
    # Resetting counter
    total <- 0
    count <- 0
  }
  
  # Getting grand total
  Data2[1,"Total"] <- overallTotal / length(consonants)
  
  # 
  # # Changing writing directory into the vowel folder
  path1 <- paste0("C:/Users/hughm/OneDrive - Belmont University/Personal/Desktop/VUMC/R01R21/Vowel_Consonant/",participant[p],"/Consonant")
  setwd(analysis)
  
  # Writing the new excel sheet to the other folder
  write.xlsx(Data2, paste0(participant[p],"_",date,"_Consonant_Scored1.xlsx"),showNA = F)
  
  setwd(path)
  write.xlsx(Data2, paste0(participant[p],"_",date,"_Consonant_Scored1.xlsx"),showNA = F)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Making the current path
  path <- paste0("C:/Users/hughm/OneDrive - Belmont University/Personal/Desktop/VUMC/R01R21/Vowel_Consonant/",participant[p])
  
  # Setting the unwanted columns shared between spreadsheets
  gorillaColumns <- c("Event Index","UTC Timestamp","UTC Date and Time","Local Timezone","Experiment ID","Experiment Version",
                      "Tree Node Key","Repeat Key","Schedule ID","Participant Private ID","Participant Starting Group",
                      "Participant Status","Participant Completion Code","Participant External Session ID",
                      "Participant Device Type","Participant Device","Participant OS","Participant Browser",
                      "Participant Monitor Size","Participant Viewport Size","Checkpoint","Room ID","Room Order","Task Version",
                      "checkpoint-ncck",	"checkpoint-x5ti",	"checkpoint-vh38",	"checkpoint-73pu",	"checkpoint-czdn",
                      "checkpoint-vx9c",	"checkpoint-vpap",	"branch-r1ry",	"branch-usxa",	"branch-r7nz",	"randomiser-spvu",
                      "checkpoint-ylq9",	"checkpoint-cppz",	"randomiser-3ddq","Spreadsheet Name","X Coordinate","Y Coordinate",
                      "Timed Out","display","d","Vocoded","Spreadsheet Row","Screen Number","Screen Name","Zone Name","Zone Type","Reaction Onset","Response Type","Attempt","Incorrect","Dishonest","randomise_blocks",
                      "randomise_trials")
  path <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Data Collection/Subject testing/Cochlear Implant"
  analysis <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Analysis/Scoring/Completed scoring"
  # Setting the unwanted columns shared between spreadsheets
  gorillaColumns <- c("Event Index","UTC Timestamp","UTC Date and Time","Local Timezone","Experiment ID","Experiment Version",
                      "Tree Node Key","Repeat Key","Schedule ID","Participant Private ID","Participant Starting Group",
                      "Participant Status","Participant Completion Code","Participant External Session ID",
                      "Participant Device Type","Participant Device","Participant OS","Participant Browser",
                      "Participant Monitor Size","Participant Viewport Size","Checkpoint","Room ID","Room Order","Task Version",
                      "checkpoint-ncck",	"checkpoint-x5ti",	"checkpoint-vh38",	"checkpoint-73pu",	"checkpoint-czdn",
                      "checkpoint-vx9c",	"checkpoint-vpap",	"branch-r1ry",	"branch-usxa",	"branch-r7nz",	"randomiser-spvu",
                      "checkpoint-ylq9",	"checkpoint-cppz",	"randomiser-3ddq","Spreadsheet Name","X Coordinate","Y Coordinate",
                      "Timed Out","display","d","Vocoded","Spreadsheet Row","Screen Number","Screen Name")
  # Setting the working path for data collection
  setwd(path)
  # Getting a list of all of the excel files
  files = list.files(full.names = T)
  # Getting rid of the ./
  files <- gsub(x = files, pattern = "./", replacement = "")
  # Getting the folder we need for the participant
  files <- files[grepl(participant[p], files)]
  # Writing the new path with the folder we just got
  path <- paste0(path,"/",files[1])
  # Setting the working directory to that
  setwd(path)
  # Getting a list of all of the folders
  files = list.files(full.names = T)
  # Getting rid of the ./
  files <- gsub(x = files, pattern = "./", replacement = "")
  # Getting the folder we need for the visit type
  files <- files[grepl(date[p], files)]
  # Writing our new path to the talker discrimination
  path <- paste0(path,"/",files[1],"/Gorilla Tasks/Vowel")
  # Setting the new working directory
  setwd(path)
  # Getting a list of all of the files
  files = list.files(full.names = T)
  # Getting rid of the ./
  files <- gsub(x = files, pattern = "./", replacement = "")
  # Getting the files that we need
  files <- files[!grepl("Scored", files)]
  
  
  
  
  
  
  
  
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
  files1 <- files1[grepl(date[p], files1)]
  # Writing our new path to the talker discrimination
  analysis <- paste0(analysis,"/",files1[1],"/Gorilla Tasks/Vowel")
  if(!dir.exists(analysis)){
    dir.create(analysis)
  }
  
  # Setting the working directory to the path
  setwd(path)
  # Getting a list of all of the excel files
  files = list.files(full.names = T)
  # Getting rid of the ./
  files <- gsub(x = files, pattern = "./", replacement = "")
  
  # Filtering out files we don't want
  files <- grep(paste("fk1l", collapse = '|'),
                files, value = TRUE, invert = TRUE)
  files <- grep(paste("gqks", collapse = '|'),
                files, value = TRUE, invert = TRUE)
  files <- grep(paste("vqlt", collapse = '|'),
                files, value = TRUE, invert = TRUE)
  files <- grep(paste("yt6f", collapse = '|'),
                files, value = TRUE, invert = TRUE)
  files <- grep(paste("Consonant", collapse = '|'),
                files, value = TRUE, invert = TRUE)
  
  # Import excel data
  Data <- read_excel(files[1])
  Data1 <- read_excel(files[2])
  
  Data1$`Reaction Time` <- as.double(Data1$`Reaction Time`)
  Data$`Reaction Time` <- as.double(Data$`Reaction Time`)
  
  # Binding rows
  Data2 <- bind_rows(Data1,Data)
  
  
  # Removing unwanted shared columns from all spreadsheets
  Data2 <- Data2[,!names(Data2) %in% gorillaColumns]
  
  # Actually removing the columns
  Data2 <- Data2 %>% filter(!grepl('AUDIO PLAY REQUESTED', Response))
  Data2 <- Data2 %>% filter(!is.na(Response))
  
  
  # Making list of vowels
  vowels <- c("AE","AH","EE","EH","ER","EY","I","OO","OW","UH","UU")
  
  # Counter
  total <- 0
  overallTotal <- 0
  count <- 0
  # Checking which vowel it is and summing
  for(v in 1:length(vowels)){
    # Adding the vowel total column
    for(i in 1:length(Data2$Vowel)){
      if(!is.na(Data2$Vowel[i])){
        if(Data2$Vowel[i] == vowels[v]){
          # Adding on value if it meets the condition
          total <- total + Data2$Correct[i]
          count <- count + 1
        }
      }
    }
    
    # Adding total to the corresponding section
    Data2[1,paste0(vowels[v], " Total")] <- (total/count)*100
    
    # Adding percentages to overall total
    overallTotal <- overallTotal + (total/count)*100
    
    # Resetting counter
    total <- 0
    count <- 0
  }
  
  # Getting grand total
  Data2[1,"Total"] <- overallTotal / 11
  
  
  # Changing writing directory into the vowel folder
  path2 <- paste0("C:/Users/hughm/OneDrive - Belmont University/Personal/Desktop/VUMC/R01R21/Vowel_Consonant/",participant[p],"/Vowel")
  setwd(analysis)
  
  # Writing the new excel sheet to the other folder
  write.xlsx(Data2, paste0(participant[p],"_",date,"_Vowel_Scored1.xlsx"),showNA = F)
  
  setwd(path)
  
  # Writing the new excel sheet to the other folder
  write.xlsx(Data2, paste0(participant[p],"_",date,"_Vowel_Scored1.xlsx"),showNA = F)
  setwd("C:/Users/hughm/OneDrive - Belmont University/Personal/Desktop")
  
}
