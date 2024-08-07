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


# Clearing the console of previous junk
shell("cls")
# shell("clear")
# Clearing the environment of previous variables
rm(list=ls()) 

participant <- c("CI223")
date <- c("preop")
# date <- c("1 mo")
# date <- c("3 mo")
# date <- c("6 mo")
taskpath <- "Gorilla Tasks/Talker Discrimination"


p = 1
d = 1
# for(p in 1:length(participant)){
#   for(d in 1:length(date)){
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
    files <- files[grepl(participant[p], files)]
    
    
    # Creating the folder if it doesn't exist
    if(length(files) == 0){
      next
    } else {
      # Writing the new path with the folder we just got
      path <- paste0(path,"/",files[1])
    }
    
    
    # setting working directory to new path
    setwd(path)
    # Getting a list of all of the excel files
    files = list.files(full.names = T)
    # Getting rid of the ./
    files <- gsub(x = files, pattern = "./", replacement = "")
    # Getting the folder we need for the participant
    files <- files[grepl(date[d], files)]
    
    
    # Creating the folder if it doesn't exist
    if(length(files) == 0){
      next
    } else {
      # Writing the new path with the folder we just got
      path <- paste0(path,"/",files[1],"/",taskpath)
    }
    
    calDate <- substr(files[1], 1,10)
    calDate <- gsub(x = calDate, pattern = " ", replacement = "")
    calDate <- gsub(x = calDate, pattern = "-", replacement = "")
    
    
    # Getting path to the task name
    setwd(path)
    # Getting a list of all of the excel files
    files = list.files(full.names = T)
    # Getting rid of the ./
    files <- gsub(x = files, pattern = "./", replacement = "")
    
    # Removing the stuff that we want
    files <- files[!grepl("Old", files)]
    files_full <- files
    files <- files[!grepl(paste0(participant,"_",calDate,"_Talker"), files)]
    
    # Checking if they exist
    if(length(files) == 0){
      next
    } 
    
    # Creating the old folder if it doesn't exist
    if(!dir.exists("Old")){
      dir.create("Old")
    }
    
    f = 1
    # If it has the Gender condition, move to the old folder
    for(f in 1:length(files)){
      if(grepl("Gender",files[f])){
        file.copy(files[f],paste0(path,"/Old"))
        if(!file.copy(files[f],paste0(path,"/Old"))){
          file.remove(files[f])
        } 
      }
    }
    
    files = list.files(full.names = T)
    # Getting rid of the ./
    files <- gsub(x = files, pattern = "./", replacement = "")
    
    # Removing the stuff that we want
    files <- files[!grepl("Old", files)]
    files <- files[!grepl(paste0(participant,"_",calDate,"_Talker"), files)]
    
    
    
    f = 1
    for(f in 1:length(files)){
      # If it only has best Aided
      if(any(grepl("CI Only",files),grepl("CI_Only",files))){
        # Changing the scored first
        if(grepl("Scored",files[f])){
          # If it is best aided or not
          if(grepl("Best",files[f])){
            file.rename(files[f],paste0(participant,"_",calDate,"_Talker Discrimination_BA_Scored.xlsx"))
          } else if(grepl("CI_Only",files[f]) || grepl("CI Only",files[f])) {
            file.rename(files[f],paste0(participant,"_",calDate,"_Talker Discrimination_CI_Scored.xlsx"))
          }
        } else { # Doing the non scored next
          # For CI Only Condition
          if(!any(grepl("CI_Only.",files)) && !any(grepl("CI Only.",files))){
            if(grepl("data_exp",files[f]) || grepl("CI_Only",files[f]) || grepl("CI Only",files[f])){
              file.rename(files[f],paste0(participant,"_",calDate,"_Talker Discrimination_CI.xlsx"))
            }
            # For best aided condition
            if(grepl("Best Aided",files[f]) || grepl("Best_Aided",files[f])){
              file.rename(files[f],paste0(participant,"_",calDate,"_Talker Discrimination_BA.xlsx"))
            }
          } else {
            if(grepl("CI_Only",files[f]) || grepl("CI Only",files[f])){
              file.rename(files[f],paste0(participant,"_",calDate,"_Talker Discrimination_CI.xlsx"))
            }
            # For best aided condition
            if(grepl("data_exp",files[f]) || grepl("Best Aided",files[f]) || grepl("Best_Aided",files[f])){
              file.rename(files[f],paste0(participant,"_",calDate,"_Talker Discrimination_BA.xlsx"))
            }
          }
          
            
        }
      } else{ # If no CI
        # Doing scored first
        if(grepl("Scored",files[f])){
          file.rename(files[f],paste0(participant,"_",calDate,"_Talker Discrimination_BA_Scored.xlsx"))
        } else { # Non scored
          if(grepl("data_exp",files[f]) || grepl("Best",files[f])){
            file.rename(files[f],paste0(participant,"_",calDate,"_Talker Discrimination_BA.xlsx"))
          }
        }
      }
    }
      
      
      
      
    #   if(length(files) < 4){
    #     if(grepl("data_exp",files[f]) && !file.exists(paste0(participant,"_",calDate,"_Talker Discrimination_BA.xlsx"))){
    #       # Rename to best aided
    #       file.rename(files[f],paste0(participant,"_",calDate,"_Talker Discrimination_BA.xlsx"))
    #     } else if(grepl("Multiple",files[f]) || grepl("Scored",files[f])){ # Renaming the multiple scored ones
    #       if(grepl("Best",files[f]) || grepl("Scored",files[f])){
    #         file.rename(files[f],paste0(participant,"_",calDate,"_Talker Discrimination_BA_Scored.xlsx"))
    #       } else if(grepl("CI_Only",files[f])){
    #         file.rename(files[f],paste0(participant,"_",calDate,"_Talker Discrimination_CI_Only_Scored.xlsx"))
    #       }
    #     }
    #   } else if(grepl("Best Aided.",files[f]) || grepl("Best_Aided.",files[f]) || grepl("BA.",files[f])){
    #     file.rename(files[f],paste0(participant,"_",calDate,"_Talker Discrimination_BA.xlsx"))
    #   } else if(grepl("Best_Aided_Scored",files[f])){
    #     file.rename(files[f],paste0(participant,"_",calDate,"_Talker Discrimination_BA_Scored.xlsx"))
    #   } else if(grepl("CI_Only.",files[f]) || grepl("CI Only.",files[f])){
    #     file.rename(files[f],paste0(participant,"_",calDate,"_Talker Discrimination_CI.xlsx"))
    #   } else if(grepl("CI_Only_",files[f]) || grepl("CI Only_",files[f])){
    #     file.rename(files[f],paste0(participant,"_",calDate,"_Talker Discrimination_CI_Scored.xlsx"))
    #   }
    #   
    # }
    
    
#   }
# }
