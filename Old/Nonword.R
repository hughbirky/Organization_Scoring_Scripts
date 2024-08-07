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

done <- ""

p = 1
d = 1
for(p in 1:length(participant)){
  for(d in 1:length(date)){
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
    
    # Setting the working directory
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
    # Writing the new path with the folder we just got
    path <- paste0(path,"/",files[1])
    # Setting the working directory to that
    setwd(path)
    # Getting a list of all of the folders
    files = list.files(full.names = T)
    # Getting rid of the ./
    files <- gsub(x = files, pattern = "./", replacement = "")
    # Getting the folder we need for the visit type
    files <- files[grepl(date[d], files)]
    # Writing our new path to the talker discrimination
    path <- paste0(path,"/",files[1],"/Gorilla Tasks/Nonword/uploads")
    # Setting the new working directory
    if(!dir.exists(path)){
      next
    } else{
      done <- c(done,paste0(participant[p],"/",date[d]))
    }
    
    setwd(path)
    # Getting a list of all of the excel files
    files = list.files(full.names = T)
    # Getting rid of the ./
    files <- gsub(x = files, pattern = "./", replacement = "")
    
    
    # Creating storage directory
    dir.create("Wav_Files")
    
    # Creating our output path for our files
    outputPathCopy <- "C:/Users/hughm/Desktop/Scoring"
    
    
    # Copying our files to a folder that the command prompt can access
    for(i in 1:length(files)){
      file.copy(from = files[i], to = paste0(outputPathCopy,"/",files[i]))
    }
    
    # Setting the working directory to our output path
    setwd(paste0(outputPathCopy))
    
    # Converting every file
    for(i in 1:length(files)){
      # Creating a string of the path to the file to be converted
      input <- paste0(outputPathCopy,"/",files[i])
      # Creating a string of the path to where we want to export
      outputpath <- paste0(outputPathCopy,"/Wav_Files/", gsub(x = files[i], pattern = ".weba", replacement = ".wav"))
      # Creating command for command prompt and executing
      cmd <- sprintf("ffmpeg -i %s -vn %s", input, outputpath)
      # Calling the system command
      system(cmd)
    }
    
    # Setting the directory to the wav files directory 
    setwd(paste0(outputPathCopy, "/Wav_Files"))
    
    
    # Getting a list of all of the files
    files = list.files(full.names = T)
    # Getting rid of the ./
    files <- gsub(x = files, pattern = "./", replacement = "")
    
    # Copying the files to the original wav files folder and then deleting the file
    for(i in 1:length(files)){
      file.copy(from = files[i], to = paste0(path,"/Wav_Files/",files[i]))
      unlink(files[i])
    }
    
    # Setting working directory to original output path
    setwd(outputPathCopy)
    # Getting a list of all of the excel files
    files = list.files(full.names = T)
    # Getting rid of the ./
    files <- gsub(x = files, pattern = "./", replacement = "")
    
    # Deleting the rest of the files
    for(i in 1:length(files)){
      unlink(files[i])
    }
  }
}



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
# Writing our new path to the nonword
path <- paste0(path,"/",files[1],"/Gorilla Tasks/Nonword")


# Copying files over
file.copy(from = "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Data Collection/Scoring Templates/Nonword Recognition/NWR score sheet integrity check.xlsx", to = paste0(path,"/",participant,"_",calDate,"_Nonword.xlsx"))



setwd("C:/Users/hughm")
