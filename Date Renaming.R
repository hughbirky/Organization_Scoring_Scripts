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



# Setting participant
participant <- "CI000"





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