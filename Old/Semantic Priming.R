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
participant <- c("CI200")
date <- c("12 mo")
calDate <- "07.30.2024"
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
path <- paste0(path,"/",files[1],"/Matlab Tasks/Semantic Priming")


# Copying files over
file.copy(from = "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Data Collection/Scoring Templates/Semantic Priming/Semantic_Priming Scoring Template.xlsx", to = paste0(path,"/",participant,"_",calDate,"_Semantic.xlsx"))



setwd("C:/Users/hughm")
