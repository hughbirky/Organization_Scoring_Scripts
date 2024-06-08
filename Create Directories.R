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

participant <- c("CI216")
name <- c("Roper")
# date <- c("preop")
# date <- c("1 month")
date <- c("3 month")
# date <- c("6 month")
calDate <- "06.06.2024"
surgeryDate <- ""
isCochlear <- T
travel <- T




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


# Creating the folder if it doesn't exist
if(length(files) == 0){
  path <- paste0(path,"/",participant,"- ",name)
  dir.create(path)
} else {
  # Writing the new path with the folder we just got
  path <- paste0(path,"/",files[1])
}



# Setting the working directory to that
setwd(path)


# Creating payment forms and real ear folders if doesn't exist
if(!dir.exists(paste0(path,"/Payment forms"))){
  dir.create(paste0(path,"/Payment forms"))
  if(date == "preop"){
    if(travel){
      file.copy(from = "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Data Collection/Subject payment forms/Template_payment_TRAVEL_form_R01.xlsx", to = paste0(path,"/Payment forms/Template_payment_TRAVEL_form_R01.xlsx"))
      file.copy(from = "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Data Collection/Subject payment forms/Template_payment_TRAVEL_form_R21.xlsx", to = paste0(path,"/Payment forms/Template_payment_TRAVEL_form_R21.xlsx"))
    } else{
      file.copy(from = "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Data Collection/Subject payment forms/Template_payment_form_R01.xlsx", to = paste0(path,"/Payment forms/Template_payment_form_R01.xlsx"))
      file.copy(from = "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Data Collection/Subject payment forms/Template_payment_form_R21.xlsx", to = paste0(path,"/Payment forms/Template_payment_form_R21.xlsx"))
    }
  }
    
  
}
if(!dir.exists(paste0(path,"/REALEAR"))){
  dir.create(paste0(path,"/REALEAR"))
}
if(!dir.exists(paste0(path,"/",surgeryDate,"- surgery")) && surgeryDate != ""){
  dir.create(paste0(path,"/",surgeryDate,"- surgery"))
}



# Getting a list of all of the folders
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")
# Getting the folder we need for the visit type
files <- files[grepl(date, files)]

# Creating the folder if it doesn't exist
if(length(files) == 0){
  path <- paste0(path,"/",calDate,"- ",date)
  dir.create(path)
} else{
  path <- paste0(path,"/",files[1])
}



# Setting working directory
setwd(path)


if(isCochlear && date == "1 mo"){
  if(!dir.exists(paste0(path,"/Ecochg"))){
    dir.create(paste0(path,"/Ecochg"))
  }
}

if(!dir.exists(paste0(path,"/Matlab Tasks"))){
  dir.create(paste0(path,"/Matlab Tasks"))
}


if(!dir.exists(paste0(path,"/Gorilla Tasks"))){
  dir.create(paste0(path,"/Gorilla Tasks"))
  
  
  # Creating Gorilla folders
  setwd(paste0(path,"/Gorilla Tasks"))
  dir.create("Consonant")
  dir.create("Vowel")
  dir.create("Nonword")
  dir.create("Talker Discrimination")
}

if(!dir.exists(paste0(path,"/Screeners"))){
  dir.create(paste0(path,"/Screeners"))
  dir.create(paste0(path,"/Screeners/Rainbow"))
}

if(!dir.exists(paste0(path,"/Inquisit Tasks"))){
  dir.create(paste0(path,"/Inquisit Tasks"))
  # Creating stroop folder
  setwd(paste0(path,"/Inquisit Tasks"))
  dir.create("Stroop")
}


if(!dir.exists(paste0(path,"/Wav Files"))){
  dir.create(paste0(path,"/Wav Files"))
  dir.create(paste0(path,"/Wav Files/Backup"))
}



















# Setting the working path for analysis scoring
setwd(analysis)
# Getting a list of all of the excel files
files1 = list.files(full.names = T)
# Getting rid of the ./
files1 <- gsub(x = files1, pattern = "./", replacement = "")
# Getting the folder we need for the participant[p]
files1 <- files1[grepl(participant, files1)]

# Creating the folder if it doesn't exist
if(length(files1) == 0){
  analysis <- paste0(analysis,"/",participant,"- ",name)
  dir.create(analysis)
} else {
  # Writing the new path with the folder we just got
  analysis <- paste0(analysis,"/",files1[1])
}



# Setting the working directory to that
setwd(analysis)




# Getting a list of all of the folders
files1 = list.files(full.names = T)
# Getting rid of the ./
files1 <- gsub(x = files1, pattern = "./", replacement = "")
# Getting the folder we need for the visit type
files1 <- files1[grepl(date, files1)]


# Creating the folder if it doesn't exist
if(length(files1) == 0){
  analysis <- paste0(analysis,"/",calDate,"- ",date)
  dir.create(analysis)
  
  setwd(analysis)
  
  # Creating Gorilla folders
  dir.create("Gorilla Tasks")
  dir.create("Matlab Tasks")
  
} else{
  analysis <- paste0(analysis,"/",files1[1])
}








