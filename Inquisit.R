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

# Clearing the console of previous junk
shell("cls")

# Clearing the environment of previous variables
rm(list=ls()) 

participant <- c("CI210")
date <- c("1 mo")
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
path <- paste0(path,"/",files[1],"/Inquisit Tasks/Digit Span")
# Setting the new working directory
setwd(path)

path <- c(path,gsub(x = path, pattern = "Digit Span", replacement = "Stroop"))

i = 1
p = 1
for(p in 1:2){
  setwd(path[p])
  files <- list.files(full.names = T)
  # Removing initial ./ in names
  files <- gsub(x = files, pattern = "./", replacement = "")
  # Renaming spreadsheets for organization based on the task name
  filesEx <- gsub(x = files, pattern = ".iqdat", replacement = ".xlsx")
  for(i in 1:length(files)){
    data <- read.table(paste0(path[p],"/",files[i]),fill = TRUE)
    write.xlsx(data, paste0(path[p],"/",filesEx[i]),showNA = F)
  }
}

