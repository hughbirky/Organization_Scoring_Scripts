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

participant <- c("CI222")
date <- c("1 mo")
calDate <- "07.22.2024"
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
path <- paste0(path,"/",files[1],"/Inquisit Tasks/Digit Span")



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
# Creating inquisit directory if necessary
if(!dir.exists(paste0(analysis,"/",files1[1],"/Inquisit Tasks"))){
  dir.create(paste0(analysis,"/",files1[1],"/Inquisit Tasks"))
}

# Creating stroop folder if needed
if(!dir.exists(paste0(analysis,"/",files1[1],"/Inquisit Tasks/Stroop"))){
  dir.create(paste0(analysis,"/",files1[1],"/Inquisit Tasks/Stroop"))
}

# Writing our new path to the talker discrimination
analysis <- paste0(analysis,"/",files1[1],"/Inquisit Tasks/Digit Span")

# Creating digit span folder if needed
if(!dir.exists(analysis)){
  dir.create(analysis)
}





# Setting the new working directory
setwd(path)
path <- c(path,gsub(x = path, pattern = "Digit Span", replacement = "Stroop"))
analysis <- c(analysis,gsub(x = analysis, pattern = "Digit Span", replacement = "Stroop"))



p = 1
i = 7
for(p in 1:2){
  setwd(path[p])
  files1 <- list.files(full.names = T)
  # Removing initial ./ in names
  files1 <- gsub(x = files1, pattern = "./", replacement = "")
  
  
    # Renaming spreadsheets for organization based on the task name
    filesEx <- gsub(x = files1, pattern = ".iqdat", replacement = ".xlsx")
    for(i in 1:length(files1)){
      # Reading in the data
      if(grepl("iqdat",files1[i])){
        data <- read.table(paste0(path[p],"/",files1[i]),fill = TRUE)
        if(length(files1[grepl(".xlsx", files1)]) == 0){
          # Rewriting the files as excel fiels
          write.xlsx(data, paste0(path[p],"/",filesEx[i]),showNA = F)
        }
        
        # If they are the summary files we want in analysis
        if(grepl("summary",files1[i])){
          # Writing a new one for backwards
          if(grepl("backw",files1[i])){
            # write.xlsx(data,paste0(analysis[p],"/",participant,"_",calDate,"_Backward Digit Span.xlsx"),showNA = F)
          } else if(grepl("forw",files1[i])){ # Writing a new one for forward
            # write.xlsx(data,paste0(analysis[p],"/",participant,"_",calDate,"_Forward Digit Span.xlsx"),showNA = F)
          } else{ # Writing a new one for stroop
            # write.xlsx(data,paste0(analysis[p],"/",participant,"_",calDate,"_Stroop.xlsx"),showNA = F)
          }
        }
      }
    }
}




