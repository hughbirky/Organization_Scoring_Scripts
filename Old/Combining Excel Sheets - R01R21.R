library(tidyverse)
library(readxl)
library(Rcpp)
library(readxl)
library(writexl)
library(tidyverse)
library(dplyr)
library(plyr)
library(purrr)
library(xlsx)




shell("cls")
# shell("clear")
# Clearing the environment of previous variables
rm(list=ls()) 

# date <- c("preop")
date <- c("1 mo")
# date <- c("3 mo")
# date <- c("6 mo")
# date <- c("12 mo")

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

# Setting the path to the task oriented area
taskPath <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Analysis/Scoring/Tasks Analysis"


# List of Tasks to be moved
# tasks <- c("/Matlab Tasks/Lexical Decision Task")
# tasks <- c("/Matlab Tasks/Ravens")
# tasks <- c("/Matlab Tasks/Retroactive Priming")
# tasks <- c("/Matlab Tasks/Rhyme Judgment Task")
# tasks <- c("/Matlab Tasks/Semantic Priming")
# tasks <- c("/Matlab Tasks/Sentence Verification Task")
# tasks <- c("/Matlab Tasks/Speech Recognition Testing CVC")
# tasks <- c("/Matlab Tasks/Speech Recognition Testing HA")
# tasks <- c("/Matlab Tasks/Speech Recognition Testing HS")
# tasks <- c("/Matlab Tasks/Speech Recognition Testing PRESTO")
# tasks <- c("/Gorilla Tasks/Consonant")
# tasks <- c("/Gorilla Tasks/Vowel")
# tasks <- c("/Gorilla Tasks/Nonword")
tasks <- c("/Gorilla Tasks/Talker Discrimination")
# tasks <- c("/Gorilla Tasks/MLST")
# tasks <- c("/Inquisit Tasks/Stroop")
# tasks <- c("/Inquisit Tasks/Digit Span")
           

tasky <- 1

# Getting rid of the folder for new oragnization style
tasksAlone <- gsub(x = tasks[tasky],pattern = "/Matlab Tasks",replacement = "")
tasksAlone <- gsub(x = tasksAlone,pattern = "/Gorilla Tasks",replacement = "")
tasksAlone <- gsub(x = tasksAlone,pattern = "/Inquisit Tasks",replacement = "")

# Setting the working path for the task folder
setwd(taskPath)
# Getting a list of all of the files
task_folder = list.files(full.names = T)
# Getting rid of the ./
task_folder <- gsub(x = task_folder, pattern = "./", replacement = "")
# Getting the task we are working on
taskPath <- paste0(taskPath,tasksAlone)
# Setting the working path for the task folder
setwd(taskPath)
# Getting a list of all of the files
task_folder = list.files(full.names = T)
# Getting rid of the ./
task_folder <- gsub(x = task_folder, pattern = "./", replacement = "")
# Getting the folder we need for the participant[p]
task_folder <- task_folder[grepl(date, task_folder)]
# Writing our new path to the task
taskPath <- paste0(taskPath,"/",task_folder[1])



# Setting the working directory to the taskPath
setwd(taskPath)
# Getting a list of all of the excel files
full_name = list.files(full.names = T)
# Getting rid of the ./
full_name <- gsub(x = full_name, pattern = "./", replacement = "")
# Only getting the template name
full_name <- full_name[grepl("Combined", full_name)]

# Reading in the full file name
full <- ""

# Getting the rest of the files
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")
# Excluding full combined
files <- files[!grepl("Combined", files)]
files <- files[!grepl("Old", files)]

i = 2
if(grepl("Talker",tasks[tasky])){
  files <- files[grepl("Best_",files)]
}

for(i in 1:length(files)){
  scored <- read_excel(files[i])
  
  if("Acc 1 or 0" %in% colnames(scored)){
    scored <- scored %>%
      dplyr::rename("Correct" = "Acc 1 or 0")
  }
  
  if(i == 1){
    full <- scored
  } else{
    
    # full <- bind_rows(full,map2_df(scored, map(full,class), ~{class(.x) <- .y;.x}))
    full <- rbind.fill(full,scored)
  }
}

# Doing this for CI in Talker disc
if(grepl("Talker",tasks[tasky])){
  # Getting the rest of the files
  files = list.files(full.names = T)
  # Getting rid of the ./
  files <- gsub(x = files, pattern = "./", replacement = "")
  # Excluding full combined
  files <- files[!grepl("Combined", files)]
  files <- files[!grepl("Old", files)]
  
  full2 <- ""
  files <- files[grepl("CI_",files)]
  if(length(files) > 0){
    for(i in 1:length(files)){
      scored <- read_excel(files[i])
      if(i == 1){
        full2 <- scored
      } else{
        # full2 <- bind_rows(full2,map2_df(scored, map(full2,class), ~{class(.x) <- .y;.x}))
        full2 <- rbind.fill(full2,scored)
        
      }
    }
    write.xlsx(full2,"Combined_CI_Only_Scoring.xlsx",showNA = F)
  }
}

# Export excel data
write.xlsx(full,"Combined_Scoring.xlsx",showNA = F)