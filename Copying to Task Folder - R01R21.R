library(tidyverse)
library(readxl)
library(Rcpp)
library(readxl)
library(writexl)
library(tidyverse)
library(dplyr)
library(purrr)
library(xlsx)



shell("cls")
# shell("clear")
# Clearing the environment of previous variables
rm(list=ls()) 


# participant <- c("CI200","CI201","CI202","CI203","CI204","CI205","CI207","CI208","CI209","CI210","CI211","CI213","CI214","CI215","CI216","CI216","CI217")
# participant <- "CI212"
participant <- "CI216"
# date <- c("preop")
# date <- c("1 mo")
date <- c("3 mo")
# date <- c("6 mo")

tasky <- 10

move_to_analysis <- T

# Finding who's computer we are on
origin <- "C:/Users"
# Setting the working path for data collection
setwd(origin)
# Getting a list of all of the excel files
source = list.files(full.names = T)
# Getting rid of the ./
source <- gsub(x = source, pattern = "./", replacement = "")
# Getting the folder we need for the participant
source <- source[grepl("hughm", source)]



p = 1

for(p in 1:length(participant)){
  # List of Tasks to be moved
  tasks <- c("/Gorilla Tasks/Talker Discrimination"
  )
  # tasks <- c("/Matlab Tasks/Lexical Decision Task",
  #            "/Matlab Tasks/Ravens",
  #            "/Matlab Tasks/Retroactive Priming",
  #            "/Matlab Tasks/Rhyme Judgment Task",
  #            "/Matlab Tasks/Semantic Priming",
  #            "/Matlab Tasks/Sentence Verification Task",
  #            "/Matlab Tasks/Speech Recognition Testing CVC",
  #            "/Matlab Tasks/Speech Recognition Testing HA",
  #            "/Matlab Tasks/Speech Recognition Testing HS",
  #            "/Matlab Tasks/Speech Recognition Testing PRESTO",
  #            "/Gorilla Tasks/Consonant",
  #            "/Gorilla Tasks/Vowel",
  #            "/Gorilla Tasks/Nonword",
  #            "/Gorilla Tasks/Talker Discrimination",
  #            "/Gorilla Tasks/MLST",
  #            "/Inquisit Tasks/Stroop",
  #            "/Inquisit Tasks/Digit Span"
  # )
  
  
  tasky = 1
  for(tasky in 1:length(tasks)){
    if(source == "hughm"){
      path <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Data Collection/Subject testing/Cochlear Implant"
      analysis <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Analysis/Scoring/Completed scoring"
    } else{
      path <- "f"
    }
    
    
    tasksAlone <- gsub(x = tasks[tasky],pattern = "/Matlab Tasks",replacement = "")
    tasksAlone <- gsub(x = tasksAlone,pattern = "/Gorilla Tasks",replacement = "")
    tasksAlone <- gsub(x = tasksAlone,pattern = "/Inquisit Tasks",replacement = "")
    
    
    
  
  
  
    # Setting the working path for analysis scoring
    setwd(analysis)
    # Getting a list of all of the excel files
    partic = list.files(full.names = T)
    # Getting rid of the ./
    partic <- gsub(x = partic, pattern = "./", replacement = "")
    # Getting the folder we need for the participant[p]
    partic <- partic[grepl(participant[p], partic)]
    # Writing the new path with the folder we just got
    analysis <- paste0(analysis,"/",partic[1])
    # Setting the working directory to that
    setwd(analysis)
    # Getting a list of all of the folders
    dat = list.files(full.names = T)
    # Getting rid of the ./
    dat <- gsub(x = dat, pattern = "./", replacement = "")
    # Getting the folder we need for the visit type
    dat <- dat[grepl(date, dat)]
    # Writing our new path to the task
    analysis <- paste0(analysis,"/",dat[1],tasks[tasky])
    # Checking if they actually did this or it's been scored yet
    if(dir.exists(analysis)){
      setwd(analysis) 
    } else if (dir.exists(paste0(analysis," Task"))){
      analysis <- paste0(analysis," Task")
      setwd(analysis)
    } else if (dir.exists(paste0(analysis,"Task"))){
      analysis <- paste0(analysis,"Task")
      setwd(analysis)
    } else if (dir.exists(gsub(x = gsub(x = analysis, pattern = " Task", replacement = ""), pattern = "Matlabs", replacement = "Matlab Tasks")) && !dir.exists(analysis)){
      analysis <- gsub(x = gsub(x = analysis, pattern = " Task", replacement = ""), pattern = "Matlabs", replacement = "Matlab Tasks")
      setwd(analysis)
    } else if (dir.exists(gsub(x = gsub(x = analysis, pattern = " Task", replacement = ""), pattern = "Inquisits", replacement = "Inquisit Tasks")) && !dir.exists(analysis)){
      analysis <- gsub(x = gsub(x = analysis, pattern = " Task", replacement = ""), pattern = "Inquisits", replacement = "Inquisit Tasks")
      setwd(analysis)
    } else if (dir.exists(gsub(x = gsub(x = analysis, pattern = " Task", replacement = ""), pattern = "Gorillas", replacement = "Gorilla Tasks")) && !dir.exists(analysis)){
      analysis <- gsub(x = gsub(x = analysis, pattern = " Task", replacement = ""), pattern = "Gorillas", replacement = "Gorilla Tasks")
      setwd(analysis)
    } else{
      # Otherwise continue
      next
    }
    
    # Getting a list of all of the excel files
    files1 = list.files(full.names = T)
    # Getting rid of the ./
    files1 <- gsub(x = files1, pattern = "./", replacement = "")
    # Removing directories
    files1 <- files1[!grepl("Old",files1)]
    
    
    
    
    # Setting the path to the task oriented area
    taskPath <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Analysis/Scoring/Tasks Analysis"
    
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
    
  
    
    
    
    
    
    
    setwd(taskPath)
    # Getting a list of all of the excel files in the tasks folder
    files2 = list.files(full.names = T)
    # Getting rid of the ./
    files2 <- gsub(x = files2, pattern = "./", replacement = "")
    # Checking to see if there is a CI of the current participant
    files2 <- files2[grepl(participant[p],files2)]

    
    
    # Running this for all present files
    f = 1
    for(f in 1:length(files1)){
      setwd(taskPath)
      if(!file.exists(files1[f])){
        setwd(analysis)
        file.copy(from = files1[f], to = paste0(taskPath,"/",files1[f]))
      }
    }
  }
}
