library(tidyverse)
library(readxl)
library(Rcpp)
library(readxl)
library(writexl)
library(tidyverse)
library(dplyr)
library(purrr)
library(xlsx)


# Clearing the console of previous junk
shell("cls")
shell("clear")
# Clearing the environment of previous variables
rm(list=ls()) 

# Setting paths
pathPar <- "C:/Users/hughm/Dropbox/Cochlear_IIR_Project/Analysis/Data/Edited_Data/Participants"
pathPar <- paste(pathPar,"/",sep = "")
pathTask <- "C:/Users/hughm/Dropbox/Cochlear_IIR_Project/Analysis/Data/Edited_Data/Tasks"
pathTask <- paste(pathTask,"/",sep = "")



# Sets the working directory 
setwd(pathPar)

# Getting a list of all of the participants in the folder
participant = list.files(full.names = T)
# Removing initial ./ in names
participant <- gsub(x = participant, pattern = "./", replacement = "")

# Doing this for the second participant 
p = 2
setwd(paste(pathPar,participant[p],sep = ""))
# Getting a list of all of the tasks in the folder
tasks = list.files(full.names = T)
# Removing initial ./ in names
tasks <- gsub(x = tasks, pattern = "./", replacement = "")
# Removing folders that aren't needed
tasks <- grep(paste("Practice", collapse = '|'),
              tasks, value = TRUE, invert = TRUE)
tasks <- grep(paste("Sentence_", collapse = '|'),
              tasks, value = TRUE, invert = TRUE)
tasks <- grep(paste("Word_", collapse = '|'),
              tasks, value = TRUE, invert = TRUE)
tasks <- grep(paste("Scored", collapse = '|'),
              tasks, value = TRUE, invert = TRUE)
tasks <- grep(paste("tnt", collapse = '|'),
              tasks, value = TRUE, invert = TRUE)
tasks <- grep(paste("uploads", collapse = '|'),
              tasks, value = TRUE, invert = TRUE)
# Getting the names of the combined excels
taskname <- gsub(x = tasks, pattern = "edit_", replacement = "")
# Making the names match the names of the folders we will be putting the full scripts in
taskname <- gsub(x = taskname, pattern = paste(" ",participant[p],".xlsx",sep = ""), replacement = " Full")
taskname <- gsub(x = taskname, pattern = " ", replacement = "_")
# Making the names of the actual files
taskFile <- gsub(x = tasks, pattern = paste(" ",participant[p],".xlsx",sep = ""), replacement = "")
t = 2
p = 2
# Running this for every task
for(t in 1:length(tasks)){
  # Resetting the export dataframe
  # setwd(paste(pathTask,taskname[t],sep = ""))
  # Full <- read_excel(paste(taskname[t],".xlsx",sep = ""))
  Full <- ""
  # Runs this for each participant so we grab each of their tasks to combine
  for(p in 1:length(participant)){
    # Setting directory to the current participant
    setwd(paste(pathPar,participant[p],sep = ""))
    
    # Reading scored excel if it exists
    if(file.exists(paste(taskFile[t]," ",participant[p],".xlsx",sep = ""))){
      # Read excel
      scored <- read_excel(paste(taskFile[t]," ",participant[p],".xlsx",sep = ""))
      # If it's the first participant, it copys the data into the blank datafram
      # Otherwise, it concatenates the columns
      if(p > 1){Full <- rbind(Full,scored)}
      if(p == 1){Full <- scored}
    }
    else{
      
    }
  }
  
  # Export excel data
  write.xlsx(Full, paste(pathTask,taskname[t],"/",taskname[t],"_Scores.xlsx",sep = ""),showNA = F)
}