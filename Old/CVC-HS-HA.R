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
library(readr)



# Clearing the environment of previous variables
rm(list=ls())

# Clearing the console of previous junk
shell("cls")
participant <- c("CI222")
date <- c("1 mo")
calDate <- "07.22.2024"
task <- c("CVC","HS","HA")
# task <- c("CVC")


t = 1
for(t in 1:length(task)){
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
  files <- files[grepl(participant[1], files)]
  # Writing the new path with the folder we just got
  path <- paste0(path,"/",files[1])
  # Setting the working directory to that
  setwd(path)
  # Getting a list of all of the folders
  files = list.files(full.names = T)
  # Getting rid of the ./
  files <- gsub(x = files, pattern = "./", replacement = "")
  # Getting the folder we need for the visit type
  files <- files[grepl(date[1], files)]
  # Writing our new path to the talker discrimination
  # path <- paste0(path,"/",files[1],"/Matlab Tasks/Speech Recognition Testing ",task[t])
  path <- paste0(path,"/",files[1],"/Matlab Tasks/Speech Recognition Testing ",task[t])
  if(!dir.exists(path)){
    next
  }
  
  # Setting the new working directory
  setwd(path)
  # Getting a list of all of the files
  files = list.files(full.names = T)
  # Getting rid of the ./
  files <- gsub(x = files, pattern = "./", replacement = "")
  # Getting the files that we need
  files <- files[grepl(".xlsx", files)]
  files <- files[!grepl("Prep", files)]
  files <- files[!grepl(" ", files)]
  
  
  f = 3
  for(f in 1:length(files)){
    # Import excel data
    Data1 <- read_excel(files[f],col_names = FALSE)
    # Computing length of original column
    length <- nrow(Data1)
    
    # Creating new column for presentation order and renaming columns and ordering alphabetically
    Data1 <- mutate(Data1, `Presentation Number` = 1:length) %>%
      relocate(`Presentation Number`, .before = 1) %>%
      rename(
        File = ...1,
        List = ...2
      ) %>%
      arrange(File)
    
    # Getting the list name from the file string
    listName <- str_sub(Data1$File[1], start = 1, end = 6)
    listName <-gsub(x = listName, pattern = "-", replacement = "")
    
    # Getting template path
    templatePath <- paste0("C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Data Collection/Scoring Templates/",task[t])
    setwd(templatePath)
    
    # Getting a list of all of the folders
    templates <- list.files(full.names = T)
    # Getting rid of the ./
    templates <- gsub(x = templates, pattern = "./", replacement = "")
    # Getting the folder we need for the list type
    templates <- templates[grepl(listName, templates)]
    
    # Reading in the template
    template <- read_excel(templates[1])
    
    if(nrow(template) > length){
      Data1[nrow(Data1) + 1,] <- NA
      Data1[nrow(Data1) + 1,] <- NA
    } else if(length > nrow(template)){
      template[nrow(template) + 1,] <- NA
      template[nrow(template) + 1,] <- NA
    }
    
    # Changing the presentation order
    template$`Presentation Number` <- Data1$`Presentation Number`
    
    if(nrow(template) == 20){
      template[nrow(template) + 1,] <- NA
      template[nrow(template) + 1,] <- NA
    }
    # Rearranging by the presentation order
    # template <- template %>% arrange(`Presentation Number`)
    
    
    # Adding possible scoring?
    if(task[t] == "CVC"){
      template$`# phonemes correct`[22] <- "=AVERAGE(O2:O21)"
      template$`word correct`[22] <- "=AVERAGE(P2:P21)" 
    }
    if(task[t] == "HA"){
      # template$`# Words Correct`[22] <- "=AVERAGE(SUM(R[-21]C:R[-2]C)/SUM(R[-21]C[-1]:R[-2]C[-1]))"
      # template$`# Keywords Correct`[22] <- "=AVERAGE(SUM(R[-21]C:R[-2]C)/SUM(R[-21]C[-1]:R[-2]C[-1]))"
      template$`# Words Correct`[22] <- "=AVERAGE(SUM(K2:K21)/SUM(J2:J21))"
      template$`# Keywords Correct`[22] <- "=AVERAGE(SUM(M2:M21)/SUM(L2:L21))"
    }
    if(task[t] == "HS"){
      # template$`# Words Correct`[22] <- "=AVERAGE(SUM(R[-21]C:R[-2]C)/SUM(R[-21]C[-1]:R[-2]C[-1]))"
      # template$`# Keywords Correct`[22] <- "=AVERAGE(SUM(R[-21]C:R[-2]C)/SUM(R[-21]C[-1]:R[-2]C[-1]))" 
      template$`# Words Correct`[22] <- "=AVERAGE(SUM(K2:K21)/SUM(J2:J21))"
      template$`# Keywords Correct`[22] <- "=AVERAGE(SUM(M2:M21)/SUM(L2:L21))" 
    }
    
    
    setwd(path)
    
    rownames(template) <- NULL
    # Writing the new excel sheet to the other folder
    # write.xlsx(x = template,file = paste0(task[t],"_",listName,"_",participant,"_",date,"_Prepped.xlsx"), sheetName = "Scorer 1",showNA = F)
    # write.xlsx(x = template,file = paste0(task[t],"_",listName,"_",participant,"_",date,"_Prepped.xlsx"), sheetName = "Scorer 2",append = TRUE,showNA = F)
    # 
    write.xlsx(x = template,file = paste0(participant,"_",calDate,"_",task[t],"_",listName,".xlsx"), sheetName = "Scorer 1",showNA = F)
    write.xlsx(x = template,file = paste0(participant,"_",calDate,"_",task[t],"_",listName,".xlsx"), sheetName = "Scorer 2",append = TRUE,showNA = F)
    
  }
  
  
  
}
