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



# Clearing the console of previous junk
shell("cls")

# Clearing the environment of previous variables
rm(list=ls())


participant <- c("CI216")
date <- c("preop")
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
task <- c(NA,NA)
 
for(d in 1:length(date)){
  p = 1
  d = 1

  analysis <- "C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Analysis/Scoring/Completed scoring"
  # Setting the unwanted columns shared between spreadsheets
  gorillaColumns <- c("Event Index","UTC Timestamp","UTC Date and Time","Local Timezone","Experiment ID","Experiment Version",
                      "Tree Node Key","Repeat Key","Schedule ID","Participant Private ID","Participant Starting Group",
                      "Participant Status","Participant Completion Code","Participant External Session ID",
                      "Participant Device Type","Participant Device","Participant OS","Participant Browser",
                      "Participant Monitor Size","Participant Viewport Size","Checkpoint","Room ID","Room Order","Task Version",
                      "checkpoint-ncck",	"checkpoint-x5ti",	"checkpoint-vh38",	"checkpoint-73pu",	"checkpoint-czdn",
                      "checkpoint-vx9c",	"checkpoint-vpap",	"branch-r1ry",	"branch-usxa",	"branch-r7nz",	"randomiser-spvu",
                      "checkpoint-ylq9",	"checkpoint-cppz",	"randomiser-3ddq","Spreadsheet Name","X Coordinate","Y Coordinate",
                      "Timed Out","display","d","Vocoded","Spreadsheet Row","Screen Number","Screen Name")
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
  path <- paste0(path,"/",files[1],"/Gorilla Tasks/Talker Discrimination")
  # Setting the new working directory
  setwd(path)
  # Getting a list of all of the files
  files = list.files(full.names = T)
  # Getting rid of the ./
  files <- gsub(x = files, pattern = "./", replacement = "")
  # Getting the files that we need
  files <- files[!grepl("Scored", files)]
  
  
  
  
  
  
  
  
  # Setting the working path for analysis scoring
  setwd(analysis)
  # Getting a list of all of the excel files
  files1 = list.files(full.names = T)
  # Getting rid of the ./
  files1 <- gsub(x = files1, pattern = "./", replacement = "")
  # Getting the folder we need for the participant[p]
  files1 <- files1[grepl(participant[p], files1)]
  # Writing the new path with the folder we just got
  analysis <- paste0(analysis,"/",files1[1])
  # Setting the working directory to that
  setwd(analysis)
  # Getting a list of all of the folders
  files1 = list.files(full.names = T)
  # Getting rid of the ./
  files1 <- gsub(x = files1, pattern = "./", replacement = "")
  # Getting the folder we need for the visit type
  files1 <- files1[grepl(date[d], files1)]
  # Writing our new path to the talker discrimination
  analysis <- paste0(analysis,"/",files1[1],"/Gorilla Tasks/Talker Discrimination")
  if(!dir.exists(analysis)){
    dir.create(analysis)
  }
  # Setting the new working directory for back to data collection
  setwd(path)
  
  
  
  
  
  
  
  
  # Renaming files
  if(length(files) > 1){
    if(grepl("Best",files[1])){
      task[1] <- "Best_Aided"
      task[2] <- "CI_Only"
    } else if(grepl("CI",files[1])){
      task[1] <- "CI_Only"
      task[2] <- "Best_Aided"
    } else if(grepl("Best",files[2])){
      task[2] <- "Best_Aided"
      task[1] <- "CI_Only"
    } else if(grepl("CI",files[2])){
      task[2] <- "CI_Only"
      task[1] <- "Best_Aided"
    } 
  } else {
    task[1] <- "Best_Aided"
  }
  
  
  
  
  
  
  
  
  f= 1
  for(f in 1:length(files)){
    
    setwd(path)
    
    # Getting the files that we need
    files <- files[!grepl("Scored", files)]
    
    # Import excel data
    Data1 <- read_excel(files[f])
    # Removing unwanted shared columns from all spreadsheets
    Data2 <- Data1[,!names(Data1) %in% gorillaColumns]
    
    # Actually removing the columns
    Data2 <- Data2 %>% filter(!grepl('AUDIO|ADJUSTED', Response))
    Data2 <- Data2 %>% filter(!is.na(Response))
    # Renaming Response and Attempt Columns
    Data2 <- Data2 %>% rename("Rating" = "Attempt")
    # Shifting Rating Column by 1
    Data2$Rating <- lead(Data2$Response, 1, default = "")
    # Filtering extra rows
    Data2 <- Data2 %>% filter(grepl('Same Talker|Different Talker', Response))
    # Reusing the attemp column
    Data2 <- Data2 %>% rename("TalkerAnswer" = "Incorrect")
    # Copying data to 
    Data2$TalkerAnswer <- Data2$Response
    # Turing region names into numbers
    Data2$TalkerAnswer<- gsub(x = Data2$TalkerAnswer,pattern = "Same Talker",replacement = "ST")
    Data2$TalkerAnswer<- gsub(x = Data2$TalkerAnswer,pattern = "Different Talker",replacement = "DT")
    
    
    Data2$Rating<- gsub(x = Data2$Rating,pattern = "7 (Very different)",replacement = 7,fixed = TRUE)
    Data2$Rating<- gsub(x = Data2$Rating,pattern = "1 (Very similar)",replacement = 1,fixed = TRUE)
    
    # Adding Gender Columns
    Data2$Gender1 <- NA
    Data2$Gender2 <- NA
    Data2$GenderCondition <- NA
    
    
    # Adding Average Columns
    Data2$AvgCorrect <- NA
    Data2$AvgRT <- NA
    Data2$AvgRating <- NA
    
    
    # Adding Gender Scoring Columns
    Data2$ST_Score <- NA
    Data2$ST_RT <- NA
    Data2$ST_Rating <- NA
    Data2$DTSG_Score <- NA
    Data2$DTSG_RT <- NA
    Data2$DTSG_Rating <- NA
    Data2$DTMG_Score <- NA
    Data2$DTMG_RT <- NA
    Data2$DTMG_Rating <- NA

    
    # Changing Correct Column
    for(z in 1:length(Data2$TalkerAnswer)){
      if(!is.na(Data2$ANSWER[z])){
        if(Data2$TalkerAnswer[z] == Data2$ANSWER[z]){
          Data2$Correct[z] <- 1
        }
      }
    }
    
    # Swapping Talker1 and Talker2 due to errors in the original spreadsheet
    for(t in 1:length(Data2$Talker1)){
      # Taking the talker name from the stimulus
      Data2$Talker1[t] <- Data2$Stimulus1[t] %>% substr(start = 1, stop = 3) %>%
        gsub(pattern = "_", replacement = "")
      
      # Putting in the gender
      if(Data2$Talker1[t] == "T1"){
        Data2$Gender1[t] <- "F"
      }
      if(Data2$Talker1[t] == "T2"){
        Data2$Gender1[t] <- "M"
      }
      if(Data2$Talker1[t] == "T4"){
        Data2$Gender1[t] <- "F"
      }
      if(Data2$Talker1[t] == "T6"){
        Data2$Gender1[t] <- "M"
      }
      if(Data2$Talker1[t] == "T9"){
        Data2$Gender1[t] <- "F"
      }
      if(Data2$Talker1[t] == "T10"){
        Data2$Gender1[t] <- "M"
      }
      
    }
    for(t in 1:length(Data2$Talker2)){
      # Taking the talker name from the stimulus
      Data2$Talker2[t] <- Data2$Stimulus2[t] %>% substr(start = 1, stop = 3) %>%
        gsub(pattern = "_", replacement = "")
      
      # Putting in the gender
      if(Data2$Talker2[t] == "T1"){
        Data2$Gender2[t] <- "F"
      }
      if(Data2$Talker2[t] == "T2"){
        Data2$Gender2[t] <- "M"
      }
      if(Data2$Talker2[t] == "T4"){
        Data2$Gender2[t] <- "F"
      }
      if(Data2$Talker2[t] == "T6"){
        Data2$Gender2[t] <- "M"
      }
      if(Data2$Talker2[t] == "T9"){
        Data2$Gender2[t] <- "F"
      }
      if(Data2$Talker2[t] == "T10"){
        Data2$Gender2[t] <- "M"
      }
      
      if(Data2$Gender1[t] == Data2$Gender2[t]){
        Data2$GenderCondition[t] <- "SG"
      } else{
        Data2$GenderCondition[t] <- "MG"
      }
    }
    
    
    
    
    # Making the values numeric
    Data2$Rating <- as.numeric(Data2$Rating)
    # Making the values numeric
    Data2$`Reaction Time` <- as.numeric(Data2$`Reaction Time`)
    # Making the values numeric
    Data2$Correct <- as.numeric(Data2$Correct)
    
    # Running this for each gender and talker in order to score
    gender <- c("MG","SG")
    talker <- c("ST","DT")
    
    g = 2
    t = 2
    # Running this for all iteration
    for(g in 1:length(gender)){
      for(t in 1:length(talker)){
        if(talker[t] != "ST"){
          # Doing this for different talkers
          score <- paste0(talker[t],gender[g],"_Score")
          RT <- paste0(talker[t],gender[g],"_RT")
          Rating <- paste0(talker[t],gender[g],"_Rating")
        } else{
          # Doing this for same talker
          score <- paste0(talker[t],"_Score")
          RT <- paste0(talker[t],"_RT")
          Rating <- paste0(talker[t],"_Rating")
        }
        
        Data3 <- Data2 %>% 
          filter(GenderCondition == gender[g]) %>%
          filter(ANSWER == talker[t])
        
        
        # Getting the average score
        Data2[1,score] <- mean(Data3$Correct)*100
        
        # Filtering out the NA's
        Data4 <- Data3 %>%
          filter(!is.na(Rating))
        
        # Calculating Mean Rating
        Data2[1,Rating] <- mean(Data4$Rating)
      
        # Filtering for reaction time below 3501
        Data4 <- Data3 %>%
          filter(`Reaction Time` < 3501)
        
        # Calculating mean reaction time
        Data2[1,RT] <- mean(Data4$`Reaction Time`)
        
      }
    }
    
    # Getting the average score
    Data2[1,"AvgCorrect"] <- mean(Data2$Correct)*100
    
    # Filtering out the NA's
    Data4 <- Data2 %>%
      filter(ANSWER == "DT") %>%
      filter(!is.na(Rating))
    
    # Calculating Mean Rating
    Data2[1,"AvgRating"] <- mean(Data4$Rating)
    
    # Filtering for reaction time below 3501
    Data4 <- Data2 %>%
      filter(`Reaction Time` < 3501)
    
    # Calculating mean reaction time
    Data2[1,"AvgRT"] <- mean(Data4$`Reaction Time`)
    
    
    
    
    
    
    
    
    
    
    # Removing extra columns
    Data2 <- Data2[,!names(Data2) %in% c("Zone Name","Zone Type","Reaction Onset","Response Type","Attempt","Incorrect","Dishonest","randomise_blocks",
                                         "randomise_trials")]
    # Writing the new excel sheet to the other folder
    write.xlsx(Data2, paste0("TD_Multiple_Conditions_",participant[p],"_",date[d],"_",task[f],"_Scored.xlsx"),showNA = F)
    
    if(move_to_analysis == T){
      setwd(analysis)
      # Getting a list of all of the files to move old files
      files2 = list.files(full.names = T)
      # Getting rid of the ./
      files2 <- gsub(x = files2, pattern = "./", replacement = "")
      # Getting the files that we need
      files2 <- files2[!grepl("Multiple", files2)]
      files2 <- files2[!grepl("Old", files2)]
      if(!dir.exists(paste0(analysis,"/Old"))){
        # Creating an old folder
        dir.create(paste0(analysis,"/Old"))
      }
      if(length(files2) > 0){
        for(x in 1:length(files2)){
          # Moving old files
          file.copy(from = files2[x], to = paste0(analysis,"/Old"))
          # Deleting file
          file.remove(files2[x])
        }
      }
      
      write.xlsx(Data2, paste0("TD_Multiple_Conditions_",participant[p],"_",date[d],"_",task[f],"_Scored.xlsx"),showNA = F)
    }
    
  }
}



