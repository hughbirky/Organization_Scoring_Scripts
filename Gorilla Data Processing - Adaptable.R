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
 
### Cleaning
# C:/Users/hughm/Dropbox
# Clearing the console of previous junk
shell("cls")
# shell("clear")
# Clearing the environment of previous variables
rm(list=ls()) 

# Creating a blank variable 'participant'
participant <- ""

# Setting the iterator for this while loop
i = 1

 
# Continues to prompt you for participants you want to do this for until you specify stop
while(T){
  # Prompts the user, stores response as the variable newparticipant
  newparticipant <- user.input <- dlgInput(paste("Enter participant",i,"that you wish to run this for.
                                                 Enter 'clear' to remove previous entry. Enter 'stop' to finish entering: "),
                                           Sys.info()["user"])$res
  # Checks to see if the user has entered stop, if so it breaks the loop
  if(grepl(pattern = "STOP|stop",x = newparticipant)){
    break
  }

  # Checks to see if the user enters clear
  if(grepl(pattern = "clear",x = newparticipant)){
    # Removes last entered participant and puts iterator back to where it was
    participant <- head(participant,-1)
    i <- i - 1
    next
  }

  # Assigns new participant to the list of participants and increments iterator
  participant[i] <- newparticipant
  i <- i + 1
}

# Prompts user to set the working directory 
# This should be the path to Dropbox
pathO <- user.input <- dlgInput(paste("Input the path to the dropbox folder"),
                               Sys.info()["user"])$res
# Raw data path
copyPath <- paste(pathO,"/Cochlear_IIR_Project/Analysis/Data/Raw_Data/Participants",sep = "")
copyPath <- paste(copyPath,"/",sep = "")
# Edited Data Path
path1 <- paste(pathO,"/Cochlear_IIR_Project/Analysis/Data/Edited_Data/Participants",sep = "")
path <-  paste(path1,"/",sep = "")
# Edited data path later
outpath <- paste(pathO,"/Cochlear_IIR_Project/Analysis/Data/Edited_Data/Participants",sep = "")
outpath <- paste(outpath,"/",sep = "")


# Enter in Tasks
task <- c("Accent Discrimination",
          "Accent Identification",
          "Comprehension Noise",
          "Comprehension Quiet Memory",
          "Comprehension Quiet Rate",
          "Gender Discrimination",
          "Sentence Rec Noise Adaptive",
          "Sentence Rec Noise Final",
          "Sentence Rec Quiet",
          "Talker Discrimination",
          "Word Rec Noise Adaptive",
          "Word Rec Noise Final",
          "Word Rec Quiet"
)

# Putting their gorilla task name

task1 <- "8xlv"
task2 <- "h1gh"
task3 <- "h5sd"
task4 <- "77c9"
task5 <- "c13a"
task6 <- "8z8z"
task7 <- "6sm8"
task8 <- c("rk1r","h9bv","a35z","lm7c","sq5x","cpa9","iktf","y1yc","62g2","4aq4","1im9","2376","387z","i1re","zm3s","omvx","jnzg","m1fl","ulz1","msh1")
task9 <- "6blb"
task10 <- "3t8j"
task11 <- "mzm6"
task12 <- c("z5lu","rc91","rrmo","csbu","hj3q","qq4y","ec4t","1apf","ph44","nm24","izig","duwb","2dy4","woox","38hp","2ltn","qq4y")
task13 <- "2qen"

# Listing gorilla tasks to delete
delete <- c("27fq","tzw8","1bdx","3jl5","aaqi","b5lv","epo4","h8iu","jzfr","lzd3","oidt","qotl","y3ig","yxng")

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

# Finding the maximum number of task names per task

max_length <- max(c(length(task1), length(task2), length(task3)), length(task4), length(task5), length(task6), length(task7),
                  length(task8), length(task9), length(task10), length(task11), length(task12), length(task13))

# Making a multiple length data frame

taskNames <- data.frame(col1 = c(task1,                 
                                 rep(NA, times = max_length - length(task1))),
                        col2 = c(task2,
                                 rep(NA, times = max_length - length(task2))),
                        col3 = c(task3,
                                 rep(NA, times = max_length - length(task3))),
                        col4 = c(task4,
                                 rep(NA, times = max_length - length(task4))),
                        col5 = c(task5,
                                 rep(NA, times = max_length - length(task5))),
                        col6 = c(task6,
                                 rep(NA, times = max_length - length(task6))),
                        col7 = c(task7,
                                 rep(NA, times = max_length - length(task7))),
                        col8 = c(task8,
                                 rep(NA, times = max_length - length(task8))),
                        col9 = c(task9,
                                 rep(NA, times = max_length - length(task9))),
                        col10 = c(task10,
                                  rep(NA, times = max_length - length(task10))),
                        col11 = c(task11,
                                  rep(NA, times = max_length - length(task11))),
                        col12 = c(task12,
                                  rep(NA, times = max_length - length(task12))),
                        col13 = c(task13,
                                  rep(NA, times = max_length - length(task13))))


### Entering tasks that require audio files

taskRecord <- c("Word Rec Noise",
                "Word Rec Quiet",
                "Sentence Rec Noise",
                "Sentence Rec Quiet")
taskRecord <- gsub(x = taskRecord, pattern = " ", replacement = "_")  

# Corresponding gorilla task names

taskRec1 <- c("z5lu","rc91","rrmo","csbu","hj3q","qq4y","ec4t","1apf","ph44","nm24","izig","duwb","2dy4","woox","38hp","2ltn","qq4y")
taskRec2 <- "2qen"
taskRec3 <- c("rk1r","h9bv","a35z","lm7c","sq5x","cpa9","iktf","y1yc","62g2","4aq4","1im9","2376","387z","i1re","zm3s","omvx","jnzg","m1fl","ulz1","msh1")
taskRec4 <- "6blb"


# Finding max length list from above
max_length_rec <- max(c(length(taskRec1), length(taskRec2), length(taskRec3)), length(taskRec4))

# Creating a multiple length data frame
taskRecName <- data.frame(col1 = c(taskRec1,                 
                                   rep(NA, times = max_length - length(taskRec1))),
                          col2 = c(taskRec2,
                                   rep(NA, times = max_length - length(taskRec2))),
                          col3 = c(taskRec3,
                                   rep(NA, times = max_length - length(taskRec3))),
                          col4 = c(taskRec4,
                                   rep(NA, times = max_length - length(taskRec4))))






# Copying Files to Edited_Data
p = 1
for(p in 1:length(participant)){
  # Copying files
  # Sets the working directory for renaming
  setwd(path1)
  dir.create(participant[p])
  
  setwd(paste(copyPath,participant[p],sep = ""))
  # Getting a list of all of the files in the folder
  files = list.files(full.names = T)
  # Removing initial ./ in names
  files <- gsub(x = files, pattern = "./", replacement = "")
  # Renaming spreadsheets for organization based on the task name
  d = 9
  for(d in 1:length(files)){
    # Creating Upload Folder
    if(dir.exists(files[d])){
      # Creating a folder for the internal folders
      setwd(paste(path,participant[p],sep = ""))
      dir.create(files[d])
      
      # Setting working directory to original file
      setwd(paste(copyPath,participant[p],"/",files[d], sep = ""))
      
      # Getting a list of all of the files in the folder
      files2 = list.files(full.names = T)
      # Removing initial ./ in names
      files2 <- gsub(x = files2, pattern = "./", replacement = "")
      e = 1
      for(e in 1:length(files2)){
        # Copying files into the new folder
        file.copy(from = files2[e], to = paste(path,participant[p],"/",files[d],sep = ""))
      }
    }
    else{
      file.copy(from = files[d], to = paste(path,participant[p],sep = ""))
    }
  }
  

  
  # Sets the working directory for renaming
  setwd(paste(path,participant[p], sep = ""))
  
  # Getting a list of all of the files in the folder
  files = list.files(full.names = T)
  # Removing initial ./ in names
  files <- gsub(x = files, pattern = "./", replacement = "")
  files <- grep(paste("uploads", collapse = '|'),
                files, value = TRUE, invert = TRUE)
  files <- grep(paste("Scored", collapse = '|'),
                files, value = TRUE, invert = TRUE)
  

  
  for(c in 1:length(files)){
    for(t in 1:max_length){
      for(a in 1:length(task)){
        if(grepl(pattern = taskNames[t,a],x = files[c]) && !is.na(taskNames[t,a])){
          file.rename(from = files[c], to = paste("edit_",task[a]," ", participant[p], ".xlsx", sep = ""))
        }
      }
    }
  }
  
  
  
  for(d in 1:length(delete)){
    files_to_delete <- dir(path= paste(path,participant[p],sep = ""),pattern=delete[d])
    file.remove(file.path(paste(path,participant[p],sep = ""), files_to_delete))
  }
  
  
  # Reorganizing Audio Files
  # Set the working directory into the uploads folder
  setwd(paste(path,participant[p], "/uploads", sep = ""))
  
  # Getting a list of all of the excel files
  files = list.files(full.names = T)
  # Removing initial ./ in name
  files <- gsub(x = files, pattern = "./", replacement = "")
  

  
  for(c in 1:length(files)){
    for(d in 1:length(taskRecord)){
      if(!dir.exists(paste(taskRecord[d],"_",participant[p], sep = ""))){
        dir.create(paste(taskRecord[d],"_",participant[p], sep = ""))
      }
      for(t in 1:max_length_rec){
        # Writing out input path for command window
        input <- paste(path,participant[p], "/uploads/", files[c], sep = "")
        # Writing the output path
        output <- gsub(x = input, pattern = ".webm", replacement = ".wav")
        if(!is.na(taskRecName[t,d]) && grepl(pattern = taskRecName[t,d],x = files[c])){
          # Creating the output path for the wav file
          outputpath <- paste(path,participant[p],"/uploads/", taskRecord[d],"_", participant[p], "/", gsub(x = files[c], pattern = ".webm", replacement = ".wav"), sep = "")
          # Creating command for command prompt and executing
          cmd <- sprintf("ffmpeg -i %s -vn %s", input, outputpath)
          # Calling the system command
          system(cmd)
        }
      }
    }
  }

    # Removing unnecessary rows and columns from the excel sheets
  # Creating a directory for the participant
  if(!dir.exists(paste(outpath,participant[p],sep = ""))){
    dir.create(paste(outpath,participant[p],sep = ""))
  }
  # Setting the working directory
  setwd(paste(path,participant[p], sep = ""))
  
  # Getting a list of all of the excel files
  files = list.files(full.names = T)
  # Getting rid of the ./
  files <- gsub(x = files, pattern = "./", replacement = "")
  # Remove the uploads folder from list
  files <- grep(paste("uploads", collapse = '|'),
                files, value = TRUE, invert = TRUE)
  files2 <- gsub(x = files, pattern = "edit_", replacement = "")
  
  
  
  # Actually removing the files
  for (c in 1:length(files)) {
    # Import excel data
    Data1 <- read_excel(files[c])
    # Removing unwanted shared columns from all spreadsheets
    Data2 <- Data1[,!names(Data1) %in% gorillaColumns]
    
    # Filtering Responses (and rows) by file name
    if(grepl("Accent Discr",files[c])){
      # Removing unneeded rows
      Data2 <- Data2 %>% filter(!grepl('AUDIO PLAY REQUESTED', Response))
      Data2 <- Data2 %>% filter(!is.na(Response))
      # Renaming Attempt Columns
      Data2 <- Data2 %>% rename("Rating" = "Attempt")
      # Copying the response column into the new rating column and shifting it up two to match the rating with the response
      Data2$Rating <- lead(Data2$Response, 2, default = "")
      # Filtering extra rows
      Data2 <- Data2 %>% filter(grepl('Native|Nonnative', Response))
      # Cutting more unwanted rows
      Data2 <- Data2[,!names(Data2) %in% c("Zone Name","Zone Type","Reaction Onset","Response Type","Attempt","Incorrect","Dishonest","randomise_blocks",
                                           "randomise_trials")]
      # Writing the new excel sheet to the other folder
      write.xlsx(Data2, paste(outpath,participant[p],"/",files2[c],sep = ""),showNA = F)
    }
    
    if(grepl(pattern = "Accent Id",x = files[c])){
      # Removing unneeded rows
      Data2 <- Data2 %>% filter(grepl('North|North Midland|Northeast|South|South Midland|West', Response))
      # Renaming Attempt Columns
      Data2 <- Data2 %>% rename("RegionAnswer" = "Attempt")
      # Copying data to 
      Data2$RegionAnswer <- Data2$Response
      # Turing region names into numbers
      Data2$RegionAnswer<- gsub(x = Data2$RegionAnswer,pattern = "Northeast",replacement = 1)
      Data2$RegionAnswer<- gsub(x = Data2$RegionAnswer,pattern = "South Midland",replacement = 4)
      Data2$RegionAnswer<- gsub(x = Data2$RegionAnswer,pattern = "North Midland",replacement = 3)
      Data2$RegionAnswer<- gsub(x = Data2$RegionAnswer,pattern = "South",replacement = 5)
      Data2$RegionAnswer<- gsub(x = Data2$RegionAnswer,pattern = "North",replacement = 2)
      Data2$RegionAnswer<- gsub(x = Data2$RegionAnswer,pattern = "West",replacement = 6)
      # Checks if the answer is correct and changes the correct answers column
      for(z in 1:length(Data2$RegionAnswer)){
        if(!is.na(Data2$Region[z])){
          if(Data2$RegionAnswer[z] == Data2$Region[z]){
            Data2$Correct[z] <- 1
          }
        }
      }
      # Removing extra columns
      Data2 <- Data2[,!names(Data2) %in% c("Zone Name","Zone Type","Reaction Onset","Response Type","Attempt","Incorrect","Dishonest","randomise_blocks",
                                           "randomise_trials")]
      # Writing the new excel sheet to the other folder
      write.xlsx(Data2, paste(outpath,participant[p],"/",files2[c],sep = ""),showNA = F)
    }
    
    if(grepl(pattern = "Comprehension Noise",x = files[c])){
      # Removing unneeded rows followed by columns
      Data2 <- Data2 %>% filter(grepl('TRUE|FALSE', Response))
      Data2 <- Data2[,!names(Data2) %in% c("Zone Name","Zone Type","Reaction Onset","Response Type","Attempt","Incorrect","Dishonest","randomise_blocks",
                                           "randomise_trials")]
      # Writing the new excel sheet to the other folder
      write.xlsx(Data2, paste(outpath,participant[p],"/",files2[c],sep = ""),showNA = F)
    }
    
    if(grepl(pattern = "Comprehension Quiet Mem",x = files[c])){
      # Removing unnecessary rows
      Data2 <- Data2 %>% filter(!grepl('AUDIO PLAY REQUESTED', Response))
      Data2 <- Data2 %>% filter(!is.na(Response))
      # Renaming Attempt Columns
      Data2 <- Data2 %>% rename("Rating" = "Attempt")
      # Copying over the data from response into rating and shifting it up one to match the responses
      Data2$Rating <- lead(Data2$Response, 1, default = "")
      # Filtering extra rows
      Data2 <- Data2 %>% filter(grepl('OLD|NEW', Response))
      # Filtering extra columns
      Data2 <- Data2[,!names(Data2) %in% c("Zone Name","Zone Type","Reaction Onset","Response Type","Attempt","Incorrect","Dishonest","randomise_blocks",
                                           "randomise_trials")]
      # Writing the new excel sheet to the other folder
      write.xlsx(Data2, paste(outpath,participant[p],"/",files2[c],sep = ""),showNA = F)
    }

    if(grepl(pattern = "Comprehension Quiet Rate",x = files[c])){
      # Removing unneeded rows followed by columns
      Data2 <- Data2 %>% filter(grepl('TRUE|FALSE', Response))
      Data2 <- Data2[,!names(Data2) %in% c("Zone Name","Zone Type","Reaction Onset","Response Type","Attempt","Incorrect","Dishonest","randomise_blocks",
                                           "randomise_trials")]
      # Writing the new excel sheet to the other folder
      write.xlsx(Data2, paste(outpath,participant[p],"/",files2[c],sep = ""),showNA = F)
    }

    if(grepl(pattern = "Dashboard",x = files[c])){
      Data2 <- Data2 %>% filter(!grepl('BEGIN|END', Response))
      Data2 <- Data2 %>% filter(!is.na(Response))
    }

    if(grepl(pattern = "Gender",x = files[c])){
      Data2 <- Data2 %>% filter(!grepl('AUDIO|ADJUSTED', Response))
      Data2 <- Data2 %>% filter(!is.na(Response))
      # Renaming Response and Attempt Columns
      Data2 <- Data2 %>% rename("Rating" = "Attempt")
      # Shifting Rating Column by 1
      Data2$Rating <- lead(Data2$Response, 2, default = "")
      # Filtering extra rows
      Data2 <- Data2 %>% filter(grepl('Third Talker|First Talker', Response))
      # Reusing the attemp column
      Data2 <- Data2 %>% rename("TalkerAnswer" = "Incorrect")
      # Copying data to 
      Data2$TalkerAnswer <- Data2$Response
      # Turing names into letters
      Data2$TalkerAnswer<- gsub(x = Data2$TalkerAnswer,pattern = "First Talker",replacement = "A")
      Data2$TalkerAnswer<- gsub(x = Data2$TalkerAnswer,pattern = "Third Talker",replacement = "B")
      # Changing Correct Column
      for(z in 1:length(Data2$TalkerAnswer)){
        if(!is.na(Data2$ANSWER[z])){
          if(Data2$TalkerAnswer[z] == Data2$ANSWER[z]){
            Data2$Correct[z] <- 1
          }
        }
      }
      # Cutting more unwanted rows
      Data2 <- Data2[,!names(Data2) %in% c("Zone Name","Zone Type","Reaction Onset","Response Type","Attempt","Incorrect","Dishonest","randomise_blocks",
                                           "randomise_trials","Stimulus","ShowProgressBar","Class","Item","Talker","List","Rate","Sentence")]
      # Writing the new excel sheet to the other folder
      write.xlsx(Data2, paste(outpath,participant[p],"/",files2[c],sep = ""),showNA = F)
    }

    if(grepl(pattern = "Practice Disc",x = files[c])){
      Data2 <- Data2 %>% filter(!grepl('AUDIO PLAY REQUESTED|AUDIO PLAY EVENT FIRED|AUDIO ENDED EVENT FIRED|50', Response))
      Data2 <- Data2 %>% filter(!is.na(Response))
    }
    
    if(grepl(pattern = "Practice Sent",x = files[c])){
      Data2 <- Data2 %>% filter(grepl('Recording Start:', Response))
    }
    
    if(grepl(pattern = "Practice SVT",x = files[c])){
      Data2 <- Data2 %>% filter(grepl('TRUE|FALSE', Response))
    }

    if(grepl(pattern = "Sentence Rec Noise Ad",x = files[c])){
      Data2 <- Data2 %>% filter(!grepl('audio started', Response))
      Data2 <- Data2 %>% filter(!is.na(Response))
      Data2 <- Data2[,!names(Data2) %in% c("Display","Screen","Screen ID","Screen Counter","Response Type","Proportion",
                                           "Tag","Component Name","Object Name","Object ID","Manipulation: Spreadsheet","Store: response",
                                           "Store: Response","Store: Correct","Store: percentCorrect","Store: participantResponse",
                                           "Store: SNRtotal","Store: Incorrect")]
      # Writing the new excel sheet to the other folder
      write.xlsx(Data2, paste(outpath,participant[p],"/",files2[c],sep = ""),showNA = F)
    }

    if(grepl(pattern = "Sentence Rec Noise Fi",x = files[c])){
      Data2 <- Data2 %>% filter(grepl('Recording Start', Response))
      Data2 <- Data2[,!names(Data2) %in% c("Display","Screen","Screen ID","Screen Counter","Response Type","Clock Time","Absolute Onset Time",
                                           "Absolute Clock Time","Absolute Reaction Time","Onset Time","Response Onset","Response Duration", "Proportion",
                                           "Tag","Component Name","Object Name","Object Number","Object ID","Spreadsheet: display",
                                           "Spreadsheet: randomise_blocks","Spreadsheet: randomise_trials","Spreadsheet: ShowProgressBar",
                                           "Manipulation: Spreadsheet")]
      # Writing the new excel sheet to the other folder
      write.xlsx(Data2, paste(outpath,participant[p],"/",files2[c],sep = ""),showNA = F)
    }

    if(grepl(pattern = "Sentence Rec Quiet",x = files[c])){
      Data2 <- Data2 %>% filter(grepl('Recording Start:', Response))
      Data2 <- Data2[,!names(Data2) %in% c("Display","Screen","Screen ID","Screen Counter","Response Type","Response Duration","Proportion",
                                           "Tag","Component Name","Object Name","Object Number","Object ID","Spreadsheet: display",
                                           "Spreadsheet: randomise_blocks","Spreadsheet: randomise_trials","Spreadsheet: ShowProgressBar",
                                           "Spreadsheet: snr","Spreadsheet: append1","Spreadsheet: append2","Manipulation: Spreadsheet")]
      # Writing the new excel sheet to the other folder
      write.xlsx(Data2, paste(outpath,participant[p],"/",files2[c],sep = ""),showNA = F)
    }

    if(grepl(pattern = "Talker Dis",x = files[c])){
      Data2 <- Data2 %>% filter(!grepl('AUDIO|ADJUSTED', Response))
      Data2 <- Data2 %>% filter(!is.na(Response))
      # Renaming Response and Attempt Columns
      Data2 <- Data2 %>% rename("Rating" = "Attempt")
      # Shifting Rating Column by 1
      Data2$Rating <- lead(Data2$Response, 2, default = "")
      # Filtering extra rows
      Data2 <- Data2 %>% filter(grepl('Same Talker|Different Talker', Response))
      # Reusing the attemp column
      Data2 <- Data2 %>% rename("TalkerAnswer" = "Incorrect")
      # Copying data to 
      Data2$TalkerAnswer <- Data2$Response
      # Turing region names into numbers
      Data2$TalkerAnswer<- gsub(x = Data2$TalkerAnswer,pattern = "Same Talker",replacement = "ST")
      Data2$TalkerAnswer<- gsub(x = Data2$TalkerAnswer,pattern = "Different Talker",replacement = "DT")
      # Changing Correct Column
      for(z in 1:length(Data2$TalkerAnswer)){
        if(!is.na(Data2$ANSWER[z])){
          if(Data2$TalkerAnswer[z] == Data2$ANSWER[z]){
            Data2$Correct[z] <- 1
          }
        }
      }
      # Removing extra columns
      Data2 <- Data2[,!names(Data2) %in% c("Zone Name","Zone Type","Reaction Onset","Response Type","Attempt","Incorrect","Dishonest","randomise_blocks",
                                           "randomise_trials")]
      # Writing the new excel sheet to the other folder
      write.xlsx(Data2, paste(outpath,participant[p],"/",files2[c],sep = ""),showNA = F)
    }

    if(grepl(pattern = "Word Rec Noise Ad",x = files[c])){
      Data2 <- Data2 %>% filter(!grepl('audio started', Response))
      Data2 <- Data2 %>% filter(!is.na(Response))
      Data2 <- Data2[,!names(Data2) %in% c("Display","Screen","Screen ID","Screen Counter","Response Type","Proportion",
                                           "Tag","Component Name","Object Name","Object Number","Object ID","Manipulation: Spreadsheet","Store: response",
                                           "Store: Response")]
      # Writing the new excel sheet to the other folder
      write.xlsx(Data2, paste(outpath,participant[p],"/",files2[c],sep = ""),showNA = F)
    }

    if(grepl(pattern = "Word Rec Noise Fin",x = files[c])){
      Data2 <- Data2 %>% filter(grepl('Recording Start:', Response))
      Data2 <- Data2[,!names(Data2) %in% c("Display","Screen","Screen ID","Screen Counter","Response Type","Response Duration","Proportion",
                                           "Tag","Component Name","Object Name","Object Number","Object ID","Spreadsheet: display",
                                           "Spreadsheet: randomise_blocks","Spreadsheet: randomise_trials","Spreadsheet: ShowProgressBar",
                                           "Manipulation: Spreadsheet")]
      # Writing the new excel sheet to the other folder
      write.xlsx(Data2, paste(outpath,participant[p],"/",files2[c],sep = ""),showNA = F)
    }
    
    if(grepl(pattern = "Word Rec Quiet",x = files[c])){
      Data2 <- Data2 %>% filter(grepl('Recording Start:', Response))
      Data2 <- Data2[,!names(Data2) %in% c("Display","Screen","Screen ID","Screen Counter","Response Type","Response Duration","Proportion",
                                           "Tag","Component Name","Object Name","Object Number","Object ID","Spreadsheet: display",
                                           "Spreadsheet: randomise_blocks","Spreadsheet: randomise_trials","Spreadsheet: ShowProgressBar",
                                           "Spreadsheet: snr","Spreadsheet: append1","Spreadsheet: append2","Manipulation: Spreadsheet")]
      # Writing the new excel sheet to the other folder
      write.xlsx(Data2, paste(outpath,participant[p],"/",files2[c],sep = ""),showNA = F)
    }
  }
  
  
  setwd(paste(outpath,participant[p],sep = ""))
  # Getting a list of all of the excel files
  files = list.files(full.names = T)
  # Getting rid of the ./
  files <- gsub(x = files, pattern = "./", replacement = "")
  # Remove the uploads folder from list
  files <- grep(paste("uploads", collapse = '|'),
                files, value = TRUE, invert = TRUE)
# setwd(pathO)
  files_to_delete <- dir(path= paste(path,participant[p],sep = ""),pattern="*edit_")
  file.remove(file.path(paste(path,participant[p],sep = ""), files_to_delete))
  
  
}



setwd(pathO)

