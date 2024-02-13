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



### For this script, you will need to do a few things. The first thing is 

# Clearing the environment of previous variables
rm(list=ls()) 

# Clearing the console of previous junk
shell("cls")

# Participant folder name
participant <- "CI200_02.12.2024_6_Month"

# Making the current path
path <- paste0("C:/Users/hughm/OneDrive - Belmont University/Personal/Desktop/VUMC/R01R21/Vowel_Consonant/",participant)

# Setting the unwanted columns shared between spreadsheets
gorillaColumns <- c("Event Index","UTC Timestamp","UTC Date","UTC Date and Time","Local Timezone","Experiment ID","Experiment Version",
                    "Tree Node Key","Repeat Key","Schedule ID","Participant Private ID","Participant Starting Group",
                    "Participant Status","Participant Completion Code","Participant External Session ID",
                    "Participant Device Type","Participant Device","Participant OS","Participant Browser",
                    "Participant Monitor Size","Participant Viewport Size","Checkpoint","Room ID","Room Order","Task Version",
                    "checkpoint-ncck",	"checkpoint-x5ti",	"checkpoint-vh38",	"checkpoint-73pu",	"checkpoint-czdn",
                    "checkpoint-vx9c",	"checkpoint-vpap",	"branch-r1ry",	"branch-usxa",	"branch-r7nz",	"randomiser-spvu",
                    "checkpoint-ylq9",	"checkpoint-cppz",	"randomiser-3ddq","Spreadsheet Name","X Coordinate","Y Coordinate",
                    "Timed Out","display","d","Vocoded","Spreadsheet Row","Screen Number","Screen Name","Zone Name","Zone Type","Reaction Onset","Response Type","Attempt","Incorrect","Dishonest","randomise_blocks",
                    "randomise_trials")


# Setting the working directory to the path
setwd(path)
# Getting a list of all of the excel files
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")

# Filtering out files we don't want
files <- grep(paste("fk1l", collapse = '|'),
              files, value = TRUE, invert = TRUE)
files <- grep(paste("57kv", collapse = '|'),
              files, value = TRUE, invert = TRUE)
files <- grep(paste("lt8f", collapse = '|'),
              files, value = TRUE, invert = TRUE)
files <- grep(paste("yt6f", collapse = '|'),
              files, value = TRUE, invert = TRUE)

# Import excel data
Data <- read_excel(files[1])
Data1 <- read_excel(files[2])

# Removing unwanted shared columns from all spreadsheets
Data <- Data[,!names(Data) %in% gorillaColumns]
# Removing unwanted shared columns from all spreadsheets
Data1 <- Data1[,!names(Data1) %in% gorillaColumns]

# Fixing error with UTC Timestamp and Date
Data <- Data[,!names(Data) %in% c("UTC Date","Local Date")]
# Removing unwanted shared columns from all spreadsheets
Data1 <- Data1[,!names(Data1) %in% c("UTC Date","Local Date")]

# Fixing reaction time issue
Data1$`Reaction Time` <- as.numeric(Data1$`Reaction Time`)
Data$`Reaction Time` <- as.numeric(Data$`Reaction Time`)


# Binding rows
Data2 <- bind_rows(Data1,Data)




# Actually removing the rows
Data2 <- Data2 %>% filter(!grepl('AUDIO PLAY REQUESTED', Response))
Data2 <- Data2 %>% filter(!is.na(Response))
Data2 <- Data2 %>% filter(!is.na(Consonant))

# Making list of consonants
consonants <- c("K","Z","M","N","DH","SH","B","ZS","T","V","D","S","TH","G","P","F")

# Counter
total <- 0
overallTotal <- 0
count <- 0
# Checking which vowel it is and summing
for(v in 1:length(consonants)){
  # Adding the vowel total column
  for(i in 1:length(Data2$Consonant)){
    if(Data2$Consonant[i] == consonants[v]){
      # Adding on value if it meets the condition
      total <- total + Data2$Correct[i]
      count <- count + 1
    }
  }

  # Adding total to the corresponding section
  Data2[1,paste0(consonants[v], " Total")] <- (total/count)*100

  # Adding percentages to overall total
  overallTotal <- overallTotal + (total/count)*100

  # Resetting counter
  total <- 0
  count <- 0
}

# Getting grand total
Data2[1,"Total"] <- overallTotal / length(consonants)

# Creating a folder for it
dir.create("Consonant")
# 
# # Changing writing directory into the vowel folder
path1 <- paste0("C:/Users/hughm/OneDrive - Belmont University/Personal/Desktop/VUMC/R01R21/Vowel_Consonant/",participant,"/Consonant")
setwd(path1)

# Writing the new excel sheet to the other folder
write.xlsx(Data2, paste0(participant,"_Consonant_Scored.xlsx"),showNA = F)

































# Making the current path
path <- paste0("C:/Users/hughm/OneDrive - Belmont University/Personal/Desktop/VUMC/R01R21/Vowel_Consonant/",participant)

# Setting the unwanted columns shared between spreadsheets
gorillaColumns <- c("Event Index","UTC Timestamp","UTC Date and Time","Local Timezone","Experiment ID","Experiment Version",
                    "Tree Node Key","Repeat Key","Schedule ID","Participant Private ID","Participant Starting Group",
                    "Participant Status","Participant Completion Code","Participant External Session ID",
                    "Participant Device Type","Participant Device","Participant OS","Participant Browser",
                    "Participant Monitor Size","Participant Viewport Size","Checkpoint","Room ID","Room Order","Task Version",
                    "checkpoint-ncck",	"checkpoint-x5ti",	"checkpoint-vh38",	"checkpoint-73pu",	"checkpoint-czdn",
                    "checkpoint-vx9c",	"checkpoint-vpap",	"branch-r1ry",	"branch-usxa",	"branch-r7nz",	"randomiser-spvu",
                    "checkpoint-ylq9",	"checkpoint-cppz",	"randomiser-3ddq","Spreadsheet Name","X Coordinate","Y Coordinate",
                    "Timed Out","display","d","Vocoded","Spreadsheet Row","Screen Number","Screen Name","Zone Name","Zone Type","Reaction Onset","Response Type","Attempt","Incorrect","Dishonest","randomise_blocks",
                    "randomise_trials")


# Setting the working directory to the path
setwd(path)
# Getting a list of all of the excel files
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")

# Filtering out files we don't want
files <- grep(paste("fk1l", collapse = '|'),
              files, value = TRUE, invert = TRUE)
files <- grep(paste("gqks", collapse = '|'),
              files, value = TRUE, invert = TRUE)
files <- grep(paste("vqlt", collapse = '|'),
              files, value = TRUE, invert = TRUE)
files <- grep(paste("yt6f", collapse = '|'),
              files, value = TRUE, invert = TRUE)
files <- grep(paste("Consonant", collapse = '|'),
              files, value = TRUE, invert = TRUE)

# Import excel data
Data <- read_excel(files[1])
Data1 <- read_excel(files[2])

# Binding rows
Data2 <- bind_rows(Data1,Data)


# Removing unwanted shared columns from all spreadsheets
Data2 <- Data2[,!names(Data2) %in% gorillaColumns]

# Actually removing the columns
Data2 <- Data2 %>% filter(!grepl('AUDIO PLAY REQUESTED', Response))
Data2 <- Data2 %>% filter(!is.na(Response))


# Making list of vowels
vowels <- c("AE","AH","EE","EH","ER","EY","I","OO","OW","UH","UU")

# Counter
total <- 0
overallTotal <- 0
count <- 0
# Checking which vowel it is and summing
for(v in 1:length(vowels)){
  # Adding the vowel total column
  for(i in 1:length(Data2$Vowel)){
    if(Data2$Vowel[i] == vowels[v]){
      # Adding on value if it meets the condition
      total <- total + Data2$Correct[i]
      count <- count + 1
    }
  }
  
  # Adding total to the corresponding section
  Data2[1,paste0(vowels[v], " Total")] <- (total/count)*100
  
  # Adding percentages to overall total
  overallTotal <- overallTotal + (total/count)*100
  
  # Resetting counter
  total <- 0
  count <- 0
}

# Getting grand total
Data2[1,"Total"] <- overallTotal / 11

# Creating a folder for it
dir.create("Vowel")

# Changing writing directory into the vowel folder
path2 <- paste0("C:/Users/hughm/OneDrive - Belmont University/Personal/Desktop/VUMC/R01R21/Vowel_Consonant/",participant,"/Vowel")
setwd(path2)

# Writing the new excel sheet to the other folder
write.xlsx(Data2, paste0(participant,"_Vowel_Scored.xlsx"),showNA = F)
setwd("C:/Users/hughm/OneDrive - Belmont University/Personal/Desktop")
