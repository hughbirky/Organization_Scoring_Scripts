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
participant <- c("CI200")
date <- c("12 mo")
calDate <- "07.30.2024"
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
path <- paste0(path,"/",files[1],"/Gorilla Tasks/MLST")


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
# files <- grep(paste("fk1l", collapse = '|'),
#               files, value = TRUE, invert = TRUE)

# Import excel data
Data <- read_excel(files[1])

# Removing unwanted shared columns from all spreadsheets
Data <- Data[,!names(Data) %in% gorillaColumns]

# Actually removing the columns
Data <- Data %>% filter(grepl("gorilla", Response))

# Adding an 3 extra rows
Data[nrow(Data) + 1,] <- NA
Data[nrow(Data) + 1,] <- NA
Data[nrow(Data) + 1,] <- NA

# finding the length of the data fram
length <- length(Data$Response)

# Importing Template
template <- read_excel("C:/Users/hughm/OneDrive - VUMC/General/R01+R21 Outcomes Studies/Data Collection/Scoring Templates/MLST/MLST template.xlsx")

# Adding new columns
Data$KeyWords <- template$KeyWords[1:length]
Data$SubjResponse <- template$SubjResponse[1:length]
Data$`Total words` <- template$`Total words`[1:length]
Data$`# words correct` <- template$`# words correct`[1:length]
Data$`Total Keywords` <- template$`Total Keywords`[1:length]
Data$`# Keywords correct` <- template$`# Keywords correct`[1:length]
Data$Comments <- template$Comments[1:length]

# Rearranging columns
Data <- Data %>%
  relocate(KeyWords,.before = ShowProgressBar)


# Adding in equations
Data$`# words correct`[length - 2] <- "=SUM(T2:T31)"
Data$`# words correct`[length] <- "=(T32/S32) * 100"
Data$`# Keywords correct`[length - 2] <- "=SUM(V2:V31)"
Data$`# Keywords correct`[length] <- "=(V32/U32) * 100"

# Writing the new excel sheet to the other folder
write.xlsx(x = Data,file = paste0(participant,"_",calDate,"_MLST.xlsx"), sheetName = "Scorer 1",showNA = F)
write.xlsx(x = Data,file = paste0(participant,"_",calDate,"_MLST.xlsx"), sheetName = "Scorer 2",append = TRUE,showNA = F)
setwd("C:/Users/hughm/Desktop")
