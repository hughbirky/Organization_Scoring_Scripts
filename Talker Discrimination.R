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

# C:/Users/hughm/Dropbox
# Clearing the console of previous junk
shell("cls")
# shell("clear")
# Clearing the environment of previous variables
rm(list=ls()) 
cat()
# path <- c("C:/Users/hughm/OneDrive - VUMC/General - Cochlear Implant Cognition and Communication Lab/R01+R21 Outcomes Studies/Data Collection/Subject testing/Cochlear Implant/CI200- McClure/06.08.2023- preop/Gorilla Tasks/TalkerDiscrim",
#           "C:/Users/hughm/OneDrive - VUMC/General - Cochlear Implant Cognition and Communication Lab/R01+R21 Outcomes Studies/Data Collection/Subject testing/Cochlear Implant/CI200- McClure/09.07.2023- 1 month/CI200_9.7.2023/Gorilla/Talker",
#           "C:/Users/hughm/OneDrive - VUMC/General - Cochlear Implant Cognition and Communication Lab/R01+R21 Outcomes Studies/Data Collection/Subject testing/Cochlear Implant/CI201- Larson/2023.08.14- 1 month/Gorilla Tasks CI201 1 Month/Talker",
#           "C:/Users/hughm/OneDrive - VUMC/General - Cochlear Implant Cognition and Communication Lab/R01+R21 Outcomes Studies/Data Collection/Subject testing/Cochlear Implant/CI202- Raybon/7.28.2023- preop/CI202-Tasks/Gorilla Tasks/TalkerDiscrim",
#           "C:/Users/hughm/OneDrive - VUMC/General - Cochlear Implant Cognition and Communication Lab/R01+R21 Outcomes Studies/Data Collection/Subject testing/Cochlear Implant/CI202- Raybon/09.27.2023- 1 month/Gorilla/Talker Discrim",
#           "C:/Users/hughm/OneDrive - VUMC/General - Cochlear Implant Cognition and Communication Lab/R01+R21 Outcomes Studies/Data Collection/Subject testing/Cochlear Implant/CI203- Luttman/08.31.2023- preop/Gorilla/Talker",
#           "C:/Users/hughm/OneDrive - VUMC/General - Cochlear Implant Cognition and Communication Lab/R01+R21 Outcomes Studies/Data Collection/Subject testing/Cochlear Implant/CI204- Hamm/09.06.2023- preop/CI204_9.6.2023/Gorilla/Talker Disc"
#           )
# participant <- c("CI200-Preop","CI200-1mo","CI201-1mo","CI202-preop","CI202-1mo","CI203-preop","CI204-preop")

# path <- "C:/Users/hughm/OneDrive - Belmont University/Personal/Desktop/VUMC/R01R21/Data"

path <- "C:/Users/hughm/OneDrive - VUMC/General - Cochlear Implant Cognition and Communication Lab/R01+R21 Outcomes Studies/Data Collection/Subject testing/Cochlear Implant/CI200- McClure/02.12.2024- 6 month/Gorilla Tasks/Talker Discrimination"
participant <- c("CI200_6_month_Best_Aided")
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

p = 1

for(p in 1:length(participant)){
  setwd(path[p])
  # Getting a list of all of the excel files
  files = list.files(full.names = T)
  # Getting rid of the ./
  files <- gsub(x = files, pattern = "./", replacement = "")
  
  
  
  # Import excel data
  Data1 <- read_excel(files[1])
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
  
  
  
  # Changing Correct Column
  for(z in 1:length(Data2$TalkerAnswer)){
    if(!is.na(Data2$ANSWER[z])){
      if(Data2$TalkerAnswer[z] == Data2$ANSWER[z]){
        Data2$Correct[z] <- 1
      }
    }
  }
  
  # Creating a new column for average rating
  Data2$AvgRating <- NA
  # Making the values numeric
  Data2$Rating <- as.numeric(Data2$Rating)
  
  # Calculating average rating
  total = 0
  count <- 0
  for(i in 1:length(Data2$Rating)){
    if(grepl("D",Data2$Response[i])){
      total <- total + Data2$Rating[i]
      count <- count + 1
    }
  }
  Data2$AvgRating[1] <- total/count
  
  
  
  
  # Creating a new column for average response time
  Data2$AvgRT <- NA
  # Making the values numeric
  Data2$`Reaction Time` <- as.numeric(Data2$`Reaction Time`)
  
  # Calculating Average Response Time
  total = 0
  count <- 0
  for(i in 1:length(Data2$`Reaction Time`)){
    if(Data2$Correct[i] == 1 && Data2$`Reaction Time`[i] < 3501){
      total <- total + Data2$`Reaction Time`[i]
      count <- count + 1
    }
  }
  Data2$AvgRT[1] <- total/count
  
  # Creating a new column for average correct
  Data2$AvgCorrect <- NA
  # Making the values numeric
  Data2$Correct <- as.numeric(Data2$Correct)
  # Getting the average score
  Data2$AvgCorrect[1] <- mean(Data2$Correct)
  
  # Removing extra columns
  Data2 <- Data2[,!names(Data2) %in% c("Zone Name","Zone Type","Reaction Onset","Response Type","Attempt","Incorrect","Dishonest","randomise_blocks",
                                       "randomise_trials")]
  # Writing the new excel sheet to the other folder
  write.xlsx(Data2, paste0("TD ",participant[p],"_Scored.xlsx"),showNA = F)
}