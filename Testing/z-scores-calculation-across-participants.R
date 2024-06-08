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
suppressMessages(library(stringdist))
library(plyr)



# Clearing the console
shell("cls")

# Clearing the environment of previous variables
rm(list=ls()) 


################################################################################
# Use these two variables to determine the path to your files and the column that you want to calculate
# Setting the working directory
path <- "C:/Users/hughm/Dropbox/Cochlear_IIR_Project/Analysis/Scoring/Scoring_in_Progress/CI/Word Rec Quiet/Pared Down"
# Name of the column we want to calculate z-score for
column_name <- "Calculated Reaction Time"









setwd(path)
# Getting a list of all of the files
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")
# Getting the files that we need
files <- files[grepl(".xlsx", files)]
files <- files[!grepl("Offset", files)]
files <- files[!grepl("~", files)]
files <- files[!grepl("Combined", files)]





# Creating a storage dataframe
combine_df <- data.frame(row.names = NULL)
names <- c("")
f = 21

# Creating a track to see where to put the averaged scores
average_z_score <- data.frame(matrix(ncol = 2,nrow = length(files)))
colnames(average_z_score) <- c("ID","Average")

# Calculating the z scores for each subject before adding them to a combined data frame
for(f in 1:length(files)){
  # Reading excel
  Data1 <- read_excel(files[f])

  # Getting column number for relocating
  col_num <- which(names(Data1) == column_name)


  # Creating new columns
  Data1 <- mutate(Data1, `z_score` = NA) %>%
    relocate(`z_score`, .after = col_num)
  Data1 <- mutate(Data1, `z_Score_Total` = NA) %>%
    relocate(`z_Score_Total`, .after = col_num+1)
  # Data1 <- mutate(Data1, `z_Score_Average` = NA) %>%
  #   relocate(`z_Score_Average`, .after = col_num+2)


  # Calculating mean if the column we want exists
  if(column_name %in% colnames(Data1)){
    x <- get(column_name, Data1)
  }else {
    names <- c(names,files[f])
    next
  }

  # Removing NA's
  x_filt <- na.omit(x)

  # Calculating mean and standard deviation
  mean <- mean(x_filt)
  std <- sd(x_filt)





  # Calculating
  for(i in 1:length(x)){
    if(!is.na(x[i])){
      Data1$z_score[i] <- (x[i] - mean) / std
    }
  }

  # Getting rid of NA's in z_score
  Data2 <- Data1 %>%
    filter(abs(z_score) < 3)
  
  

  # Adding to the combined data frame
  if(f == 1){
    # If the combined data frame is empty, we want to make it the original file
    combine_df <- Data2
  } else{
    combine_df <- rbind.fill(combine_df,Data2)
  }
  
  # Adding ID for the average scores data frame
  average_z_score$ID[f] <- Data2$`Participant Public ID`[2]
  
  
}




# Calculating and filtering z_Score_Total
# Assigning the values to x
x1 <- get(column_name, combine_df)

# Removing NA's
x1_filt <- na.omit(x1)

# Calculating mean and standard deviation
mean <- mean(x1_filt)
std <- sd(x1_filt)

# Calculating z_score
for(i in 1:nrow(combine_df)){
  if(!is.na(x1[i])){
    combine_df$z_Score_Total[i] <- (x1[i] - mean) / std
  }
}


for(a in 1:length(average_z_score$ID)){
# for(a in 1:2){
  filtered <- combine_df %>%
    filter(`Participant Public ID` == average_z_score$ID[a])
  
  average_z_score$Average[a] <- mean(filtered$z_Score_Total)
}



# Writing excel sheet
# write.xlsx(combine_df,paste0("Combined_z_scores_",column_name,".xlsx"),showNA = F)
write.xlsx(combine_df,paste0("Combined_z_scores_",column_name,".xlsx"), sheetName = "Scores",showNA = F)
write.xlsx(average_z_score,paste0("Combined_z_scores_",column_name,".xlsx"), sheetName = "Average_Z_Scores",append = TRUE,showNA = F)



