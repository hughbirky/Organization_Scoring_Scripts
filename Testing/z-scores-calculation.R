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
path <- "C:/Users/hughm/Dropbox/Cochlear_IIR_Project/Analysis/Scoring/Scoring_in_Progress/CI/Word Rec Quiet"
# Name of the column we want to calculate z-score for
column_name <- "Calculated Reaction Time"

z_score_filter <- 3









setwd(path)
# Getting a list of all of the files
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")
# Getting the files that we need
files <- files[grepl(".xlsx", files)]
files <- files[!grepl("Offset", files)]
files <- files[!grepl("~", files)]




# Creating a storage dataframe
combine_df <- data.frame(row.names = NULL)
names <- c("")
f = 1

# Calculating the z scores for each subject before adding them to a combined data frame

# Reading excel
Data1 <- read_excel(files[f])

# Getting column number for relocating
col_num <- which(names(Data1) == column_name)


# Creating new columns
Data1 <- mutate(Data1, `z_score` = NA) %>%
  relocate(`z_score`, .after = col_num)


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
  filter(abs(z_score) < z_score_filter)


# Adding to the combined data frame

# If the combined data frame is empty, we want to make it the original file
combine_df <- Data2

# Writing excel sheet
write.xlsx(combine_df,paste0("Combined_z_scores_",column_name,".xlsx"),showNA = F)


