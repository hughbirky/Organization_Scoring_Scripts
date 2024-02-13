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



# Clearing the console of previous junk
shell("cls")
# shell("clear")
# Clearing the environment of previous variables
rm(list=ls()) 

path <- "C:/Users/hughm/OneDrive - Belmont University/Personal/Desktop/VUMC/R01R21/Rhyme_Judgement"
participant <- c("CI211- preop")

setwd(path)
# Getting a list of all of the excel files
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")

### Reading in the template
template <- read_excel(files[1])
# Finding length of the template
lengthTemp <- length(template$`# TrialWord1`)
# Import excel data
Data1 <- read.csv(files[2])
# Computing length of original column
length <- length(Data1$X..TrialWord1)



# Adding in extra rows to make it 160
if(length < lengthTemp){
  for(z in 1:(lengthTemp - length)){
    Data1[nrow(Data1) + 1,] <- NA
  }
}

# Creating a copy of the data frame
Data2 <- Data1

i = 1
# Reorganizing
for (i in 1:length){
  # Getting the word to sort
  word <- Data1$X..TrialWord1[i]
  # Checking what row that is in the template
  row <- which(template==word,arr.ind=TRUE)
  row[1]
  
  Data2[row[1],] <- Data1[i,]
}
  
  

# Making rows blank otherwise
for(f in 1:length(Data2$X..TrialWord1)){
  if(template$`# TrialWord1`[f] != Data2$X..TrialWord1[f]){
    Data2[f,] <- NA
  }
}


# Writing the new excel sheet to the other folder
write.xlsx(Data2, paste0("Rhyme ",participant," Ordered.xlsx"),showNA = F)
