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

path <- "C:/Users/hughm/OneDrive - Belmont University/Personal/Desktop/VUMC/R01R21/Data"
participant <- c("CI211-preop")

setwd(path)
# Getting a list of all of the excel files
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")


# Import excel data
Data1 <- read_excel(files[1])
# Computing length of original column
length <- length(Data1$Word_num)

# Adding in extra rows to make it 160
if(length < 160){
  for(z in 1:(160 - length)){
    Data1[nrow(Data1) + 1,] <- NA
  }
}

# Creating# Creating a copy of the data frame
Data2 <- Data1
# Shifting rows around


for(i in 1:length){
  Data2[Data1$Word_num[i],] <- Data1[i,]
}

# Making rows blank otherwise
for(f in 1:160){
  if(f != Data2$Word_num[f]){
    Data2[f,] <- NA
  }
}


# Writing the new excel sheet to the other folder
write.xlsx(Data2, paste0("LD ",participant," Ordered.xlsx"),showNA = F)
