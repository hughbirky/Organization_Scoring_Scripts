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
library(rJava)
library(rChoiceDialogs)
library(shiny)


shell("cls")
# shell("clear")
# Clearing the environment of previous variables
rm(list=ls()) 

path <- "C:/Users/hughm/OneDrive - Belmont University/Personal/Desktop/VUMC/R01R21/Data"

setwd(path)
# Getting a list of all of the excel files
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")
filename <- gsub(x = files, pattern = ".xlsx", replacement = "")



# Import excel data
Data1 <- read_excel(files[1])

Data1$Answer<-gsub(".","",x = Data1$Answer,fixed = TRUE)
Data1$Answer<-gsub(",","",x = Data1$Answer,fixed = TRUE)
i = 1
for(i in 1:20){
  x1 <- tolower(Data1$Answer[i])
  x2 <- tolower(Data1$Response[i])
  
  x1_split <- unlist(strsplit(x1,split= " "))
  x1_split
  
  x2_split <- unlist(strsplit(x2,split= " "))
  x2_split
  
  if(grepl("-",x1)){
    Data1$`# Words Correct`[i] <- length(intersect(x1_split,x2_split)) + 1
  }
  else{
    Data1$`# Words Correct`[i] <- length(intersect(x1_split,x2_split))
  }
  
  
  x1 <- tolower(Data1$Keywords[i])
  
  x1_split <- unlist(strsplit(x1,split= " "))
  x1_split
  if(grepl("-",x1)){
    Data1$`# Keywords Correct`[i] <- length(intersect(x1_split,x2_split)) + 1
  }
  else{
    Data1$`# Keywords Correct`[i] <- length(intersect(x1_split,x2_split))
  }
}

Data1$`# Keywords Correct`[22] <- sum(Data1$`# Keywords Correct`[1:20])/sum(Data1$`Total Keywords`[1:20])

Data1$`# Words Correct`[22] <- sum(Data1$`# Words Correct`[1:20])/sum(Data1$`Total Words`[1:20])
write.xlsx(Data1, paste0(filename,"_Scored.xlsx"),showNA = F)

