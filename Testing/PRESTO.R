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

Data1$Response<-gsub("..."," ",x = Data1$Response,fixed = TRUE)
Data1$ANSWER<-gsub(",","",x = Data1$ANSWER,fixed = TRUE)
Data1$Response<-gsub(",","",x = Data1$Response,fixed = TRUE)
Data1$Response<-gsub(".","",x = Data1$Response,fixed = TRUE)
Data1$ANSWER<-gsub(".","",x = Data1$ANSWER,fixed = TRUE)

length <- length(Data1$ANSWER)

for(i in 1:length){
  x1 <- tolower(Data1$ANSWER[i])
  x2 <- tolower(Data1$Response[i])
  
  x1_split <- unlist(strsplit(x1,split= " "))
  x1_split
  
  x2_split <- unlist(strsplit(x2,split= " "))
  x2_split
  
  if(grepl("-",x1)){
    Data1$`# words correct`[i] <- length(intersect(x1_split,x2_split)) + 1
  }
  else{
    Data1$`# words correct`[i] <- length(intersect(x1_split,x2_split))
  }
  

}

Data1$`Percentage Words Correct` <- NA

Data1$`Percentage Words Correct`[1] <- sum(Data1$`# words correct`[1:length])/sum(Data1$`# words total`[1:length])

Data1$`Total Words Correct` <- NA

Data1$`Total Words Correct`[1] <- sum(Data1$`# words correct`[1:length])

write.xlsx(Data1, paste0(filename,"_Scored.xlsx"),showNA = F)

