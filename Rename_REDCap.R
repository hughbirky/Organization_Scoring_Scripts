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
library(readr)


# Clearing the environment of previous variables
rm(list=ls()) 

# Clearing the console of previous junk
shell("cls")

path <- "C:/Users/hughm/Dropbox/Cochlear_IIR_Project/Analysis/Scoring/Scoring_in_Progress/CI/REDCap/Raw Data"
path <- "C:/Users/hughm/Dropbox/Cochlear_IIR_Project/Analysis/Scoring/Scoring_in_Progress/CI/REDCap/Labels"


setwd(path)


# Getting a list of all of the files
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")
files <- files[grepl(".csv", files)]


f = 1
for(f in 1:length(files)){
  Data1 <- read.csv(files[f])
  
  title <- colnames(Data1)[3]
  title <- gsub(x = title, pattern = "_timestamp", replacement = "")
  
  write.xlsx(Data1, paste0(title,"_04.22.2024.xlsx"),showNA = F)
}