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

shell("cls")
# shell("clear")
# Clearing the environment of previous variables
rm(list=ls()) 

path <- "C:/Users/hughm/Desktop/Convert"
outputPathCopy <- "C:/Users/hughm/Desktop/Convert"


setwd(path)
# Getting a list of all of the excel files
files = list.files(full.names = T)
# Getting rid of the ./
files <- gsub(x = files, pattern = "./", replacement = "")
files <- files[!grepl("mp3", files)]

i = 1
# Converting every file
for(i in 1:length(files)){
  # Creating a string of the path to the file to be converted
  input <- paste0(outputPathCopy,"/",files[i])
  # Creating a string of the path to where we want to export
  outputpath <- paste0(outputPathCopy, "/",gsub(x = files[i], pattern = ".wav", replacement = ".mp3"))
  # Creating command for command prompt and executing
  # cmd <- sprintf("ffmpeg -i %s -vn -ar 44100 -ac 2 -b:a 192k %s", input, outputpath)
  cmd <- sprintf("ffmpeg -i %s -vn %s", input, outputpath)
  # Calling the system command
  system(cmd)
}