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

# Clearing the console of previous junk
shell("cls")
# shell("clear")
# Clearing the environment of previous variables
rm(list=ls()) 
# path <- "C:/Users/hughm/OneDrive - VUMC/General - Cochlear Implant Cognition and Communication Lab/R01+R21 Outcomes Studies/Data Collection/Subject testing/Cochlear Implant/CI200- McClure/06.08.2023- preop/CI200 06.08.23 Matlab Tasks/Inquisit Lab v6"
path <- "C:/Users/hughm/OneDrive - Belmont University/Personal/Desktop/VUMC/R01R21/Data"

setwd(path)
files <- list.files(full.names = T)
# Removing initial ./ in names
files <- gsub(x = files, pattern = "./", replacement = "")
# Renaming spreadsheets for organization based on the task name
filesEx <- gsub(x = files, pattern = ".iqdat", replacement = ".xlsx")
for(i in 1:length(files)){
  data <- read.table(paste0(path,"/",files[i]),fill = TRUE)
  write.xlsx(data, paste0(path,"/",filesEx[i]),showNA = F)
}
