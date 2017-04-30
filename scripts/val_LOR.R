
# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)



# Data Input -------------------------------------------------------------

if("Windows" %in% Sys.info()['sysname'] == TRUE){ 
        x = "something"
} else { 
        key <- read_excel("~/Documents/GitHub/Validation_Set/data/New_Validation_Workbook.xlsx", 
                          sheet = "Key")
        LOR <- read_excel("~/Documents/GitHub/Validation_Set/data/New_Validation_Workbook.xlsx", 
                          sheet = "LOD-LOR", skip = 3)
}

LOR <- LOR[,1:2]


colnames(LOR)[1] <- "Test"
colnames(LOR)[2] <- "Result"

LOR <- na.omit(LOR)

limit_d <- 3*sd(LOR$Result)
limit_r <- 10*sd(LOR$Result)


