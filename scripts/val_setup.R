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

location <- "~/Desktop/VB1201/Vitamin_B12_Validation_Workbook.xlsx"

if("Windows" %in% Sys.info()['sysname'] == TRUE){ 
        x = "something"
} else { 
        key <- read_excel(location,  sheet = "Key")

}

# Functions --------------------------------------------------------------

remove_outliers <- function(x, na.rm = TRUE, ...) {
        qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
        H <- 1.5 * IQR(x, na.rm = na.rm)
        y <- x
        y[x < (qnt[1] - H)] <- NA
        y[x > (qnt[2] + H)] <- NA
        y
}