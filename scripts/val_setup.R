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

<<<<<<< Updated upstream
location <- "Y:/Validation and Verification of methods/Chemistry/Project 239 - VIB501 submission to NATA/1. Data/VIB501_Validation_Workbook_2017.xlsx"
=======
location <- "~/Desktop/GB Taurine/Taurine_Validation_Workbook.xlsx"
>>>>>>> Stashed changes

key <- read_excel(location,  sheet = "Key")

# Functions --------------------------------------------------------------

remove_outliers <- function(x, na.rm = TRUE, ...) {
        qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
        H <- 1.5 * IQR(x, na.rm = na.rm)
        y <- x
        y[x < (qnt[1] - H)] <- NA
        y[x > (qnt[2] + H)] <- NA
        y
}