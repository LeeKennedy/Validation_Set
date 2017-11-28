# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(dts.quality)
library(tidyverse)

# Data Input -------------------------------------------------------------

location <- "/Volumes/Samsung USB/Project 236 - Iodine by GB method/1. Data/Project 236 - Iodine.xlsx"

key <- read_excel(location,  sheet = "Key")

# Functions --------------------------------------------------------------


