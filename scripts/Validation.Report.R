library(rmarkdown)
library(readxl)
library(readxl)
library(ggplot2)
library(tidyr)
library(broom)

# -- insert analyte ----------------------------------
code <- "Blahblah"
workbook_name <- "Validation_Workbook.xlsx"

linearity <- read_excel(paste("~/Documents/GitHub/Validation_Set/data/", workbook_name, sep=""), sheet=1)
LOR <- read_excel(paste("~/Documents/GitHub/Validation_Set/data/", workbook_name, sep=""), sheet=2)
data_anova <- read_excel(paste("~/Documents/GitHub/Validation_Set/data/", workbook_name, sep=""), sheet=3)
bias <- read_excel(paste("~/Documents/GitHub/Validation_Set/data/", workbook_name, sep=""), sheet=4)
data3 <- read_excel(paste("~/Documents/GitHub/Validation_Set/data/", workbook_name, sep=""), sheet=5)
data_stdadd <- read_excel(paste("~/Documents/GitHub/Validation_Set/data/", workbook_name, sep=""), sheet=6)

rmarkdown::render("~/Documents/GitHub/Validation_Set/reports/VAL.RMarkdown.Report.Rmd", 
        output_format = "word_document",
        output_file = paste(code, "-Validation Report-",Sys.Date(), ".docx", sep=""),
        output_dir = "~/Documents/GitHub/Validation_Set/reports")
                




