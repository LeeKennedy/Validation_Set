library(rmarkdown)
library(readxl)
library(readxl)
library(ggplot2)
library(tidyr)
library(broom)

# -- insert analyte ----------------------------------
code <- "Blahblah2"
#workbook_name <- "Validation_Workbook_2.xlsx"
workbook_name <- "Validation_Workbook.xlsx"
units <- "mg/kg"
style <- 1      # 1 = Word document, 2 = web document


# Things to watch out for: --------------------------------
# 1. Is workbook name correct?
# 2. Is the report name correct?
# 3. Have all the comments been written?
# 4. Change rounding in LOR/LOD if necessary.

# Import the data ---------------------------------------------
linearity <- read_excel(paste("~/Documents/GitHub/Validation_Set/data/", workbook_name, sep=""), sheet=1)
LOR <- read_excel(paste("~/Documents/GitHub/Validation_Set/data/", workbook_name, sep=""), sheet=2)
data_anova <- read_excel(paste("~/Documents/GitHub/Validation_Set/data/", workbook_name, sep=""), sheet=3)
bias <- read_excel(paste("~/Documents/GitHub/Validation_Set/data/", workbook_name, sep=""), sheet=4)
data3 <- read_excel(paste("~/Documents/GitHub/Validation_Set/data/", workbook_name, sep=""), sheet=5)
data_stdadd <- read_excel(paste("~/Documents/GitHub/Validation_Set/data/", workbook_name, sep=""), sheet=6)


# Run report -------------------------------------------------
if(style == 1) {
        output <- "word_document"
        doc_suffix <- ".docx"      
}else{
        output <- "html_document"
        doc_suffix <- ".html"
}
rmarkdown::render("~/Documents/GitHub/Validation_Set/reports/VAL.RMarkdown.Report.Rmd", 
        output_format = output,
        output_file = paste(code, "-Validation Report-",Sys.Date(), doc_suffix, sep=""),
        output_dir = "~/Documents/GitHub/Validation_Set/reports")
                




