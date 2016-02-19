###  This script takes a table of retested data and converts it to sets of pairs
###  of duplicates (repeatability) and relicates (reproducibility).

library(data.table)
library(readxl)

###  Input Data -------------------------------------------------------------
data.in <- read_excel("data/Validation_Workbook.xlsx", sheet = "ANOVA1")
       
###  Create dataframe shell -------------------------------------------------
df <- data.frame(
        A = numeric(),
        B = numeric())

###  Calculate repeatibility pairs ------------------------------------------
l <- length(data.in)
for (i in 1:l) {
        newt <- t(combn(data.in[,i],2))
        df <- rbind(df,newt)
}

###  Label repeatibility pairs ---------------------------------------------
df$Type <- "Repeatability"

###  Omit NAs caused by unequal columns of data ----------------------------
df <- na.omit(df)

###  Calculate reproducibility pairs ---------------------------------------
newdf <- rbindlist(lapply(seq_len(length(data.in) - 1), 
                          function(i) CJ(data.in[, i], unlist(data.in[, -(1:i)]))))

###  Omit NAs caused by unequal columns of data ----------------------------
newdf <- na.omit(newdf)

###  Label reproducibility pairs ------------------------------------------
newdf$Type <- "Reproducibility"

###  Combine the two sets -------------------------------------------------
output <- data.frame(rbind(df, newdf))

###  Re-order the columns -------------------------------------------------
output <- output[,c(3,1,2)]

###  Export to csv file ---------------------------------------------------
write.csv(output, "reports/combinations.csv", row.names=FALSE )
