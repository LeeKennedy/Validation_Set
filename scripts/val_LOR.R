library("ProjectTemplate")
load.project()

# LOD-LOR-----------------------------------------------------------
LOR <- Validation.Workbook.LOD.LOR
#LOR <- LOR[,1:2]

colnames(LOR)[1] <- "Test"
colnames(LOR)[2] <- "Result"

LOR <- na.omit(LOR)

limit_d <- 3*sd(LOR$Result)
limit_r <- 10*sd(LOR$Result)
paste("Limit of Detection = ",round(limit_d,3), sep="")
paste("Limit of Reporting = ",round(limit_r,3), sep="")
