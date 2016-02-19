library (readxl)

data2 <- read_excel("data/Validation_Workbook.xlsx", sheet = "LOD_LOR")

n <- nrow(data2)

SD <- sd(data2$Result)

chi_low <- qchisq(.975, df=(n-1))
chi_high <- qchisq(.025, df=(n-1))

SD_low <- sqrt(((n-1)*SD^2)/chi_low)
SD_high <- sqrt(((n-1)*SD^2)/chi_high)


#-------adjust Vol & sample weight to reflect weight & volume used -------------------

######>>>> Run Standard.curve.R first to get blank & slope----------------------------

Vol <- 10
sam_wt <- 1.8
blank <- as.numeric(coef(fit)[1])
slope <- as.numeric(coef(fit)[2])

LOD <- Vol*(blank + 3 * SD)/(slope * sam_wt)
LOR <- Vol*(blank + 10 * SD)/(slope * sam_wt)

LOD_low_ci <- ((blank + 3 * SD_low)/slope)*sqrt(  (0.5*(CI_curve[1,2]-CI_curve[1,1])/blank)^2 + (0.5*(CI_curve[2,2]-CI_curve[2,1])/slope)^2  )
LOD_low <- Vol*(blank + 3 * SD_low)/(slope * sam_wt) - LOD_low_ci
LOD_high_ci <- ((blank + 3 * SD_high)/slope)*sqrt(  (0.5*(CI_curve[1,2]-CI_curve[1,1])/blank)^2 + (0.5*(CI_curve[2,2]-CI_curve[2,1])/slope)^2  )
LOD_high <- Vol*(blank + 3 * SD_high)/(slope * sam_wt) + LOD_high_ci

LOR_low_ci <- ((blank + 10 * SD_low)/slope)*sqrt(  (0.5*(CI_curve[1,2]-CI_curve[1,1])/blank)^2 + (0.5*(CI_curve[2,2]-CI_curve[2,1])/slope)^2  )
LOR_low <- Vol*(blank + 10 * SD_low)/(slope * sam_wt) - LOR_low_ci
LOR_high_ci <- ((blank + 10 * SD_high)/slope)*sqrt(  (0.5*(CI_curve[1,2]-CI_curve[1,1])/blank)^2 + (0.5*(CI_curve[2,2]-CI_curve[2,1])/slope)^2  )
LOR_high <- Vol*(blank + 10 * SD_high)/(slope * sam_wt) + LOR_high_ci

LOD_LOR <- data.frame(
        Min = numeric(),
        Raw = numeric(),
        Max = numeric())


LOD_LOR[1,1] <- LOD_low
LOD_LOR[1,2] <- LOD
LOD_LOR[1,3] <- LOD_high
LOD_LOR[2,1] <- LOR_low
LOD_LOR[2,2] <- LOR
LOD_LOR[2,3] <- LOR_high

rownames(LOD_LOR) <- c("LOD","LOR")

round(LOD_LOR,2)

