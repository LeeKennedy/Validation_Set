
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
        data3 <- read_excel("~/Documents/GitHub/Validation_Set/data/New_Validation_Workbook.xlsx", 
                            sheet = "XY_Comparison")
}

  
fit = lm(New ~ Old, data = data3)

summary(fit)

dataplot <- ggplot(data3, aes(x = Old, y = New)) + 
        geom_point(size=5, shape = 21, colour = "grey20", aes(fill = factor(Product))) + 
        geom_abline(intercept=coef(fit)[1], slope=coef(fit)[2]) + 
        annotate("text", label = paste("R2 = ",round(summary(fit)$r.squared,digits=4)), x = 7.5, y = 6) +
        geom_smooth(method = "lm") +
        geom_abline(slope=1, intercept = 0, colour = "red", lty = 2) +
        labs(title = paste(key[1,2], "Comparison: Old vs New\n", sep=""), x= "Old Method", y = "New Method") +
        theme_bw(base_size = 12, base_family = "Arial") +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              text = element_text(size = 14))
dataplot

ggsave(paste(key[1,2], " XY_plot.png", sep=""), width=12, height=12, dpi=100)

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 


