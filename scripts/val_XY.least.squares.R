
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

data3 <- read_excel("~/Documents/GitHub/Validation_Set/data/New_Validation_Workbook.xlsx", 
                    sheet = "XY_Comparison")
  
fit = lm(New ~ Old, data = data3)

summary(fit)

dataplot <- ggplot(data3, aes(x = Old, y = New)) + 
        geom_point(size=5, shape = 21, colour = "grey20", aes(fill = factor(Product))) + 
        geom_abline(intercept=coef(fit)[1], slope=coef(fit)[2]) + 
        annotate("text", label = paste("R2 = ",round(summary(fit)$r.squared,digits=4)), x = 7.5, y = 6) +
        geom_smooth(method = "lm") +
        geom_abline(slope=1, intercept = 0, colour = "red", lty = 2) +
        xlab("Old method") +
        ylab("New method") +
        ggtitle("Comparison: Old vs New\n") +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        theme_bw(base_size = 12, base_family = "Arial") +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              text = element_text(size = 14))
dataplot

ggsave("XY_plot_test1.png", width=12, height=6, dpi=100)

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 


