
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

linearity <- read_excel("~/Documents/GitHub/Validation_Set/data/New_Validation_Workbook.xlsx", 
                        sheet = "Linearity")


# Linearity-----------------------------------------------------------
col1 <- colnames(linearity[1])
col2 <- colnames(linearity[2])
colnames(linearity)[1] <- "A"
colnames(linearity)[2] <- "B"

lin <- lm(linearity$B~linearity$A)
lin1 <- summary(lin)
lin2 <- tidy(lin)
R2 <- lin1$r.squared

dev.off()

linplot = ggplot(linearity, aes(x = A, y = B)) + 
        geom_point(size=5, shape = 21, colour = "darkgreen") + 
        geom_abline(intercept=coef(lin)[1], slope=coef(lin)[2]) + 
        stat_smooth(method = "lm", fullrange = TRUE) +
        annotate("text", 
                 label = paste("R2 = ",
                 round(summary(lin)$r.squared,digits=4)), 
                 x = 0.5*max(linearity[,1]), y = 0.8*max(linearity[,2])) +
       labs(x=col1, y=col2, title = "Linearity", subtitle = "The grey zone is the 95% Confidence Interval") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              legend.position = c(2.3,8), 
              text = element_text(size = 14))

linplot

ggsave("lin_plot_test1.png", width=12, height=6, dpi=100)


lin2
R2


# 95% CI for co-efficients-----------------------------------------
coefficients(lin) # model coefficients
confint(lin, level=0.95) # CIs for model parameters
CI_curve <- confint(lin, level=0.95) 

