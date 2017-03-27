# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Functions --------------------------------------------------------------

remove_outliers <- function(x, na.rm = TRUE, ...) {
 qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
 y <- x
 y[x < (qnt[1] - H)] <- NA
 y[x > (qnt[2] + H)] <- NA
 y
}

# Data Input -------------------------------------------------------------

Input <- read_excel("~/Desktop/Lactoferrin/Validation Workbook July 2016.xlsx", 
                         sheet = "ANOVA")

# ANOVA ------------------------------------------------------------------
#Always look at the data

dev.off()
boxplot(Input,
        frame = TRUE,
        cex.axis = 1.5,
        cex.main = 2,
        outpch=16,
        outcol = "red")

#convert to a two element stack
xs <- na.omit(stack(Input))

# Run ANOVA
anova1 <- aov(values ~ ind, data = xs)

#Review results
summary(anova1)

#Check for significant differences
TukeyHSD(anova1)

#Repeatability & Interim Precision
mean.sqr <- summary(anova1)[1][[1]][[3]]
ncount <- as.numeric(length(anova1$effects))/as.numeric(length(anova1$coefficients))
sdr <- sqrt(mean.sqr[2])
interim <- sqrt((mean.sqr[1]-mean.sqr[2])/ncount)
sdR <- sqrt(sdr^2 + interim^2)
sdr
sdR

#---------------------------------------------------------------------------------


Mean <- mean(xs$values)
UCL <- Mean + 3*sdR
UWL <- Mean + 2*sdR
LWL <- Mean - 2*sdR
LCL <- Mean - 3*sdR

dev.off()

boxplot(Input,
        ylim = c(0.95*LCL,1.05*UCL))

abline(h=Mean, lty=5, lwd=1.5, col = "blue")
abline(h=UCL, lty=5, lwd=1.5, col = "red")
abline(h=LCL, lty=5, lwd=1.5, col = "red")
abline(h=UWL, lty=5, lwd=1.5, col = "darkgreen")
abline(h=LWL, lty=5, lwd=1.5, col = "darkgreen")

plot_anova = ggplot(xs, aes(x=ind, y=values)) +
        geom_point(size=5, shape = 21, colour = "black", fill = "cornflowerblue") +
        geom_hline(aes(yintercept=Mean),lty=5, col = "blue") +
        geom_hline(aes(yintercept=UCL),lty=5, col = "red") +
        geom_hline(aes(yintercept=LCL),lty=5, col = "red") +
        geom_hline(aes(yintercept=UWL),lty=5, col = "darkgreen") +
        geom_hline(aes(yintercept=LWL),lty=5, col = "darkgreen") +
        scale_y_continuous(limits = c(0.997*LCL,1.003*UCL)) +
        labs(x="Batch", y="Percent Lactoferrin", title = "Lactoferrin Results") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              legend.position = c(2.3,8), 
              text = element_text(size = 14))

plot_anova

