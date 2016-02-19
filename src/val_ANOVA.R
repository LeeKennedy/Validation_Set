library(readxl)
library(ggplot2)
library(broom)
vfile <- "data/FRUCTANS.xlsx"
sheets <- excel_sheets(vfile)
sheets

# STOP--------------Set m = desired sheet number------------------
m <- 4

# ANOVA-----------------------------------------------------------
anova <- read_excel(vfile, sheets[m])
anova

# STOP--------------Set rows and columns for ANOVA data-----------
cols <- 2
rows <- 5

Input <- anova[1:rows, 1:cols]

for (i in 1:cols) {
        Input[,i] = as.numeric(unlist(Input[,i]))
}


#Always look at the data

png(filename = paste0(sheets[m], "-Boxplot.png", sep=""),    width = 1000, height = 550, units = "px", pointsize = 12)

boxplot(Input,
        frame = TRUE,
        cex.axis = 1.5,
        cex.main = 2,
        outpch=16,
        outcol = "red",
        main = sheets[m])
dev.off()

# Review data frame
summary(Input)

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

boxplot(Input,
        ylim = c(0.95*LCL,1.05*UCL))

abline(h=Mean, lty=5, lwd=1.5, col = "blue")
abline(h=UCL, lty=5, lwd=1.5, col = "red")
abline(h=LCL, lty=5, lwd=1.5, col = "red")
abline(h=UWL, lty=5, lwd=1.5, col = "darkgreen")
abline(h=LWL, lty=5, lwd=1.5, col = "darkgreen")

library(ggplot2)
plot = ggplot(xs, aes(x=ind, y=values)) +
        geom_point(size=5, colour = "cornflowerblue") +
        geom_hline(aes(yintercept=Mean),lty=5, col = "blue") +
        geom_hline(aes(yintercept=UCL),lty=5, col = "red") +
        geom_hline(aes(yintercept=LCL),lty=5, col = "red") +
        geom_hline(aes(yintercept=UWL),lty=5, col = "darkgreen") +
        geom_hline(aes(yintercept=LWL),lty=5, col = "darkgreen") +
        scale_y_continuous(limits = c(0.95*LCL,1.05*UCL)) +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              legend.position = c(2.3,8), 
              text = element_text(size = 14))

plot

