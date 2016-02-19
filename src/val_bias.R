library(readxl)
library(ggplot2)
library(broom)
vfile <- "data/FRUCTANS.xlsx"
sheets <- excel_sheets(vfile)
sheets

# Bias-----------------------------------------------------------
bias <- read_excel(vfile, sheets[5], skip = 8)

u_ref <- mean(bias$"% sd")/sqrt(mean(bias$n))
ave_bias <- mean(bias$"% Bias")
sd_bias <- sd(bias$"% Bias")
UoB <- sqrt(u_ref^2 + sd_bias^2)

if(ave_bias > 2*UoB) {
        signif = "Significant"
        }else{
        signif = "Insignificant"
}

bias$row_n <- as.numeric(rownames(bias))

biasplot <- ggplot(bias, aes(x=row_n, y= bias$"% Bias")) +
        geom_bar(position = "identity", stat = "identity", fill = "cornflowerblue", colour = "blue") +
        labs(title = "Percent Bias\n", y = "% Bias", x = "") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              legend.position = c(2.3,8), 
              text = element_text(size = 14))
biasplot

png(filename = paste("graphs/","Bias.png",sep=""),
    width = 1000, height = 550, units = "px", pointsize = 12)
plot(biasplot)
dev.off()
