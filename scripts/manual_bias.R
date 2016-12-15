library(readxl)
library(ggplot2)

# Bias-----------------------------------------------------------

data.in <- read_excel("Validation_Workbook_VITE04.xlsx", sheet = "Bias", skip = 8)
r_bias = nrow(data.in)
bias <- data.in$Bias

bias <- na.omit(bias)


u_ref <- mean(data.in$pct_sd)/sqrt(mean(data.in$n))
ave_bias <- mean(data.in$pct_Bias)
sd_bias <- sd(data.in$pct_Bias)
UoB <- sqrt(u_ref^2 + sd_bias^2)

if(ave_bias > 2*UoB) {
        signif = "Significant"
        }else{
        signif = "Insignificant"
}

data.in$row_n <- as.numeric(rownames(data.in))

biasplot <- ggplot(data.in, aes(x=row_n, y= data.in$pct_Bias)) +
        geom_bar(position = "identity", stat = "identity", fill = "cornflowerblue", colour = "blue") +
        labs(title = "Percent Bias\n", y = "pct_Bias", x = "") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              legend.position = c(2.3,8), 
              text = element_text(size = 14))
biasplot
