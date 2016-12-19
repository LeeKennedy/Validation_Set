library(readxl)
library(ggplot2)
library(dplyr)

# Bias-----------------------------------------------------------

## Mac Data -----------------------------------------------------
data.in <- read_excel("~/Documents/GitHub/Validation_Set/data/Ammonia Validation Workbook.xlsx", 
                      sheet = "Bias", skip = 6)

colnames(data.in)[12] <- "pct_Bias"
colnames(data.in)[10] <- "pct_sd"
data.in <- data.in[,-13]

# Filter -----------------------------------------------------------------
data.in <- data.in %>% 
        filter(Type == "High Standard")

u_ref <- mean(data.in$pct_sd, na.rm = TRUE)/sqrt(mean(data.in$n, na.rm = TRUE))
ave_bias <- mean(data.in$pct_Bias, na.rm = TRUE)
sd_bias <- sd(data.in$pct_Bias, na.rm = TRUE)
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

