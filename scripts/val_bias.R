library("ProjectTemplate")
load.project()

# Bias-----------------------------------------------------------
r_bias = nrow(Validation.Workbook.Bias)
bias <- Validation.Workbook.Bias

bias <- na.omit(bias)


u_ref <- mean(bias$pct_sd)/sqrt(mean(bias$n))
ave_bias <- mean(bias$pct_Bias)
sd_bias <- sd(bias$pct_Bias)
UoB <- sqrt(u_ref^2 + sd_bias^2)

if(ave_bias > 2*UoB) {
        signif = "Significant"
        }else{
        signif = "Insignificant"
}

bias$row_n <- as.numeric(rownames(bias))

biasplot <- ggplot(bias, aes(x=row_n, y= bias$pct_Bias)) +
        geom_bar(position = "identity", stat = "identity", fill = "cornflowerblue", colour = "blue") +
        labs(title = "Percent Bias\n", y = "pct_Bias", x = "") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              legend.position = c(2.3,8), 
              text = element_text(size = 14))
biasplot
