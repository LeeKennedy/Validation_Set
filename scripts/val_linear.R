library("ProjectTemplate")
load.project()

# Linearity-----------------------------------------------------------
linearity <- Validation.Workbook.Linearity

col1 <- colnames(linearity[1])
col2 <- colnames(linearity[2])
colnames(linearity)[1] <- "A"
colnames(linearity)[2] <- "B"

lin <- lm(linearity$B~linearity$A)
lin1 <- summary(lin)
lin2 <- tidy(lin)
R2 <- lin1$r.squared

linplot = ggplot(linearity, aes(x = A, y = B)) + 
        geom_point(size=5, shape = 21, colour = "darkgreen") + 
        geom_abline(intercept=coef(lin)[1], slope=coef(lin)[2]) + 
        stat_smooth(method = "lm", fullrange = TRUE) +
        annotate("text", label = paste("R2 = ",round(summary(lin)$r.squared,digits=4)), x = 0.5*max(linearity[,1]), y = 0.8*max(linearity[,2])) +
       labs(x=col1, y=col2, title = "Linearity\n") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              legend.position = c(2.3,8), 
              text = element_text(size = 14))
linplot

png(filename = paste("graphs/","Linearity.png",sep=""),
    width = 1000, height = 550, units = "px", pointsize = 12)
plot(linplot)
dev.off()
lin2
R2

# 95% CI for co-efficients-----------------------------------------
coefficients(lin) # model coefficients
confint(lin, level=0.95) # CIs for model parameters
CI_curve <- confint(lin, level=0.95) 
