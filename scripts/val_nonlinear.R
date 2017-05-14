
# Data Input -------------------------------------------------------------

linearity <- read_excel(location, sheet = "Linearity")


# Linearity-----------------------------------------------------------
col1 <- colnames(linearity[1])
col2 <- colnames(linearity[2])
colnames(linearity)[1] <- "A"
colnames(linearity)[2] <- "B"

# Cubic Fit ----------------------------------------------------------
lin <- lm(linearity$B ~ linearity$A + I(linearity$A^2) + I(linearity$A^3))

lin1 <- summary(lin)
lin2 <- tidy(lin)
R2 <- lin1$r.squared

dev.off()

linplot = ggplot(linearity, aes(x = A, y = B)) + 
        geom_point(size=5, shape = 21, colour = "darkgreen", fill = "cornflowerblue") + 
        geom_smooth(method = glm, formula = y ~ x + I(x^2) + I(x^3)) + 
        annotate("text", 
                 label = paste("R2 = ",
                 round(summary(lin)$r.squared,digits=4)), 
                 x = 0.5*max(linearity[,1]), y = 0.8*max(linearity[,2])) +
       labs(title = paste(key[1,2]," Linearity", sep=""), subtitle = "Batch VB1201-17-0053", caption = "Cubic Least Squares Fit",  x = key[2,2], y=key[3,2]) +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              legend.position = c(2.3,8), 
              text = element_text(size = 14))

linplot

ggsave(paste(key[1,2], "_linearity.png", sep=""), width=12, height=6, dpi=100)


lin2
R2


# 95% CI for co-efficients-----------------------------------------
coefficients(lin) # model coefficients
confint(lin, level=0.95) # CIs for model parameters
CI_curve <- confint(lin, level=0.95) 

