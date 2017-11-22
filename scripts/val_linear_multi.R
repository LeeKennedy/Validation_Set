
# Data Input -------------------------------------------------------------

linearity <- read_excel(location, sheet = "Multi_Linearity")

n <- nrow(linearity)
m <- ncol(linearity)-1

linearity$Average <- rowMeans(linearity[-1])

linearity2 <- linearity
linearity <- linearity[,-(m+2)]

# Linearity of average data points ---------------------------------------

lin_data <- tidyr::gather(data = linearity, key = Batch, value = Result, na.rm = FALSE, -Conc)

lin <- lm(linearity2$Average~linearity2$Conc)
lin1 <- summary(lin)
lin2 <- tidy(lin)
R2 <- lin1$r.squared

#dev.off()

linplot = ggplot(lin_data, aes(x = Conc, y = Result, fill = Batch)) + 
        geom_point(size=5, shape = 21,  col = "black") + 
        geom_smooth(method = lm, se = FALSE, col="grey70")+
        geom_abline(intercept=coef(lin)[1], slope=coef(lin)[2], lty = 2, col = "blue") + 
        labs(title = paste(key[1,2]," Linearity", sep=""), x = key[2,2], y=key[3,2]) +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"),
              text = element_text(size = 14))

linplot

#ggsave(paste(key[1,2], "_linearity_multi.png", sep=""), width=12, height=6, dpi=100)


lin2
R2


# 95% CI for co-efficients-----------------------------------------
coefficients(lin) # model coefficients
confint(lin, level=0.95) # CIs for model parameters
CI_curve <- confint(lin, level=0.95) 


#### Calculate the R2 values for all curves -----------------------
aR2 <- rep(0,m)

for (k in 1:m) {
        alin <- lm(as.matrix(linearity[1:n,k+1])~as.matrix(linearity[1:n,1]))
        alin1 <- summary(alin)
        alin2 <- tidy(alin)
        aR2[k] <- alin1$r.squared    
        
}
aR2
