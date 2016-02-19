library(ggplot2)
library (readxl)

data1 <- read_excel("data/Validation_Workbook.xlsx", sheet = "Linearity")
  
# First Pass - includes variable intercept - if CI overlaps zero, rerun forced through zero.
fit = lm(Response ~ Conc, data = data1)
#Remove the -1 if you do not want the curve forced through zero
#fit = lm(Response ~ Conc-1, data = data1)
summary(fit)

dataplot = ggplot(data = data1, aes(x = Conc, y = Response)) + 
  geom_point(size=5, shape = 21, colour = "darkgreen") + 
  geom_abline(intercept=coef(fit)[1], slope=coef(fit)[2]) + 
  stat_smooth(method = "lm", fullrange = TRUE) +
  annotate("text", label = paste("R2 = ",round(summary(fit)$r.squared,digits=4)), x = 36, y = 10) +
  xlab("Values of X") +
  ylab("Values of Y") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_bw(base_size = 12, base_family = "Arial") +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              legend.position = c(2.3,8), 
              text = element_text(size = 14))
dataplot

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
CI_curve <- confint(fit, level=0.95) 


