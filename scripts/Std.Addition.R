#load data
data1 <- read.csv("data/stdadd.csv", as.is = TRUE, header = TRUE)
library("ggplot2")
col1 <- "#CC3300"

#Do a correlation test on two components
cor.test(data1$Abs, data$Conc)

fit <- lm(data1$Abs ~ data1$Conc)
#fit <- lm(test$Y ~ test$X -1) #forces through zero
fit
# plot(fit) #plots residuals
summary(fit)

#fitted(fit) # returns the predicted values
#predict(fit, newdata = 1, se.fit = TRUE) # predicted values plus error
a <- coef(fit)
intercept <- a[1]/a[2]



p <- ggplot(data1, aes(x = Conc, y = Abs)) +
  geom_point(size = 4) + 
  stat_smooth(method = lm, se = TRUE, fullrange = TRUE) +
  geom_vline(xintercept = 0, colour="black", lwd = 0.5, linetype=1) +
  geom_vline(xintercept = intercept*-1, colour=col1, lwd = 0.75, linetype=2) + geom_hline(yintercept = 0, colour = "black", lwd = 0.5, linetype = 1) +
  annotate("text", label = round(intercept,3), x = -1*intercept, y = 0.2*max(data1[,2]))

science_theme = theme(panel.grid.major = element_line(size = 0.5, color = "grey"), axis.line = element_line(size = 0.7, color = "black"), legend.position = c(2.3,8), text = element_text(size = 14))
p <- p + 
  theme_bw() +
  science_theme
p

intercept

