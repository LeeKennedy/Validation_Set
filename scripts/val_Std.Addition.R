library("ProjectTemplate")
load.project()

data_stdadd <- Validation.Workbook.Std.Add

col1 <- "#CC3300"

#Do a correlation test on two components
cor.test(data_stdadd$Abs, data_stdadd$Addition)


fit <- lm(data_stdadd$Abs~data_stdadd$Addition)
#fit <- lm(test$Y ~ test$X -1) #forces through zero
fit
# plot(fit) #plots residuals
summary(fit)

#fitted(fit) # returns the predicted values
#predict(fit, newdata = 1, se.fit = TRUE) # predicted values plus error
a <- coef(fit)
intercept <- a[1]/a[2]



p <- ggplot(data_stdadd, aes(x = Addition, y = Abs)) +
  geom_point(size = 4) + 
  stat_smooth(method = lm, se = TRUE, fullrange = TRUE) +
  geom_vline(xintercept = 0, colour="black", lwd = 0.5, linetype=1) +
  geom_vline(xintercept = intercept*-1, colour=col1, lwd = 0.75, linetype=2) + 
        geom_hline(yintercept = 0, colour = "black", lwd = 0.5, linetype = 1) +
  annotate("text", label = round(intercept,3), x = -1*intercept, y = 0.2*max(data_stdadd[,2]))

science_theme = theme(panel.grid.major = element_line(size = 0.5, color = "grey"), axis.line = element_line(size = 0.7, color = "black"), legend.position = c(2.3,8), text = element_text(size = 14))
p <- p + 
  theme_bw() +
  science_theme
p

intercept

