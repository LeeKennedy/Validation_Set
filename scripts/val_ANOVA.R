
# Data Input -------------------------------------------------------------

aov_input <- read_excel(location, sheet = "ANOVA") 

aov_input <- aov_input[rowSums(is.na(aov_input)) != ncol(aov_input),]


# ANOVA ------------------------------------------------------------------
#Always look at the data


boxplot(aov_input,
        frame = TRUE,
        cex.axis = 1.5,
        cex.main = 2,
        outpch=16,
        outcol = "red")

#convert to a two element stack
xs <- na.omit(stack(aov_input))

# Run ANOVA
anova1 <- aov(values ~ ind, data = xs)

#Review results
summary(anova1)

#Check for significant differences
TukeyHSD(anova1)

#Repeatability & Interim Precision
mean.sqr <- summary(anova1)[1][[1]][[3]]
ncount <- as.numeric(length(anova1$effects))/as.numeric(length(anova1$coefficients))
sdr <- sqrt(mean.sqr[2])
interim <- sqrt((mean.sqr[1]-mean.sqr[2])/ncount)
sdR <- sqrt(sdr^2 + interim^2)
sdr
sdR

#---------------------------------------------------------------------------------


Mean <- mean(xs$values)
UCL <- Mean + 3*sdR
UWL <- Mean + 2*sdR
LWL <- Mean - 2*sdR
LCL <- Mean - 3*sdR

plot_anova = ggplot(xs, aes(x=ind, y=values)) +
        geom_point(size=5, shape = 21, colour = "black", fill = "cornflowerblue") +
        geom_hline(aes(yintercept=Mean),lty=5, col = "blue") +
        geom_hline(aes(yintercept=UCL),lty=5, col = "red") +
        geom_hline(aes(yintercept=LCL),lty=5, col = "red") +
        geom_hline(aes(yintercept=UWL),lty=5, col = "darkgreen") +
        geom_hline(aes(yintercept=LWL),lty=5, col = "darkgreen") +
        scale_y_continuous(limits = c(0.96*LCL,1.04*UCL)) +
        labs(x="Batch", y= key[2,2], title = paste(key[1,2], " Results\n")) +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              legend.position = c(2.3,8), 
              text = element_text(size = 14))

plot_anova

ggsave(paste( key[1,2],"_anova_plot.png", sep=""), width=12, height=6, dpi=100)


