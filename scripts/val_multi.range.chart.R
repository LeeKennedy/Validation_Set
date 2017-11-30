# Data Input -------------------------------------------------------------

data <- read_excel(location, sheet = "IRM_Comparison")


remove.outliers <- function(x, na.rm = TRUE, ...) {
        qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
        H <- 1.5 * IQR(x, na.rm = na.rm)
        y <- x
        y[x < (qnt[1] - H)] <- NA
        y[x > (qnt[2] + H)] <- NA
        y
}


colnames(data)[1] <- "Sample"
#data$LOGIN_DATE <- dmy_hm(data$LOGIN_DATE)

data <- data %>% 
        filter(Omit == "No")

data2 <- data %>%
        group_by(ANALYSIS)%>%
        mutate(ENTRY = remove.outliers(ENTRY))%>%
        na.omit()%>%
        summarise(Mean = mean(ENTRY), 
                              UWL = Mean+2*sd(ENTRY),
                              UCL = Mean+3*sd(ENTRY),
                              LWL = Mean-2*sd(ENTRY),
                              LCL = Mean-3*sd(ENTRY),
                                SD = sd(ENTRY))
data2

n <- unique(data$SAMPLING_POINT)

data3 <- data %>% 
        group_by(ANALYSIS)%>%
        arrange(LOGIN_DATE)%>%
        mutate(count=row_number(),
               first_SRM = count==1)%>%
        filter(first_SRM==TRUE)
       
data3

data4 <- data %>% 
        group_by(ANALYSIS)%>%
        arrange(desc(LOGIN_DATE))%>%
        mutate(count=row_number(),
               last_SRM = count==1)%>%
        filter(last_SRM==TRUE)

data4

plot <- ggplot(data, aes(x=LOGIN_DATE, y=ENTRY, fill = ANALYSIS)) +
        geom_point(size=4, shape=21, col = "black") +
        geom_segment(aes(x = data3$LOGIN_DATE[1],xend = data4$LOGIN_DATE[1], y = data2$Mean[1], yend = data2$Mean[1]), colour = "red", lty=2, lwd=0.5) +
        geom_segment(aes(x = data3$LOGIN_DATE[2],xend = data4$LOGIN_DATE[2], y = data2$Mean[2], yend = data2$Mean[2]), colour = "red", lty=2, lwd=0.5) +
        geom_segment(aes(x = data3$LOGIN_DATE[1],xend = data4$LOGIN_DATE[1], y = data2$UCL[1], yend = data2$UCL[1]), colour = "grey50", lty=5, lwd=0.5) +
        geom_segment(aes(x = data3$LOGIN_DATE[2],xend = data4$LOGIN_DATE[2], y = data2$UCL[2], yend = data2$UCL[2]), colour = "grey50", lty=5, lwd=0.5) +
        geom_segment(aes(x = data3$LOGIN_DATE[1],xend = data4$LOGIN_DATE[1], y = data2$LCL[1], yend = data2$LCL[1]), colour = "grey50", lty=5, lwd=0.5) +
        geom_segment(aes(x = data3$LOGIN_DATE[2],xend = data4$LOGIN_DATE[2], y = data2$LCL[2], yend = data2$LCL[2]), colour = "grey50", lty=5, lwd=0.5) +
        scale_color_brewer(palette="Set1") +
        labs(title = "IRM001A, tested by ICP-MS and GB", y="ug/100g", x="", subtitle = "Grey lines = UCL & LCL")+
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              text = element_text(size = 14))

        
plot


#ggsave("CC.png", last_plot(), width=8, height=4, dpi=50)

### Alternatively - a boxplot --------------------------------------
boxplot(data$ENTRY~data$ANALYSIS,
        main = "IRM001A, tested by ICP-MS, IODI01 and GB\n",
        ylab = "ug/100g",
        col="cornflowerblue")
