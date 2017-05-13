
# Data Input -------------------------------------------------------------

srmdata <- read_excel(location, sheet = "SRM_Data")




# Data Cleaning ----------------------------------------------------------

srm <- strsplit(srmdata$TEXT_ID, split="-")

srm <- sapply(srm,function(x) x[2])
srm <- as.data.frame(srm)
srmdata <- cbind(srmdata, srm)
srm2 <- srmdata[,c(1,19,8,9)]


# Boxplot ----------------------------------------------------------------

boxplot(srm2$ENTRY~srm2$srm)

# Desired SRM ------------------------------------------------------------

srm.name <- "IRM001B"


srm.raw <- srm2 %>% 
        filter(srm == srm.name)
colnames(srm.raw)[4] <- "All_Data"

srm.active <- srm2 %>% 
        filter(srm == srm.name) %>% 
        mutate(ENTRY = remove_outliers(ENTRY))
colnames(srm.active)[4] <- "Clean"

srm.active <- cbind(srm.active, srm.raw$All_Data)
srm.active$n <- as.numeric(row.names(srm.active))

srm.active <- srm.active[,c(6,1,2,3,4,5)]
colnames(srm.active)[6] <- "All_Data"

# Ranges -----------------------------------------------------------------

srm.temp <- na.omit(srm.active$Clean)
Mean <- mean(srm.temp)
SD <- sd(srm.temp)
UCL <- Mean + 3*SD
UWL <- Mean + 2*SD
LWL <- Mean - 2*SD
LCL <- Mean - 3*SD


# Visualising Data -------------------------------------------------------

srm.plot <- ggplot(srm.active, aes(x=n, y=Clean)) +
        geom_line(lwd=0.1, col="black") +
        geom_point(size=4, shape=21, fill="cornflowerblue", col = "black") +
        geom_hline(yintercept = Mean, lty=2, col="red") +
        geom_hline(yintercept = UCL, lty=2, col="darkgreen") +
        geom_hline(yintercept = LCL, lty=2, col="darkgreen") +
        geom_hline(yintercept = UWL, lty=2, col="black") +
        geom_hline(yintercept = LWL, lty=2, col="black") +
        labs(title = paste(srm.name, " Control Chart\n", sep=""), y = "mg/kg", x="") +
        scale_y_continuous(limits = c(0.01, 0.04)) +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              text = element_text(size = 14))
srm.plot

ggsave(paste(srm.name, "_control_chart.png", sep=""), width = 12, height = 5, dpi = 100)

