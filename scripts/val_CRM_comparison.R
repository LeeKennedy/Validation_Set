
# Data Input -------------------------------------------------------------

crmdata <- read_excel(location, sheet = "CRM_Comparison")


### Data extraction
crm_mean <- as.numeric(crmdata[2,5])
crm_sd <- as.numeric(crmdata[3,5])


crm_plot <- ggplot(crmdata, aes(x=Run, y = Result)) +
        geom_point(size=4, shape = 21, fill="cornflowerblue", col="black") +
        geom_hline(yintercept = crm_mean+2*crm_sd, lty=2, col="darkgreen") +
        geom_hline(yintercept = crm_mean-2*crm_sd, lty=2, col="darkgreen") +
        labs(title="CRM 1849a Analysis", subtitle="Dotted green lines indicate 95% CI for the CRM", y = "ug/kg") +
        scale_y_continuous(lim=c(900,1500)) +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), axis.line = element_line(size = 0.7, color = "black"), text = element_text(size = 14))

crm_plot