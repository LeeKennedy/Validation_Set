#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(dts.quality)

#### Functions -----------------------------


#### Data Input -----------------------------

data.in <- read_csv("~/Desktop/Fun with FOS/FOS_MU/OLIG04.csv")

#### Data Cleaning -----------------------------

data.in2 <- data.in %>% 
        filter(REPLICATE_COUNT == 1) %>% 
        filter(PRODUCT != "QC")

#### Visualising Data -----------------------------

plot_data <- ggplot(data.in2, aes(x=ENTRY)) +
        geom_histogram(fill = "cornflowerblue", col = "grey20", binwidth = 0.1) +
        labs(title = "Histogram of 2017 FOS Samples\n", x="g/100g") +
        theme_bw() +
        theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
              axis.line = element_line(size = 0.7, color = "black"), 
              text = element_text(size = 14))
plot_data

ggsave("FOS_Histogram.png", width = 8, height = 4, units = "in", dpi = 100)
