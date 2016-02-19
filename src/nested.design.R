library (readxl)

# Data should be in labelled columns with no row labels.
Input <- read_excel("data/Validation_Workbook.xlsx", sheet = "ANOVA1")

#Always look at the data
boxplot(Input, frame = FALSE)

setEPS()
postscript("boxplot.eps")
boxplot(Input, 
        frame = TRUE,
        main = "Matrix Batch Results",
        ylab = "Units")
dev.off()

#convert to a two element stack
xs <- stack(Input)

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

# CI for sdr ------------------------------------------
l <- length(Input)

n <- 0
for (i in 1:l){
    x = length(combn(na.omit(Input[,i]),2))/2
    n = n + x
}
n

chir_low <- qchisq(.975, df=(n-1))
chir_high <- qchisq(.025, df=(n-1))

sdr_low <- sqrt(((n-1)*sdr^2)/chir_low)
sdr_high <- sqrt(((n-1)*sdr^2)/chir_high)

# CI for sdR ------------------------------------------

items <- rep(0,l)
for (j in 1:l) {
        items[j] <- length(na.omit(Input[,j]))
}

m <- 0
p <- l-1

for (i in 1:p) {
        q <- i+1
        for (j in q:l) {
        x = items[i] * items[j]
        m = m + x
}
}

chiR_low <- qchisq(.975, df=(m-1))
chiR_high <- qchisq(.025, df=(m-1))

sdR_low <- sqrt(((m-1)*sdr^2)/chiR_low)
sdR_high <- sqrt(((m-1)*sdr^2)/chiR_high)


