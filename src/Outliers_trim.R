
xx <- read.delim('clipboard', as.is=TRUE, header = FALSE)
#x <- read.csv("vitd01.csv", header = TRUE)

library(dts.quality)

new <- outliers(xx, b=TRUE)
summary(new)


