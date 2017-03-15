library(ggplot2)
library(plyr)

#data and subset
data = read.table("Russian_Poetry_genre_years.csv", sep=";", header=T, dec=",")

length.small = subset(data, !grepl(paste("поэма", collapse = "|"), data$Genre, ignore.case=T))
length.small = subset(length.small, !grepl(paste("цикл", collapse = "|"), length.small$Genre, ignore.case=T))
length.small = subset(length.small, Verses < 101 & Verses > 4 & Year > 1799 & Year < 1921)

#linear regression for raw data (log scale)
raw.lm = lm(log(Verses) ~ Year, data = length.small)
summary(raw.lm)

#summarise and melt
len.small.mean = ddply(length.small, "Year", summarise, verse.mean = mean(Verses))
len.small.median = ddply(length.small, "Year", summarise, verse.median = median(Verses))
len.small.mean$verse.median = len.small.median$verse.median

#linear regression for means and medians
mean.lm = lm(verse.mean ~ Year, data = len.small.mean)
summary(mean.lm)

median.lm = lm(verse.median ~ Year, data = len.small.mean)
summary(median.lm)

