library(ggplot2)
library(scales)
library(reshape2)
library(plyr)
library(RColorBrewer)

#read csv
data = read.table("Russian_Poetry_genre_years.csv", sep=";", header=T, dec=",")

# subsetting, Verse < 100, no poems
length.small = subset(data, !grepl(paste("поэма", collapse = "|"), data$Genre, ignore.case=T))
length.small = subset(length.small, !grepl(paste("цикл", collapse = "|"), length.small$Genre, ignore.case=T))
length.small = subset(length.small, Verses < 101 & Verses > 4 & Year > 1719 & Year < 1990)

# cutting
lab = as.character(seq(1720, 1980, by = 10))
br = seq(1720, 1990, by = 10)
len.small.period = transform(length.small, period = cut(Year, breaks = br, labels = lab))

# aggregation
len.small.1 = ddply(len.small.period, "period", summarise, verse.mean = mean(Verses))
len.small.2 = ddply(len.small.period, "period", summarise, verse.median = median(Verses))
len.small.1$verse.median <- len.small.2$verse.median
len.small.fin = subset(len.small.1, period != "NA")
len.small.fin$period = as.numeric(as.vector(len.small.fin$period))

melt.len = melt(len.small.fin, id = "period")
melt.len = subset(melt.len, period > 1740 & period < 1920)

#plot.together

ggplot(melt.len, aes(period, value, group = variable, color = variable)) + geom_point(size = 3) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()) +
  theme(axis.line.x = element_line(color="black"), axis.line.y = element_line(color="black")) +
  labs(x = "Year", y = "Lines", title = "", color = "") +
  theme(axis.text=element_text(size=16), axis.title=element_text(size = 18), legend.text = element_text(size = 16)) +
  scale_x_continuous(breaks=pretty_breaks(n = 10)) +
  scale_y_continuous(breaks=pretty_breaks(n = 8)) +
  scale_color_brewer(breaks = c("verse.mean", "verse.median"), labels=c("Mean", "Median"), palette="Paired")
