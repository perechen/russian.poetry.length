library(ggplot2)
library(scales)
library(reshape2)
library(plyr)


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

# subset and count percent
len.14 = subset(len.small.period, Verses > 7 & Verses < 21 & Verses != 14)
count.all = count(len.small.period, "period")
count.all$period = as.numeric(as.vector(count.all$period))
count.all = subset(count.all, period != "NA" & period < 1920)

count.14 = count(len.14, "period")
count.14$period = as.numeric(as.vector(count.14$period))
count.14= subset(count.14, period < 1920)
count.14$all = count.all$freq
count.14$perc = count.14$freq/count.14$all
count.14 = subset(count.14, period > 1740)

#plot
ggplot(count.14, aes(period, perc*100)) + geom_line(size = 2) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()) +
  theme(axis.line.x = element_line(color="black"), axis.line.y = element_line(color="black")) +
  labs(x = "Decades", y = "%", title = "", color = "") +
  theme(axis.text=element_text(size=16), axis.title=element_text(size = 18), legend.text = element_text(size = 16)) +
  scale_y_continuous(breaks=pretty_breaks(n = 10)) +
  scale_x_continuous(breaks=pretty_breaks(n = 10))

