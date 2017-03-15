library(ggplot2)
library(scales)
library(reshape2)
library(plyr)

#read csv
data = read.table("Russian_Poetry_genre_years.csv", sep=";", header=T, dec=",")
data.clear = subset(genre.df, Year > 1720 & Year < 1990)

# cutting
lab = as.character(seq(1720, 1980, by = 10))
br = seq(1720, 1990, by = 10)
data.period = transform(data.clear, period = cut(Year, breaks = br, labels = lab))

#counting
count.per = count(data.period, "period")
count.per$period = as.numeric(as.vector(count.per$period))
count.per = subset(count.per, period > 1760 & period < 1920)

#cycles and counting
cyles = subset(data.period, grepl(paste("цикл", collapse = "|"), data.period$Genre, ignore.case=T))
count.cycles = count(cyles, "period")    
count.cycles$period = as.numeric(as.vector(count.cycles$period))
count.cycles = subset(count.cycles, period > 1760 & period < 1920)
count.cycles$perc = count.cycles$freq/count.df.per$freq

#plot
ggplot(count.cycles, aes(period, perc*100)) + geom_line(size = 2) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()) +
  theme(axis.line.x = element_line(color="black"), axis.line.y = element_line(color="black")) +
  labs(x = "Decades", y = "%", title = "", color = "") +
  theme(axis.text=element_text(size=16), axis.title=element_text(size = 18), legend.text = element_text(size = 16)) +
  scale_y_continuous(breaks=pretty_breaks(n = 10)) +
  scale_x_continuous(breaks=pretty_breaks(n = 10))
