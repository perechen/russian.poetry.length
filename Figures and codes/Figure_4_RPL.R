library(ggplot2)
library(scales)

#read csv
data = read.table("Russian_Poetry_genre_years.csv", sep=";", header=T, dec=",")

#plot
ggplot(subset(data, Year > 1750 & Year < 1922 & Verses < 50), aes(Verses)) + geom_bar() +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()) +
  theme(axis.line.x = element_line(color="black"), axis.line.y = element_line(color="black")) +
  labs(x = "Lines", y = "Count", title = "", color = "") +
  theme(axis.text=element_text(size=16), axis.title=element_text(size = 18), legend.text = element_text(size = 16)) +
  scale_x_continuous(breaks=pretty_breaks(n = 10)) +
  scale_y_continuous(breaks=pretty_breaks(n = 6))


