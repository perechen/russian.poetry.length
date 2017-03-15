library(ggplot2)
library(scales)
library(reshape2)
library(plyr)
library(RColorBrewer)

#data
data = read.table("Russian_Poetry_genre_years.csv", sep=";", header=T, dec=",")

#cutting
lab = as.character(seq(1720, 1980, by = 10))
br = seq(1720, 1990, by = 10)
base.df  = subset(data, Year < 1990)
base.period = transform(base.df, period = cut(Year, breaks = br, labels = lab))

# aggregation
base.1 = ddply(base.period, "period", summarise, verse.mean = mean(Verses))
base.2 = ddply(base.period, "period", summarise, verse.median = median(Verses))
base.1$verse.median <- base.2$verse.median
base.fin = subset(base.1, period != "NA")
base.fin$period = as.numeric(as.vector(base.fin$period))

melt.base = subset(base.fin, period > 1740 & period < 1920)
melt.base = melt(melt.base, id = "period")

#plot.together
ggplot(melt.base, aes(period, value, group = variable, color = variable)) + geom_point(size = 3) +
  theme_bw() +
  geom_smooth(se = FALSE) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()) +
  theme(axis.line.x = element_line(color="black"), axis.line.y = element_line(color="black")) +
  labs(x = "Decades", y = "Lines", title = "", color = "") +
  theme(axis.text=element_text(size=16), axis.title=element_text(size = 18), legend.text = element_text(size = 16)) +
  scale_x_continuous(breaks=pretty_breaks(n = 10)) +
  scale_y_continuous(breaks=pretty_breaks(n = 6)) +
  scale_color_brewer(palette="Paired") +
  theme(legend.position="none")
