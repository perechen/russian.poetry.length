library(ggplot2)
library(scales)
library(reshape2)
library(plyr)
library(RColorBrewer)

#read csv
data = read.table("Russian_Poetry_genre_years.csv", sep=";", header=T, dec=",")

#subset and aggregating
df1 = subset(data, Year < 1921 & Year > 1799)
cor.df.mean = ddply(df1, "Year", summarise, verse.mean = mean(Verses))
cor.df.median = ddply(df1, "Year", summarise, verse.median = median(Verses))
cor.df.mean$verse.median = cor.df.median$verse.median
melt.cor.df = melt(cor.df.mean, id = "Year")

#plot
ggplot(melt.cor.df, aes(Year, value, group = variable, color = variable)) + geom_point(size = 2) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()) +
  theme(axis.line.x = element_line(color="black"), axis.line.y = element_line(color="black")) +
  labs(x = "Year", y = "Lines", title = "", color = "") +
  theme(axis.text=element_text(size=16), axis.title=element_text(size = 18), legend.text = element_text(size = 16)) +
  scale_x_continuous(breaks=pretty_breaks(n = 10)) +
  scale_y_continuous(breaks=pretty_breaks(n = 6)) +
  scale_color_brewer(palette="Paired") +
  theme(legend.position="none")



