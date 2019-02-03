library("forecast")
library("timeSeries")
library("ggplot2")

data = read.csv("/home/dziabaku/Studia/pakiety_statystyczne/Projekt/RejectStatsA.csv", sep=",")
data.ts = ts(data$Violent.crime, start = 1997, frequency = 1)
data.train = window(data.ts, end = 2010)
data.train = window(data.ts, start = 2011)
png('main.png')
plot.ts(data.train, xaxt='n',
        main = "Szereg czasowy dla przestępstw w USA",
        xlab="Rok", ylab="Liczba przestępstw")
axis(1, at=1997:2010)
dev.off()
