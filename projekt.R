# Zaciąganie bibliotek
library("readr") #Load our data
library("dplyr") # Data Wrangling
library("ggplot2") #Visualization
library("ggfortify") #Visualization
library("timeSeries") #Statistical Tests for Time Series data
library("forecast") #Time Series Forecasting
library(DT) # View our Raw Data

install.packages('dplyr')
install.packages('ggfortify')
install.packages('DT')
# Ustawianie lokalnej pozycji do m.in zapisu plików oraz ich wczytywania
setwd("/home/dziabaku/Studia/pakiety_statystyczne/Projekt/")

# Wczytywanie danych
data = read.csv("rejected_stats.csv", sep=",")

# Utworzenie timeSeries
data.ts = ts(data$Amount.requested, start = c(2007, 146), frequency=365.25) # Ponieważ 26 maja jest 146 dniem roku 2007

# Utworzenie okna uczącego się oraz testowego
data.train = window(data.ts, end = c(2011, 365))
data.test = window(data.ts, start = c(2011, 366)) # Ponieważ w 2012 roku jest 366 dni

# Utworzenie szeregu czasowego dla kwoty odrzucanych dziennie pożyczek przez Lending Club dla zbioru uczącego
png('train_year_plot.png')
plot.ts(data.train/1000000,
        xaxt = 'n',
        main = 'Szereg czasowy dla kwoty odrzuconych pożyczek przez Lending Club',
        xlab = 'Rok',
        ylab = 'Kwota odrzuconych pożyczek (mln dolarów)')
axis(1, at=2007:2012, las=2)
dev.off()

plots = stl(data.train,s.window="periodic")

png('train_season_plot.png')
ggseasonplot(data.train, year.labels=TRUE, year.labels.left=FALSE) +
             ylab("Kwota odrzuconych pożyczek (mln dolarów)") +
             ggtitle("Wykres wahań sezonowych")
dev.off()
