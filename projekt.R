# Zaciąganie bibliotek
library("forecast")
library("timeSeries")
library("ggplot2")

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
        main = 'Szereg czasowy dla odrzucanych pożyczek przez Lending Club',
        xlab = 'Rok',
        ylab = 'Suma pożyczek (mln dolarów)')
axis(1, at=2007:2012, las=2)
dev.off()

png('train_month_plot.png')
monthplot(data.train, 
          main = 'Wykres wahań sezonowych', 
          xlab = 'Miesiąc',
          ylab = 'Suma pożyczek (mln dolarów)')

png('train_day_plot.png')
