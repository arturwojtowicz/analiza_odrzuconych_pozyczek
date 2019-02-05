# Zaciąganie bibliotek
library("ggplot2") #Visualization
library("timeSeries") #Statistical Tests for Time Series data
library("forecast") #Time Series Forecasting

# Ustawianie lokalnej pozycji do m.in zapisu plików oraz ich wczytywania
setwd("/home/dziabaku/Studia/pakiety_statystyczne/Projekt/")

# Wczytywanie danych
data = read.csv("rejected_stats.csv", sep=",") # Dane dzienne
data_monthly = read.csv("rejected_monthly_stats.csv", sep=",") # Dane miesięczne

# Utworzenie timeSeries
data.ts = ts(data$Amount.requested, start = c(2007, 146), frequency=365.25) # Ponieważ 26 maja jest 146 dniem roku 2007
data_monthly.ts = ts(data_monthly$Amount.requested, start = c(2007, 5), frequency = 12)

# Utworzenie okna uczącego się oraz testowego
data.train = window(data.ts, end = c(2011, 365))
data_monthly.train = window(data_monthly.ts, end = c(2011, 12))
data.test = window(data.ts, start = c(2011, 366)) # Ponieważ w 2012 roku jest 366 dni
data_monthly.test = window(data_monthly.ts, start = c(2012, 1))

# Utworzenie szeregu czasowego dla kwoty odrzucanych dziennie pożyczek przez Lending Club dla zbioru uczącego
png('images/train_year_plot.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

plot.ts(data.train/1000000,
        xaxt = 'n',
        main = 'Szereg czasowy dla kwoty odrzuconych pożyczek przez Lending Club',
        xlab = 'Rok',
        ylab = 'Kwota odrzuconych pożyczek (mln dolarów)')
axis(1, at=2007:2012, las=2)
dev.off()


png('images/train_month_plot.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

monthplot(data_monthly.train/1000000, 
          ylab = "Kwota odrzuconych pożyczek (mln dolarów)",
          main = "Wykres wahań sezonowych",
          xlab = "Miesiąc")
dev.off()


png('images/train_season_plot.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

seasonplot(data_monthly.train/1000000,
           main = "Wykres wahań sezonowych",
           ylab = "Kwota odrzuconych pożyczek (mln dolarów)",
           xlab = "Miesiąc")
dev.off()


png('images/train_box_plot.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

boxplot(data_monthly.train/1000000 ~ cycle(data_monthly.train),
        main = "Wykres pudełkowy dla każdego miesiąca",
        ylab = "Kwota odrzuconych pożyczek (mln dolarów)",
        xlab = "Miesiąc")
dev.off()


png('images/train_lag_plot.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

lag.plot(data_monthly.train/1000000, 
         lags = 12, 
         main = "Wykres rozrzutu dla wartości opóźnionych")
dev.off()


png('images/train_acf_plot.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)
Acf(data.train, 
    main="Wykres autokorelacji ACF wartości dziennych")
dev.off()


png('images/train_pacf_plot.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

Pacf(data.train, main='Wykres autokorelacji cząstkowej PACF wartości dziennych')
dev.off()

# Transformacja BoxaCoxa
lambda.data.ts = BoxCox.lambda(data.train);lambda.data.ts
data.train.BoxCox = BoxCox(data.train, lambda = lambda.data.ts)


png('images/train_boxcox_plot.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

par(mfrow=c(2,1))
plot(data.train/1000000, 
     main="Dane oryginalne",
     ylab = "Kwota pożyczek (mln dolarów)",
     xlab = "Miesiąc")
plot(data.train.BoxCox, 
     main="Dane z transformacją BoxaCoxa przy lambda = 0.3692711",
     yaxt = 'n',
     ylab = '',
     xlab = "Miesiąc")
dev.off()


# Różnicowanie 

data.train.diff = diff(data.train, differences=1)

png('images/train_original_diff_plot.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

tsdisplay(data.train, main = "Dane oryginalne")
dev.off()


png('images/train_diff_diff_plot.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

tsdisplay(data.train.diff, main = "Dane po zróżnicowaniu z opóźnieniem 1")
dev.off()


# Dekompozycja
data.train.decom = decompose(data.train, type="additive")

png('images/train_diff_plot.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

plot(data.train.decom)
dev.off()


png('images/train_fluct_plot.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

tsdisplay(data.train.decom$random, main="Losowe fluktuacje")
dev.off()

# Testowanie hipotezy o rozkładzie normalnym dla reszt
shapiro.test(data.train.diff) # Wykluczenie białego szumu. Odrzucamy h0


