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
png('images/data_train_roczny_ogolny.png',
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


png('images/data_monthly_train_miesieczny_ogolny.png',
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


png('images/data_monthly_train_sezonowy_ogolny.png',
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


png('images/data_monthly_train_pudelkowy.png',
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


png('images/data_monthly_train_opoznienia.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

lag.plot(data_monthly.train/1000000, 
         lags = 12, 
         main = "Wykres rozrzutu dla wartości opóźnionych")
dev.off()


png('images/data_monthly_train_wykres_autokorelacji.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)
Acf(data_monthly.train, 
    main="Wykres autokorelacji ACF wartości dziennych")
dev.off()


png('images/data_monthly_train_wykres_autokorelacji_czastkowej.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

Pacf(data_monthly.train, main='Wykres autokorelacji cząstkowej PACF wartości dziennych')
dev.off()

# Transformacja BoxaCoxa
lambda.data_monthly.ts = BoxCox.lambda(data_monthly.train);lambda.data.ts
data_monthly.train.BoxCox = BoxCox(data_monthly.train, lambda = lambda.data.ts)


png('images/data_monthly_transformacja_boxacoxa.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

par(mfrow=c(2,1))
plot(data_monthly.train/1000000, 
     main="Dane miesięczne oryginalne",
     ylab = "Kwota pożyczek (mln dolarów)",
     xlab = "Miesiąc")
plot(data_monthly.train.BoxCox, 
     main="Dane miesięczne z transformacją BoxaCoxa przy lambda = 0.3692711",
     yaxt = 'n',
     ylab = '',
     xlab = "Miesiąc")
dev.off()


# Różnicowanie 

data_monthly.diff = diff(data_monthly.train, differences=1)

png('images/data_monthly_train_dane_oryginalne.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

tsdisplay(data_monthly.train, main = "Dane oryginalne")
dev.off()

png('images/data_monthly_train_roznicowanie_z_opoznieniem_jeden.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

tsdisplay(data_monthly.train.diff, main = "Dane po zróżnicowaniu z opóźnieniem 1")
dev.off()

# Dekompozycja
data_monthly.train.decom = decompose(data_monthly.train, type="additive")

png('images/train_monthly_train_dekompozycja.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

plot(data_monthly.train.decom)
dev.off()


png('images/train_monthly_train_fluktuacje.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

tsdisplay(data_monthly.train.decom$random, main="Losowe fluktuacje")
dev.off()

# Testowanie hipotezy o rozkładzie normalnym dla reszt
shapiro.test(data_monthly.train.diff) # Wykluczenie białego szumu. Odrzucamy h0


