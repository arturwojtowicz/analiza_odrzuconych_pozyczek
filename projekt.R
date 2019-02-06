# Zaciąganie bibliotek
library("ggplot2") #Visualization
library("timeSeries") #Statistical Tests for Time Series data
library("forecast") #Time Series Forecasting

# Ustawianie lokalnej pozycji do m.in zapisu plików oraz ich wczytywania
setwd("/home/dziabaku/Studia/pakiety_statystyczne/Projekt/datasets")

# Wczytywanie danych
#data = read.csv("rejected_stats.csv", sep=",") # Dane dzienne
data_monthly = read.csv("rejected_monthly_stats.csv", sep=",") # Dane miesięczne

# Utworzenie timeSeries
#data.ts = ts(data$Amount.requested, start = c(2007, 146), frequency=365.25) # Ponieważ 26 maja jest 146 dniem roku 2007
data_monthly.ts = ts(data_monthly$Amount.requested, start = c(2007, 5), frequency = 12)

png('../images/data_ogolnie_ts.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

par(mfrow=c(2,1))
plot(data_monthly.ts/1000000, 
     main = "Miesięczna kwota odrzuconych pożyczek przez Lending Club",
     xlab = 'Rok',
     ylab = 'mln dolarów')
dev.off()

# Utworzenie okna uczącego się oraz testowego
data_monthly.train = window(data_monthly.ts, end = c(2011, 12))
data_monthly.test = window(data_monthly.ts, start = c(2012, 1))

# Utworzenie szeregu czasowego dla kwoty odrzucanych dziennie pożyczek przez Lending Club dla zbioru uczącego
png('../images/data_train_roczny_ogolny.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

plot.ts(data_monthly.train/1000000,
        xaxt = 'n',
        main = 'Szereg czasowy dla kwoty odrzuconych pożyczek przez Lending Club',
        xlab = 'Rok',
        ylab = 'Kwota odrzuconych pożyczek (mln dolarów)')
axis(1, at=2007:2012, las=2)
dev.off()


png('../images/data_monthly_train_miesieczny_ogolny.png',
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


png('../images/data_monthly_train_sezonowy_ogolny.png',
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


png('../images/data_monthly_train_pudelkowy.png',
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


png('../images/data_monthly_train_opoznienia.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

lag.plot(data_monthly.train/1000000, 
         lags = 12, 
         main = "Wykres rozrzutu dla wartości opóźnionych",
         do.lines = FALSE)
dev.off()

png('../images/data_monthly_train_wykres_autokorelacji_oraz_czastkowej.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

par(mfrow=c(2,1))
Acf(data_monthly.train, 
    main="Wykres autokorelacji ACF wartości miesięcznych")
Pacf(data_monthly.train, 
     main='Wykres autokorelacji cząstkowej PACF wartości miesięcznych')
dev.off()


# Różnicowanie 

data_monthly.train.diff = diff(data_monthly.train, differences=1)

png('../images/data_monthly_train_dane_oryginalne.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

tsdisplay(data_monthly.train, main = "Dane oryginalne")
dev.off()

png('../images/data_monthly_train_roznicowanie_z_opoznieniem_jeden.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

tsdisplay(data_monthly.train.diff, main = "Dane po zróżnicowaniu z opóźnieniem 1")
dev.off()

# AR(2) - z PACF
# MA(1) - z ACF zróżnicowania
data_monthly.AR2 = Arima(data_monthly.train, order = c(2,1,0), seasonal = c(0,0,0))
data_monthly.MA1 = Arima(data_monthly.train, order = c(0,1,1), seasonal = c(0,0,0))

summary(data_monthly.AR2)
# AIC=2055.9   AICc=2056.38   BIC=2061.93
summary(data_monthly.MA1)
# AIC=2056.01   AICc=2056.24   BIC=2060.02

png('../images/train_monthly_train_AR2_reszty.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

tsdisplay(data_monthly.AR2$residuals, main = "Wykres reszt modelu AR2")
dev.off()

png('../images/train_monthly_train_MA1_reszty.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

tsdisplay(data_monthly.MA1$residuals, main = "Wykres reszt modelu MA1")
dev.off()

#Test losowości reszt
Box.test(data_monthly.AR2$residuals)
Box.test(data_monthly.MA1$residuals)

# Automatyczny wybór optymalnego różnicowania
ndiffs(data_monthly.train)
nsdiffs(data_monthly.train)
auto.arima(data_monthly.train, allowdrift=FALSE, trace=TRUE)
# MA(1)
# AIC=2056.01   AICc=2056.24   BIC=2060.02


# Prognozy 

data_monthly.AR2.prognoses <- forecast(data_monthly.AR2, h=length(data_monthly.test))
data_monthly.MA1.prognoses <- forecast(data_monthly.MA1, h=length(data_monthly.test))

png('../images/prognozy.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

par(mfrow=c(3,1))
plot(data_monthly.ts, main = "Główny szereg czasowy")
plot(data_monthly.AR2.prognoses, main = "Prognoza AR2")
plot(data_monthly.MA1.prognoses, main = "Prognoza MA1")
dev.off()


png('../images/mieszane.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

ts.plot(
  data_monthly.test/1000000,
  data_monthly.AR2.prognoses$mean/1000000,
  data_monthly.MA1.prognoses$mean/1000000,
  main = "Porównanie prognoz z rzeczywistym stanem",
  col = c("red", "green", "black"),
  ylab = "mln dolarów",
  xlab = "Okres")
grid()
legend("topleft", legend = c("Zbiór testowy", "AR2", "MA1"), col = c("red", "green", "black"), lty=1:2, cex=0.7)
dev.off()


### BŁĘDY PREDYKCJI
criteries <- c("MAE", "RMSE", "MAPE", "MASE")
round(accuracy(data_monthly.AR2.prognoses)[,criteries],2)
round(accuracy(data_monthly.MA1.prognoses)[,criteries],2)


# METODA HOLTA
data_monthly.holt <- holt(data_monthly.train, h=length(data_monthly.test))

png('../images/holt.png',
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

ts.plot(data_monthly.test/1000000, data_monthly.holt$mean/1000000, main = "Prognozowanie metodą Holta dla danych miesięcznych",
        col = c('black', 'red'), lty=1:2, xlab="Okres", ylab = "mln dolarów")
grid()
legend("bottomright", legend=c("Holt", "Zbiór testowy"), col = c("red", "black"), lty=1:2)
dev.off()


round(accuracy(data_monthly.holt)[,criteries],2)


#data_monthly.train.hw <- hw(data_monthly.train, h=12)
#png('../images/winters.png')
#ts.plot(data_monthly.test/1000000, data_monthly.train.hw$mean/1000000, 
#        main = "Prognozowanie metodą Holta-Wintersa dla danych miesięcznych",
#        col = c('black', 'red'), lty=1:2, xlab="Okres", ylab = "mln dolarów")
#dev.off()
#
#round(accuracy(data_monthly.train.hw)[,criteries],2)
#autoplot(cdpr.hw) + 
#  ggtitle("Holt - Winters method") +
#  labs(x = "Time", y = "Price")