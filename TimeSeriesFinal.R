
#Szkolenie 24.04.2018 -  Time sereis 
#LAadowanie potrzebnych bibliotek 
library(openxlsx)
library(dplyr)
library(tseries)
library(lubridate)
library(Rcpp)
####Pakiet przydatny do konstukcji modeli ARIMA oraz do prognozowania i diagnostyki prognoz 
library(forecast)
### Pakiet do analizy kointegracji i diagnostyki szeregÃ³w czasowych 
library(urca)
###Pakiet do modeli VAR
library(vars)
#####
library(ggplot2)
library(timeSeries)
library(PerformanceAnalytics)

#Pakiet do bezpoÅ›redniego Å›ciÄ…gania danych z Eurostatu  - Dane zostaly sciagniete za posrednictwem tego pakietyu 
#library(eurostat)

###Kod do bezposredniego sciagniecia danych z Eurostatu:  
#Nie bedzie wykonywany na zajeciach (chyba, ze na sam koniec by pokazac jak to fukcjonuje)
###Sharmonizowany indeks cen konsumenta (HICP)
#Wykaz wszystkich baz danych Eurostatu w które maja w swojej nazwie HCIP 
#
#HICP = search_eurostat(pattern="HICP")
#
#Sciagniecie calej bazy danych dla HICP
#
#HICP_all <- get_eurostat(as.character("prc_hicp_midx"))
#
#
#####Selekcja jedynie indeksów dla wszystkich dóbr (coicop CP00) dla 2005=100 (unit "I05")
#Generalnie po kodach COCIOP mozna wybrac indeksy dla bardziej szczególowych kategorii dóbr. 
#
#HICP_total <- HICP_all %>% dplyr::filter(coicop =="CP00"& unit == "I05")
#
### Wybór indeksu HICP dla wszystkich dórb jedynie dla Polski
#
#HICP_PL <- HICP_total%>% filter(geo =="PL")
#
#Wybór indeksu HICP dla Polski, Niemiec i Francji 
#HICP_PL_WE <- HICP_total%>% dplyr::filter(geo =="PL" | geo == "DE"|geo == "FR")
#
#
#write.csv(HICP_PL_WE, "HICP_PL_WE.csv")






###Sciezke nalezy ustawic wg wlasnych preferncji
setwd("\\\\UBSPROD.MSAD.UBS.NET/userdata/burdaa/rf/Desktop/AGH_course/TimeSeries_Workshop/New")

#setwd("C:/Users/AdrianoB/Desktop/Praca/KursAGH")


###Sciagniacie danych potrzebnych do analizy  - Wskaznik cen konsumenta HICP z Eurostatu dla Polski, Niemec i Francji


CPI_PL_WE <-read.csv2("HICP_PL_WE.csv",sep=",")

####### Ogladamy dane 
head(CPI_PL_WE)

####Ucinamy pierwsza kolumne
CPI_PL_WE<- CPI_PL_WE[,-1]

####Uporzadkowanie Dat - w taki sposób aby na poczatku byly najwczesniejsze obserwacje

### Wykorzystujemy tu pakiet lubridate - sluzacy do latwiejszej pracy z Datami w R
#Pierwszy punkt to przeksztlcenie zmiennej "time" 
#format "Date" wg schematu - year -month- day 
CPI_PL_WE$time <- lubridate::ymd(CPI_PL_WE$time)
#
#UWaga:  Tutaj wersja 0.12.13 pakietu Rcpp jest wymagana.  

#Drugi krok to sortowanie danych po czasie, a nastepnie po 
#zmiennej "geo" okreslajacych ksztalt 
CPI_PL_WE<-CPI_PL_WE %>% dplyr::arrange(time) %>% dplyr::arrange(geo)

#Przeksztalcenie values - ze zmiennej factor na numeryczne:
#Uwaga - tu trzeba  factor przeksztalcic na character i dopiero potem na numeric

CPI_PL_WE<- CPI_PL_WE %>% mutate(value = as.numeric(as.character(values))) %>%
  dplyr::select(-values)

#Utworzenie Zmiennych pomocnicznych okreslajacych poczatek i koniec szeregu czasowego 
MinMaxDate <- CPI_PL_WE %>% group_by(geo)%>% 
  summarize(Begin = min(time), Ends = max(time))

#Utworzenie zmiennych pomocniczych pomocnych przy tworzeniu szeregów czasowych
MinMaxDate2 <- MinMaxDate %>% group_by(geo)%>%
  mutate(YB = as.numeric(format(Begin, "%Y")),
         MB = as.numeric(format(Begin, "%m")),
         YE = as.numeric(format(Ends, "%Y")),
         ME = as.numeric(format(Ends, "%m"))
         )

##Zlaczenie zbiorów MinMAxDate2 i CPI_PL_WE  tak aby latwo utworzyc zmienne klasy time series  

CPI_PL_WE2 <- full_join(CPI_PL_WE, MinMaxDate2)

#Joining jest dokonywane przez  by = "geo"



####Wykres liniowy szeregów czasowych
ggplot(CPI_PL_WE2, aes(x=time,y=value, color=geo))+geom_line() +xlab("lata")+ylab("2005=100") +ggtitle("Sharmonizowany indeks cen konsumenta (HICP) \n dla Polski, Niemiec i Francji") 
#
#
#Zadanie nr 1 - > Przedstaw na wykresie dane od roku 2000,  przy przeksztlceniu indeksu jednopodstawowego, tak, ze, ze 01_2000 = 100  
#Tu jest miejsce na Twoje rozwiazanie !
#
#
#
#
#
#
#
#
#
#
#Tu jest jedno z proponowanych rozwiazan:    

CPI_PL_WE3 <-CPI_PL_WE2 %>%dplyr::filter(time >="2000-01-01")

Values2000 <- CPI_PL_WE %>% dplyr::filter(time=="2000-01-01") %>% dplyr::select(value, geo) %>% rename(value2000= value)

CPI_PL_WE4 <- full_join(CPI_PL_WE3, Values2000)%>% dplyr::select(-c(YB, MB, Begin)) %>% mutate(NewIndex = (value/value2000)*100)
  
ggplot(CPI_PL_WE4, aes(x=time,y=NewIndex, color=geo))+geom_line() +xlab("lata")+ylab("01.2000=100") +ggtitle("Sharmonizowany indeks cen konsumenta (HICP) \n dla Polski, Niemiec i Francji") 












#########Wyciaganie indeksu CPI dla Polski - tak aby  

CPI_PL <- CPI_PL_WE4 %>% dplyr::filter(geo == "PL")

MM_PL <- MinMaxDate2 %>% dplyr::filter(geo =="PL")%>% dplyr::select(-c(YB,MB))

#### Tworzenie wektorów szeregów czasowych, z wykorzystaniem funkcji TS 

CPI_P = ts(CPI_PL[,"NewIndex"],start=c(2000,1), end=c(as.numeric(MM_PL["YE"]), as.numeric(MM_PL["ME"])),frequency=12)

##Logarytm - czesto stosowane w badaniach empirycznych (stabilizacja wariancji w czasie - nie powinno sie stosowac do zmiennych których wartosci moga byc ujemne)
lnCPI_P = log(CPI_P)
## Pierwsze róznice logarytmów
dlnCPI_P = diff(lnCPI_P,lag=1)

#Krótkie zadanie - stwórz szereg przyrostów logarytmów r/r 
#
#
#
#
#
#
#
#
#
#Proponowane rozwiazanie: 
sdlnCPI_P = diff(lnCPI_P, lag=12)

###WProste wykres
plot(CPI_P)
plot(lnCPI_P)
plot(dlnCPI_P)
plot(sdlnCPI_P)

### Analiza wlasciwosci szeregów    
#Analiza stacjonarnosci test ADF,  wariant ze stala i metoda selekcji opóznienie - kryterium akaike
#domyslnie liczba lags ustalona przez uzytkownika  
ADF_CPI <- ur.df(CPI_P, type="drift",lags=18,selectlags="AIC")
ADF_lCPI <- ur.df(lnCPI_P, type="drift", lags =18,selectlags="AIC")
ADF_dlCPI <- ur.df(dlnCPI_P, type="drift",lags= 18,selectlags="AIC")
ADF_sdlCPI <- ur.df(sdlnCPI_P, type="drift",lags= 18,selectlags="AIC")



summary(ADF_CPI)
summary(ADF_lCPI)
summary(ADF_dlCPI)
summary(ADF_sdlCPI)


#Test KPSS

KPSS_CPI <- ur.kpss(CPI_P, type="mu",lags="short")
KPSS_lCPI <- ur.kpss(lnCPI_P, type="mu",lags="short")
KPSS_dlCPI <- ur.kpss(dlnCPI_P, type="mu",lags="short")
KPSS_sdlCPI <- ur.kpss(sdlnCPI_P, type="mu",lags="short")


summary(KPSS_CPI)
summary(KPSS_lCPI)
summary(KPSS_dlCPI)
summary(KPSS_sdlCPI)




#funkcja autokrelacji

acf(CPI_P)
pacf(CPI_P)

acf(lnCPI_P)
acf(dlnCPI_P)
acf(sdlnCPI_P)
pacf(dlnCPI_P)

pacf(sdlnCPI_P)

### Co mozemy powiedziec o szeregu ? 
#Jak podejsc do jego modelowania




#Model AR(1,0,0)
arima(lnCPI_P, order=c(1,0,0))

#Model AR(1,1,0)
arima(lnCPI_P, order=c(1,1,0),include.mean = TRUE)

#Model AR(1,0,0) dla przyrostów i sezonowych przyrostów
arima(dlnCPI_P, order=c(1,0,0))
arima(sdlnCPI_P, order=c(1,0,0))

#Model z sezonowymi komponentatmi (SARIMA(1,0,0), (1,0,0))
arima(dlnCPI_P, order=c(1,0,0),seasonal = list(order = c(1, 0, 0)))

Arima1_dCPI <- arima(dlnCPI_P, order=c(1,0,0))
 
plot(Arima1_dCPI$residuals)
pacf(Arima1_dCPI$residuals)




SARIMA1_dCPI <-arima(dlnCPI_P, order=c(1,0,0),seasonal = list(order = c(1, 0, 0)))
plot(SARIMA1_dCPI$residuals)
pacf(SARIMA1_dCPI$residuals)




ForecastAR1 <- forecast(Arima1_dCPI)
plot(ForecastAR1$mean)


ForecastSAR1 <- forecast(SARIMA1_dCPI)
plot(ForecastSAR1$mean)
#
#
#
#
#
#
###Automayczna selekcja modeli - wykorzystanie funkcji auto.arima z pakietu forecast 
#auto.arima(y, d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2,
#max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2,
#start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE,
#seasonal = TRUE, ic = c("aicc", "aic", "bic"), stepwise = TRUE,
#trace = FALSE, approximation = (length(x) > 150 | frequency(x) > 12),
#truncate = NULL, xreg = NULL, test = c("kpss", "adf", "pp"),
#seasonal.test = c("seas", "ocsb", "hegy", "ch"), allowdrift = TRUE,
#allowmean = TRUE, lambda = NULL, biasadj = FALSE, parallel = FALSE,
#num.cores = 2, x = y, ...)
#Arguments
#
#

AutoArima <- auto.arima(dlnCPI_P)
forecast(AutoArima)


ForecastAuto <- forecast(AutoArima)
plot(ForecastAuto$mean)



Zadanie
#A) Sporzadzenie wykresu prognoz punktówych przeksztalconych do CPI: 
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#Proponowane rozwiazanie 
#
#
#
dCPI_Forecast = cbind(ForecastAR1$mean, ForecastAuto$mean, ForecastSAR1$mean)

ln_CPIForecastP = cbind(lnCPI_P, lnCPI_P, lnCPI_P)

Zeros = matrix( rep(0,nrow(dCPI_Forecast)),ncol=1)

ZeroTserie = ts(Zeros, start=c(2018,4), end=c(2020, 3),frequency=12)

ZeroTseries = cbind(ZeroTserie, ZeroTserie, ZeroTserie)
colnames(ZeroTseries)<-colnames(dCPI_Forecast)

ZeroTseries[1,]=ln_CPIForecastP[nrow(ln_CPIForecastP),]+dCPI_Forecast[1,]

for(i in 2:nrow(dCPI_Forecast)){
  ZeroTseries2=ZeroTseries
  ZeroTseries[i,]=ZeroTseries2[i-1,]+dCPI_Forecast[i,]
}

##Zbiór danych przeksztalcony do CPI 
ForecastCPI = exp(ZeroTseries)   

#Wykres - na jednym wykresie 
plot(ForecastCPI, plot.type="s", at="pretty")
#
#
#
#
#
#
#
#Wielowymiarowa analiza szeregów czasowych
#
#
#Zadanie przypominajace - stworzenie szeregów ts dla CPI, lnCPI,  dlnCPI i sdlnCPI dla Niemiec i Francji 
#Tu jest miejsce na Twoje rozwiazanie: 
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

###Proponowane rozwiazane
###Dane_dlaNiemiec

CPI_DE <- CPI_PL_WE4 %>% dplyr::filter(geo == "DE")
MM_DE <- MinMaxDate2 %>% dplyr::filter(geo =="DE")%>% dplyr::select(-c(YB,MB))

CPI_D = ts(CPI_DE[,"NewIndex"],start=c(2000,1), end=c(as.numeric(MM_DE["YE"]), as.numeric(MM_DE["ME"])),frequency=12)
lnCPI_D = log(CPI_D)
dlnCPI_D = diff(lnCPI_D,lag=1)
sdlnCPI_D = diff(lnCPI_D, lag=12)

###DAne

CPI_FR <- CPI_PL_WE4 %>% dplyr::filter(geo == "FR")
MM_FR <- MinMaxDate2 %>% dplyr::filter(geo =="FR")%>% dplyr::select(-c(YB,MB))

CPI_F = ts(CPI_FR[,"NewIndex"],start=c(2000,1), end=c(as.numeric(MM_FR["YE"]), as.numeric(MM_FR["ME"])),frequency=12)
lnCPI_F = log(CPI_F)
dlnCPI_F = diff(lnCPI_F,lag=1)

sdlnCPI_F = diff(lnCPI_F, lag=12)


######Przygotowanie Zbioru do modelu 'VAR

SdCPI <- cbind(sdlnCPI_D,sdlnCPI_F, sdlnCPI_P)

dCPI <- cbind(dlnCPI_D,dlnCPI_F, dlnCPI_P)



#Model VAR - z wykorzystaniem pakietu VAR  
#
#VAR(y, p = 1, type = c("const", "trend", "both", "none"),
  #  season = NULL, exogen = NULL, lag.max = NULL,
   # ic = c("AIC", "HQ", "SC", "FPE"))
#
#

VARdCPI <- VAR(dCPI, p=12, type = "const", season=12, lag.max=18, ic="AIC")
summary(VARdCPI)


VARSdCPI <- VAR(SdCPI,  type = "const",lag.max=12, ic="AIC")
summary(VARSdCPI)


######Funkcje reakcji na impulse (impulseresponsefunctions)
####
irfdCPI <- irf(VARdCPI, n.ahead=40, runs=1000)
plot(irfdCPI)


irfsdCPI <- irf(VARSdCPI, n.ahead=40, runs=1000)

plot(irfsdCPI)


##########Prognozowanie z  modelu VAR.     
#predict(object, ..., n.ahead = 10, ci = 0.95, dumvar = NULL)

PreddCPI  <- predict(VARdCPI , n.ahead = 24)
PredsdCPI  <- predict(VARSdCPI , n.ahead = 24)
###Wizualizacja: Time Series plots of VAR forecasts with differently shaded confidence regions (fanchart) for each endogenous variable.


#f#anchart(x, colors = NULL, cis = NULL, names = NULL, main = NULL, ylab =
 #          NULL, xlab = NULL, col.y = NULL, nc, plot.type = c("multiple",
  #                                                            "single"), mar = par("mar"), oma = par("oma"), ... )

plot(PreddCPI)

plot(PredsdCPI)



fanchart(PreddCPI)

fanchart(PredsdCPI)




