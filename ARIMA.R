#t�m �lkeler
rm(list=ls())
library(COVID19)
library(EnvStats)
library(xlsx)
library(writexl)
library(WriteXLS)
library(MASS)
library(magrittr)
library(qwraps2)
library(readxl)
library(lmtest)
library(dplyr)
library(aTSA)
install.packages("forecast")
library(tseries)
library(forecast)
library(ggplot2)




#T�rkiye


turkiye.all.data=covid19(country = "TURKEY", level=1, start = "2020-03-20", end = "2020-11-25",verbose = FALSE,gmr ="https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")

turkiye.all.data1=covid19(country = "TURKEY", level=1, start = "2020-03-20", end = "2020-11-30",verbose = FALSE,gmr ="https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")


gunluk_vaka=as.matrix(diff(turkiye.all.data$confirmed))

gunluk_�y�lesen=as.matrix(diff(turkiye.all.data$recovered))

gunluk_olum=as.matrix(diff(turkiye.all.data$deaths))


gunluk_�y�lesen1=as.matrix(diff(turkiye.all.data1$recovered))

gunluk_olum1=as.matrix(diff(turkiye.all.data1$deaths))







#T�rkiye i�in adf test# 

a=gunluk_vaka #g�nl�k vaka asl�nda g�nl�k hasta say�s�d�r#

#�nce a dura�an m� bak�l�r. Augmented Dickey-Fuller test. H0: Veri dura�an de�il. p<0.05 ise H0 reddedilir. Yani seri dura�and�r #
adf.test(a,alternative = 'stationary')
#p>0.05 ise 1. dereceden fark al�n�r ve fark al�narak elde edilen serinin dura�anl���na bak�l�r.#
ad1=diff(a,differences = 1)
adf.test(ad1,alternative = 'stationary')
#p<0.05 olup seri dura�and�r# #ARIMA(p,1,q) oldu�u g�r�l�r#


#Box Test#
Box.test(a,type = c("Box-Pierce", "Ljung-Box"),lag=1)

#ACF (MA serisinin derecesini belirlemek i�in kullan�l�r) ve PACF (AR serisinin derecesini belirlemek i�in kullan�l�r) grafiklerinden AR ve Ma serilerinin derecelerini belirleyelim #
tm <- cbind(a, as.matrix(rbind(0,ad1),ncol=1))
plot.ts(tm, xlab="zaman",main="G�nl�k hasta say�lar� ve fark serisi")#Serinin birinci fark� al�nd���na dura�an oldu�u g�r�l�r#

par(mfrow=c(2,2))

ts.plot(a,xlab="zaman", main="G�nl�k Vaka Say�s�")
ts.plot(ad1,xlab="zaman",main="Birinci fark serisi")

#ts.plot(yd2,main="�kinci fark serisi")#
#ts.plot(yd3,main="���nc� fark serisi")#

#par(mfrow=c(1,2))
acf(ad1,main="Otokorelasyon Fonksiyonu")
pacf(ad1,main="K�smi Otokorelasyon Fonksiyonu")


#Burada fit i�in auto arima kullan�larak �ng�r� yapaca��z#

fit1<-auto.arima(gunluk_vaka)
fit1
coeftest(fit1)
fcast1<-(forecast(fit1))
fcast1 # �ng�r�n�n g�ven aral���n�n alt ve �st s�n�rlar�n� verir#
round(fcast1$mean)#de�erler say� oldu�unddan 19,25 gibi kalamaz onun i�in yuvarlama yap�ll�r#
round(fcast1$lower)
round(fcast1$upper)
plot(fcast1,main="G�nl�k Hasta Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast1)#art�klar�n normal da��l�p da��lmad���n�n kontrol�#

######En �yi 3 ARIMA modeli i�in grafik##

real_data<-as.matrix(rbind(6876,6592,6714,6439,6514))#G�nl�k hasta say�s� Sa�l�k Bakanl��� web sitesinden al�nd�(25-26-27-28-29-30 Kas�m i�in)
data<-gunluk_vaka
a.ts <- ts(data,frequency=1)


arima011_css =stats::arima(x = a.ts, order = c(0, 1, 0), method = "CSS")#Arima dereceleri +1,-1 dderece farklar�na sahip olan Arima modellerinden en k���k AIC de�erine sahip olanlar al�narak yap�ld�#
arima011_forecast = forecast(arima011_css, h=5, level=c(99.5))
arima321_css =stats::arima(x = a.ts, order = c(0, 1, 1), method = "CSS")#Arima dereceleri +1,-1 dderece farklar�na sahip olan Arima modellerinden en k���k AIC de�erine sahip olanlar al�narak yap�ld�#
arima321_forecast = forecast(arima321_css, h=5, level=c(99.5))
arima221_css =stats::arima(x = a.ts, order = c(1, 1, 0), method = "CSS")#Arima dereceleri +1,-1 dderece farklar�na sahip olan Arima modellerinden en k���k AIC de�erine sahip olanlar al�narak yap�ld�#
arima221_forecast = forecast(arima221_css, h=5, level=c(99.5))
plot(arima011_forecast)
par(new=T)
plot(arima321_forecast)

par(new=T)
plot(arima221_forecast)


# Ger�ek veriyi, zaman serisine �evirme
a.ts_real <- ts(real_data,start=251,frequency=1) #a.ts'in end noktas� 250, o y�zden 251 ald�m#
autoplot(a.ts,xlab = "Zaman",ylab = "G�nl�k Hasta Say�s�",main = "G�nl�k Hasta Say�lar� ve En �yi 3 ARIMA Modeli") +
  autolayer(arima011_forecast, series = "ARIMA(0,1,0)", alpha = 0.5) +
  autolayer(arima321_forecast, series = "ARIMA(0,1,1)", alpha = 0.5) +
  autolayer(arima221_forecast, series = "ARIMA(1,1,0)", alpha = 0.5) +
  autolayer(a.ts_real, series="Data")+  # ger�ek verinin eklendi�i sat�r
  guides(colour = guide_legend("Model"))

###G�nl�k �yile�en##


b=gunluk_�y�lesen 

#�nce a dura�an m� bak�l�r. Augmented Dickey-Fuller test. H0: Veri dura�an de�il. p<0.05 ise H0 reddedilir. Yani seri dura�and�r #
adf.test(b,alternative = 'stationary')
#p>0fcast1se 1. dereceden fark al�n�r ve fark al�narak elde edilen serinin dura�anl���na bak�l�r.#
bd1=diff(b,differences = 1)
adf.test(bd1,alternative = 'stationary')
#p=0.01653<0.05 olup seri dura�and�r# #ARIMA(p,1,q) oldu�u g�r�l�r#


#Box Test#
Box.test(b,type = c("Box-Pierce", "Ljung-Box"),lag=1)

#ACF (MA serisinin derecesini belirlemek i�in kullan�l�r) ve PACF (AR serisinin derecesini belirlemek i�in kullan�l�r) grafiklerinden AR ve Ma serilerinin derecelerini belirleyelim #
tm1 <- cbind(b, as.matrix(rbind(0,ad1),ncol=1))
plot.ts(tm1, xlab="zaman",main="G�nl�k iyile�en say�lar� ve fark serisi")#Serinin birinci fark� al�nd���na dura�an oldu�u g�r�l�r#

par(mfrow=c(2,2))

ts.plot(b,xlab="zaman", main="G�nl�k iyile�en Say�s�")
ts.plot(bd1,main="Birinci fark serisi")

#ts.plot(yd2,main="�kinci fark serisi")#
#ts.plot(yd3,main="���nc� fark serisi")#

#par(mfrow=c(1,2))
acf(bd1,main="Otokorelasyon Fonksiyonu")
pacf(bd1,main="K�smi Otokorelasyon Fonksiyonu")


#Burada fit i�in auto arima kullan�larak �ng�r� yapaca��z#


fit2<-auto.arima(gunluk_�y�lesen)
fit2
coeftest(fit2)
fcast2<-forecast(fit2)
fcast2
round(fcast2$mean)
round(fcast2$lower)
round(fcast2$upper)
plot(fcast2,main="G�nl�k �yilelen Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast2)#art�klar�n normal da��l�p da��lmad���n�n kontrol�#


#En iyi 3 ARIMA moeli i�in grafik#
gunluk_iyilesen_1=(gunluk_�y�lesen1[250:255,])
real_data1<-as.matrix(rbind(gunluk_iyilesen_1))#G�nl�k hasta say�s� Sa�l�k Bakanl��� web sitesinden al�nd�(25-26-27-28-29-30 Kas�m i�in)
data1<-gunluk_vaka
a.ts1 <- ts(data1,frequency=1)


arima011_css =stats::arima(x = a.ts1, order = c(1, 0, 0), method = "CSS")#Arima dereceleri +1,-1 dderece farklar�na sahip olan Arima modellerinden en k���k AIC de�erine sahip olanlar al�narak yap�ld�#
arima011_forecast = forecast(arima011_css, h=5, level=c(99.5))
arima321_css =stats::arima(x = a.ts1, order = c(2, 0, 0), method = "CSS")#Arima dereceleri +1,-1 dderece farklar�na sahip olan Arima modellerinden en k���k AIC de�erine sahip olanlar al�narak yap�ld�#
arima321_forecast = forecast(arima321_css, h=5, level=c(99.5))
arima221_css =stats::arima(x = a.ts1, order = c(2, 0, 1), method = "CSS")#Arima dereceleri +1,-1 dderece farklar�na sahip olan Arima modellerinden en k���k AIC de�erine sahip olanlar al�narak yap�ld�#
arima221_forecast = forecast(arima221_css, h=5, level=c(99.5))
plot(arima011_forecast)
par(new=T)
plot(arima321_forecast)

par(new=T)
plot(arima221_forecast)


# Ger�ek veriyi, zaman serisine �evirme
a.ts_real1 <- ts(real_data1,start=251,frequency=1) #a.ts'in end noktas� 250, o y�zden 251 ald�m#
autoplot(a.ts1,xlab = "Zaman",ylab = "G�nl�k �yile�en Say�s�",main = "G�nl�k �yile�en Say�lar� ve En �yi 3 ARIMA Modeli") +
  autolayer(arima011_forecast, series = "ARIMA(1,0,0)", alpha = 0.5) +
  autolayer(arima321_forecast, series = "ARIMA(2,0,0)", alpha = 0.5) +
  autolayer(arima221_forecast, series = "ARIMA(2,0,1)", alpha = 0.5) +
  autolayer(a.ts_real, series="Data")+  # ger�ek verinin eklendi�i sat�r
  guides(colour = guide_legend("Model"))


c=gunluk_olum
#�nce a dura�an m� bak�l�r. Augmented Dickey-Fuller test. H0: Veri dura�an de�il. p<0.05 ise H0 reddedilir. Yani seri dura�and�r #
adf.test(c,alternative = 'stationary')
#p>0.05 ise 1. dereceden fark al�n�r ve fark al�narak elde edilen serinin dura�anl���na bak�l�r.#
cd1=diff(c,differences = 1)
adf.test(cd1,alternative = 'stationary')
#p>0.05 ise 2. dereceden fark al�n�r ve fark al�narak elde edilen serinin dura�anl���na bak�l�r.#
cd2=diff(c,differences = 2)
adf.test(cd2,alternative = 'stationary')
#p<0.05 olup seri dura�and�r# #ARIMA(p,2,q) oldu�u g�r�l�r#

#Box Test#
Box.test(c,type = c("Box-Pierce", "Ljung-Box"),lag=2)

#ACF (MA serisinin derecesini belirlemek i�in kullan�l�r) ve PACF (AR serisinin derecesini belirlemek i�in kullan�l�r) grafiklerinden AR ve Ma serilerinin derecelerini belirleyelim #
c1<-as.matrix(c[1:249,])#2.fark serisiyle boyutlar� e�itlemek i�in 1'en 249'a kadar olan de�erleri ald�k
cd11<-as.matrix(cd1[1:248,])#2.fark serisiyle boyutlar� e�itlemek i�in 1'en 248'e kadar olan de�erleri ald�k
tm3 <- cbind(c1, as.matrix(rbind(0,cd11),ncol=1)  ,as.matrix(rbind(0,cd2),ncol=1))
plot.ts(tm3, xlab="zaman",main="G�nl�k �l�m say�lar� ve fark serisi")#Serinin birinci fark� al�nd���na dura�an oldu�u g�r�l�r#

par(mfrow=c(3,2))

ts.plot(c,xlab="zaman", main="G�nl�k �l�m Say�s�")
ts.plot(cd1,main="Birinci fark serisi")
ts.plot(cd2,main="�kinci fark serisi")

#ts.plot(yd2,main="�kinci fark serisi")#
#ts.plot(yd3,main="���nc� fark serisi")#

#par(mfrow=c(1,2))
acf(cd2,main="Otokorelasyon Fonksiyonu")
pacf(cd2,main="K�smi Otokorelasyon Fonksiyonu")


#Burada fit i�in auto arima kullan�larak �ng�r� yapaca��z#

fit3<-auto.arima(gunluk_olum)
fit3
coeftest(fit3)# Burada p de�erlerini almak i�in coeftest komutunu kulland�k#
fcast3<-forecast(fit3)
fcast3
round(fcast3$mean)
round(fcast3$lower)
round(fcast3$upper)
plot(fcast3,main="G�nl�k �l�m Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast3)#art�klar�n testi#

gunluk_olum_1=(gunluk_olum1[250:255,])
real_data2<-as.matrix(rbind(gunluk_olum_1))#G�nl�k hasta say�s� Sa�l�k Bakanl��� web sitesinden al�nd�(25-26-27-28-29-30 Kas�m i�in)
data2<-gunluk_vaka
a.ts2 <- ts(data2,frequency=1)


arima011_css =stats::arima(x = a.ts1, order = c(2, 2, 1), method = "CSS")#Arima dereceleri +1,-1 dderece farklar�na sahip olan Arima modellerinden en k���k AIC de�erine sahip olanlar al�narak yap�ld�#
arima011_forecast = forecast(arima011_css, h=5, level=c(99.5))
arima321_css =stats::arima(x = a.ts1, order = c(2, 2, 3), method = "CSS")#Arima dereceleri +1,-1 dderece farklar�na sahip olan Arima modellerinden en k���k AIC de�erine sahip olanlar al�narak yap�ld�#
arima321_forecast = forecast(arima321_css, h=5, level=c(99.5))
arima221_css =stats::arima(x = a.ts1, order = c(3, 2, 2), method = "CSS")#Arima dereceleri +1,-1 dderece farklar�na sahip olan Arima modellerinden en k���k AIC de�erine sahip olanlar al�narak yap�ld�#
arima221_forecast = forecast(arima221_css, h=5, level=c(99.5))
plot(arima011_forecast)
par(new=T)
plot(arima321_forecast)

par(new=T)
plot(arima221_forecast)


# Ger�ek veriyi, zaman serisine �evirme
a.ts_real1 <- ts(real_data2,start=251,frequency=1) #a.ts'in end noktas� 250, o y�zden 251 ald�m#
autoplot(a.ts2,xlab = "Zaman",ylab = "G�nl�k �yile�en Say�s�",main = "G�nl�k �yile�en Say�lar� ve En �yi 3 ARIMA Modeli") +
  autolayer(arima011_forecast, series = "ARIMA(2,2,1)", alpha = 0.5) +
  autolayer(arima321_forecast, series = "ARIMA(2,2,3)", alpha = 0.5) +
  autolayer(arima221_forecast, series = "ARIMA(3,2,2)", alpha = 0.5) +
  autolayer(a.ts_real, series="Data")+  # ger�ek verinin eklendi�i sat�r
  guides(colour = guide_legend("Model"))




#ALMANYA

almanya.all.data=covid19(country = "GERMANY", level=1, start = "2020-03-03", end = "2020-11-25",verbose = FALSE,gmr ="https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
#almanya.all.data=covid19(country = "GERMANY", level=1, start = "2020-11-25", end = "2020-11-30",verbose = FALSE,gmr ="https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")


gunluk_vaka_almanya=as.matrix(diff(almanya.all.data$confirmed))

gunluk_�y�lesen_almanya=as.matrix(diff(almanya.all.data$recovered))

gunluk_olum_almanya=as.matrix(diff(almanya.all.data$deaths))


d=gunluk_vaka_almanya #g�nl�k vaka asl�nda g�nl�k hasta say�s�d�r#

#�nce a dura�an m� bak�l�r. Augmented Dickey-Fuller test. H0: Veri dura�an de�il. p<0.05 ise H0 reddedilir. Yani seri dura�and�r #
adf.test(d,alternative = 'stationary')
#p>0.05 ise 1. dereceden fark al�n�r ve fark al�narak elde edilen serinin dura�anl���na bak�l�r.#
ad1=diff(d,differences = 1)
adf.test(ad1,alternative = 'stationary')
#p<0.05 olup seri dura�and�r# #ARIMA(p,1,q) oldu�u g�r�l�r#


#Box Test#
Box.test(a,type = c("Box-Pierce", "Ljung-Box"),lag=1)

#ACF (MA serisinin derecesini belirlemek i�in kullan�l�r) ve PACF (AR serisinin derecesini belirlemek i�in kullan�l�r) grafiklerinden AR ve Ma serilerinin derecelerini belirleyelim #
tm <- cbind(a, as.matrix(rbind(0,ad1),ncol=1))
plot.ts(tm, xlab="zaman",main="G�nl�k hasta say�lar� ve fark serisi")#Serinin birinci fark� al�nd���na dura�an oldu�u g�r�l�r#

par(mfrow=c(2,2))

ts.plot(a,xlab="zaman", main="G�nl�k Vaka Say�s�")
ts.plot(ad1,xlab="zaman",main="Birinci fark serisi")

#ts.plot(yd2,main="�kinci fark serisi")#
#ts.plot(yd3,main="���nc� fark serisi")#

#par(mfrow=c(1,2))
acf(ad1,main="Otokorelasyon Fonksiyonu")
pacf(ad1,main="K�smi Otokorelasyon Fonksiyonu")




fit4<-auto.arima(gunluk_vaka_almanya)
fit4
fcast4<-forecast(fit4)
fcast4
round(fcast4$mean)
round(fcast4$lower)
round(fcast4$upper)
plot(fcast4,main="G�nl�k Vaka Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast4)#art�klar�n testi#
real_gunluk_vaka_almanya=as.matrix(diff(falmanya.all.data$confirmed))
real_gunluk_vaka_almanya



fit5<-auto.arima(gunluk_�y�lesen_almanya)
fit5
fcast5<-forecast(fit5)
fcast5
round(fcast5$mean)
round(fcast5$lower)
round(fcast5$upper)
plot(fcast5,main="G�nl�k �yile�en Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast5)#art�klar�n testi#
real_gunluk_�y�lesen_almanya=as.matrix(diff(falmanya.all.data$recovered))
real_gunluk_�y�lesen_almanya



fit6<-auto.arima(gunluk_olum_almanya)
fit6
fcast6<-forecast(fit6)
fcast6
round(fcast6$mean)
round(fcast6$lower)
round(fcast6$upper)
plot(fcast6,main="G�nl�k �l�m Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast6)#art�klar�n testi#
real_gunluk_olum_almanya=as.matrix(diff(falmanya.all.data$deaths))
real_gunluk_olum_almanya



#��N

�in.all.data=covid19(country = "China", level=1, start = "2020-01-22", end = "2020-11-25",verbose = FALSE,gmr ="https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
f�in.all.data=covid19(country = "China", level=1, start = "2020-11-25", end = "2020-11-30",verbose = FALSE,gmr ="https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")



gunluk_vaka_cin=as.matrix(diff(�in.all.data$confirmed))

gunluk_�y�lesen_cin=as.matrix(diff(�in.all.data$recovered))

gunluk_olum_cin=as.matrix(diff(�in.all.data$deaths))



fit7<-auto.arima(gunluk_vaka_cin)
fit7
fcast7<-forecast(fit7)
fcast7
round(fcast7$mean)
round(fcast7$lower)
round(fcast7$upper)
plot(fcast7,main="G�nl�k Vaka Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast7)#art�klar�n testi#
real_gunluk_vaka_cin=as.matrix(diff(f�in.all.data$confirmed))
real_gunluk_vaka_cin


#�in'de s�k�nt� var o y�zden
#library(coronavirus)

#gi=filter(type == "recovered", country == "China") %>%
#group_by(country)
#gunluk_iyilesen_cin=as.matrix(gi$cases)

fit8<-auto.arima(gunluk_�y�lesen_cin)
fit8
fcast8<-forecast(fit8)
fcast8
round(fcast8$mean)
round(fcast8$lower)
round(fcast8$upper)
plot(fcast8,main="G�nl�k �yile�en Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast8)#art�klar�n testi#
real_gunluk_�y�lesen_cin=as.matrix(diff(f�in.all.data$recovered))
real_gunluk_�y�lesen_cin



fit9<-auto.arima(gunluk_olum_cin)
fit9
fcast9<-forecast(fit9)
fcast9
round(fcast9$mean)
round(fcast9$lower)
round(fcast9$upper)
plot(fcast9,main="G�nl�k �l�m Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast9)#art�klar�n testi#
real_gunluk_olum_cin=as.matrix(diff(f�in.all.data$deaths))
real_gunluk_olum_cin



#G�ney Kore


guneykore.all.data<-covid19(c("KOR"),verbose = FALSE,level=1,start ="2020-02-20", end = "2020-11-25")
fguneykore.all.data<-covid19(c("KOR"),verbose = FALSE,level=1,start = "2020-11-25", end = "2020-11-30")


gunluk_vaka_guneykore=as.matrix(diff(guneykore.all.data$confirmed))

gunluk_iyilesen_guneykore=as.matrix(diff(guneykore.all.data$recovered))

gunluk_olum_guneykore=as.matrix(diff(guneykore.all.data$deaths))



fit10<-auto.arima(gunluk_vaka_guneykore)
fit10
fcast10<-forecast(fit10)
fcast10
round(fcast10$mean)
round(fcast10$lower)
round(fcast10$upper)
plot(fcast10,main="G�nl�k Vaka Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast10)#art�klar�n testi#
real_gunluk_vaka_guneykore=as.matrix(diff(fguneykore.all.data$confirmed))
real_gunluk_vaka_guneykore



fit11<-auto.arima(gunluk_iyilesen_guneykore)
fit11
fcast11<-forecast(fit11)
fcast11
round(fcast11$mean)
round(fcast11$lower)
round(fcast11$upper)
plot(fcast11,main="G�nl�k �yile�en Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast11)#art�klar�n testi#
real_gunluk_�y�lesen_guneykore=as.matrix(diff(fguneykore.all.data$recovered))
real_gunluk_�y�lesen_guneykore



fit12<-auto.arima(gunluk_olum_guneykore)
fit12
fcast12<-forecast(fit12)
fcast12
round(fcast12$mean)
round(fcast12$lower)
round(fcast12$upper)
plot(fcast12,main="G�nl�k �l�m Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast12)#art�klar�n testi#
real_gunluk_olum_guneykore=as.matrix(diff(fguneykore.all.data$deaths))
real_gunluk_olum_guneykore



#JAPONYA


japonya.all.data=covid19(country = "Japan", level=1, start = "2020-03-27", end = "2020-11-25",verbose = FALSE,gmr ="https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
fjaponya.all.data=covid19(country = "Japan", level=1, start = "2020-11-25", end = "2020-11-30",verbose = FALSE,gmr ="https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")



gunluk_vaka_japonya=as.matrix(diff(japonya.all.data$confirmed))

gunluk_iyilesen_japonya=as.matrix(diff(japonya.all.data$recovered))

gunluk_olum_japonya=as.matrix(diff(japonya.all.data$deaths))


fit13<-auto.arima(gunluk_vaka_japonya)
fit13
fcast13<-forecast(fit13)
fcast13
round(fcast13$mean)
round(fcast13$lower)
round(fcast13$upper)
plot(fcast13,main="G�nl�k Vaka Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast13)#art�klar�n testi#
#real_gunluk_vaka_japonya=as.matrix(diff(fjaponya.all.data$confirmed))
#real_gunluk_vaka_japonya



fit14<-auto.arima(gunluk_iyilesen_japonya)
fit14
fcast14<-forecast(fit14)
fcast14
round(fcast14$mean)
round(fcast14$lower)
round(fcast14$upper)
plot(fcast14,main="G�nl�k �yile�en Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast14)#art�klar�n testi#
#real_gunluk_�y�lesen_japonya=as.matrix(diff(fjaponya.all.data$recovered))
#real_gunluk_�y�lesen_japonya



fit15<-auto.arima(gunluk_olum_japonya)
fit15
fcast15<-forecast(fit12)
fcast15
round(fcast15$mean)
round(fcast15$lower)
round(fcast15$upper)
plot(fcast15,main="G�nl�k �l�m Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast15)#art�klar�n testi#
#real_gunluk_olum_japonya=as.matrix(diff(fjaponya.all.data$deaths))
#real_gunluk_olum_japonya

#�SPANYA


ispanya.all.data=covid19(country = "SPAIN", level=1, start = "2020-03-06", end = "2020-11-25",verbose = FALSE,gmr ="https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
fispanya.all.data=covid19(country = "SPAIN", level=1, start = "2020-11-25", end = "2020-11-30",verbose = FALSE,gmr ="https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")



gunluk_vaka_ispanya=as.matrix(diff(ispanya.all.data$confirmed))

gunluk_iyilesen_ispanya=as.matrix(diff(ispanya.all.data$recovered))

gunluk_olum_ispanya=as.matrix(diff(ispanya.all.data$deaths))


fit16<-auto.arima(gunluk_vaka_ispanya)
fit16
fcast16<-forecast(fit16)
fcast16
round(fcast16$mean)
round(fcast16$lower)
round(fcast16$upper)
plot(fcast16,main="G�nl�k Vaka Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast16)#art�klar�n testi#
#real_gunluk_vaka_ispanya=as.matrix(diff(fispanya.all.data$confirmed))
#real_gunluk_vaka_ispanya



fit17<-auto.arima(gunluk_iyilesen_ispanya)
fit17
fcast17<-forecast(fit17)
fcast17
round(fcast17$mean)
round(fcast17$lower)
round(fcast17$upper)
plot(fcast17,main="G�nl�k �yile�en Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast17)#art�klar�n testi#
#real_gunluk_�y�lesen_ispanya=as.matrix(diff(fispanya.all.data$recovered))
#real_gunluk_�y�lesen_ispanya



fit18<-auto.arima(gunluk_olum_ispanya)
fit18
fcast18<-forecast(fit18)
fcast18
round(fcast18$mean)
round(fcast18$lower)
round(fcast18$upper)
plot(fcast18,main="G�nl�k �l�m Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast18)#art�klar�n testi#
real_gunluk_olum_ispanya=as.matrix(diff(fispanya.all.data$deaths))
real_gunluk_olum_ispanya



#FRANSA

fransa.all.data<-covid19(c("FRANCE"),verbose = FALSE,level=1,start ="2020-03-05", end = "2020-11-25")
ffransa.all.data<-covid19(c("FRANCE"),verbose = FALSE,level=1,start = "2020-11-25", end = "2020-11-30")



gvaka=as.matrix(diff(fransa.all.data$confirmed))
gunluk_vaka_fransa=as.matrix(na.omit(gvaka))



gunlukiyilesen=as.matrix(diff(fransa.all.data$recovered))
gunluk_iyilesen_fransa=as.matrix(na.omit(gunlukiyilesen))



gunlukolum=as.matrix(diff(fransa.all.data$deaths))
gunluk_olum_fransa=as.matrix(na.omit(gunlukolum))


fit19<-auto.arima(gunluk_vaka_fransa)
fit19
fcast19<-forecast(fit19)
fcast19
round(fcast19$mean)
round(fcast19$lower)
round(fcast19$upper)
plot(fcast319,main="G�nl�k Vaka Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast19)#art�klar�n testi#
real_gunluk_vaka_fransa=as.matrix(diff(ffransa.all.data$confirmed))
real_gunluk_vaka_fransa



fit20<-auto.arima(gunluk_iyilesen_fransa)
fit20
fcast20<-forecast(fit20)
fcast20
round(fcast20$mean)
round(fcast20$lower)
round(fcast20$upper)
plot(fcast20,main="G�nl�k �yile�en Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast20)#art�klar�n testi#
real_gunluk_�y�lesen_fransa=as.matrix(diff(ffransa.all.data$recovered))
real_gunluk_�y�lesen_fransa



fit21<-auto.arima(gunluk_olum_fransa)
fit21
fcast21<-forecast(fit21)
fcast21
round(fcast21$mean)
round(fcast21$lower)
round(fcast21$upper)
plot(fcast21,main="G�nl�k �l�m Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast21)#art�klar�n testi#
real_gunluk_olum_fransa=as.matrix(diff(ffransa.all.data$deaths))
real_gunluk_olum_fransa



#iTALYA

italya.all.data=covid19(country = "ITALY", level=1, start = "2020-02-26", end = "2020-11-25",verbose = FALSE,gmr ="https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
fitalya.all.data=covid19(country = "ITALY", level=1, start = "2020-11-25", end = "2020-11-30",verbose = FALSE,gmr ="https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")



gunluk_vaka_italya=as.matrix(diff(italya.all.data$confirmed))


gunluk_iyilesen_italya=as.matrix(diff(italya.all.data$recovered))


gunluk_olum_italya=as.matrix(diff(italya.all.data$deaths))


fit22<-auto.arima(gunluk_vaka_italya)
fit22
fcast22<-forecast(fit22)
fcast22
round(fcast22$mean)
round(fcast22$lower)
round(fcast22$upper)
plot(fcast22,main="G�nl�k Vaka Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast22)#art�klar�n testi#
real_gunluk_vaka_italya=as.matrix(diff(fitalya.all.data$confirmed))
real_gunluk_vaka_italya



fit23<-auto.arima(gunluk_iyilesen_italya)
fit23
fcast23<-forecast(fit23)
fcast23
round(fcast23$mean)
round(fcast23$lower)
round(fcast23$upper)
plot(fcast23,main="G�nl�k �yile�en Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast23)#art�klar�n testi#
real_gunluk_�y�lesen_italya=as.matrix(diff(fitalya.all.data$recovered))
real_gunluk_�y�lesen_italya



fit24<-auto.arima(gunluk_olum_italya)
fit24
fcast24<-forecast(fit24)
fcast24
round(fcast24$mean)
round(fcast24$lower)
round(fcast24$upper)
plot(fcast24,main="G�nl�k �l�m Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast24)#art�klar�n testi#
real_gunluk_olum_italya=as.matrix(diff(fitalya.all.data$deaths))
real_gunluk_olum_italya



#AVUSTRALYA



avustralya.all.data=covid19(country = "AUSTRALIA", level=1, start = "2020-03-19", end = "2020-11-25",verbose = FALSE,gmr ="https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
favustralya.all.data=covid19(country = "AUSTRALIA", level=1, start = "2020-11-25", end = "2020-11-30",verbose = FALSE,gmr ="https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")



gunluk_vaka_avustralya=as.matrix(diff(avustralya.all.data$confirmed))

gunluk_iyilesen_avustralya=as.matrix(diff(avustralya.all.data$recovered))

gunluk_olum_avustralya=as.matrix(diff(avustralya.all.data$deaths))



fit25<-auto.arima(gunluk_vaka_avustralya)
fit25
fcast25<-forecast(fit25)
fcast25
round(fcast25$mean)
round(fcast25$lower)
round(fcast25$upper)
plot(fcast25,main="G�nl�k Vaka Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast25)#art�klar�n testi#
real_gunluk_vaka_avustralya=as.matrix(diff(favustralya.all.data$confirmed))
real_gunluk_vaka_avustralya



fit26<-auto.arima(gunluk_iyilesen_avustralya)
fit26
fcast26<-forecast(fit26)
fcast26
round(fcast26$mean)
round(fcast26$lower)
round(fcast26$upper)
plot(fcast26,main="G�nl�k �yile�en Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast26)#art�klar�n testi#
real_gunluk_�y�lesen_avustralya=as.matrix(diff(favustralya.all.data$recovered))
real_gunluk_�y�lesen_avustralya



fit27<-auto.arima(gunluk_olum_avustralya)
fit27
fcast27<-forecast(fit27)
fcast27
round(fcast27$mean)
round(fcast27$lower)
round(fcast27$upper)
plot(fcast27,main="G�nl�k �l�m Say�s�") #�ng�r�n�n alt ve �st s�n�r�n�n grafi�i#
checkresiduals(fcast27)#art�klar�n testi#
real_gunluk_olum_avustralya=as.matrix(diff(favustralya.all.data$deaths))
real_gunluk_olum_avustralya











