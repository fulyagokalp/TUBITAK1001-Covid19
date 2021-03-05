####Poisson Regression
rm(list=ls())
library(COVID19)
library(EnvStats)
library(MASS)
library(npbr)
library(vcd)  ##good fit için
library(maxLik)
library(npbr)
library(tscount) ##tsglm için 
library(tidyverse)
options("scipen"=100, digits = 2)

data<-covid19(c("Turkey"),verbose = FALSE,level=1,start = "2020-03-18", end = "2020-11-25")
all.data<-covid19(c("Turkey"),verbose = FALSE,level=1,start = "2020-03-18", end = "2020-11-30")
##Aþaðýdaki 3 satýr Türkiye verisine özgü olarak girildi.#####################
##Pakette vaka sayýsý verildiði ancak 25 Kasým 2020 sonrasýnda verilen sayýlarýn hasta sayýsý olduðu bildirildiði ve hasta sayýsý ile çalýþtýðýmýz için bu kýsým manuel girildi. 
all.data[254:258,4]=as.matrix(c(474606, 481198,487912,494351,500865),ncol=1)
all.data[254:258,5]=as.matrix(c(388771,392616,396227,400242,404727),ncol=1)
all.data[254:258,6]=as.matrix(c(13014, 13191,13373,	13558,13746),ncol=1)
###########################################################################################################

#data<-covid19(c("ZAF"),verbose = FALSE,level=1,start ="2020-03-17", end = "2020-11-25")
#all.data<-covid19(c("ZAF"),verbose = FALSE,level=1,start = "2020-03-17", end = "2020-11-30")

#data<-covid19(c("CHN"),verbose = FALSE,level=1,start ="2020-01-19", end = "2020-11-25")
#all.data<-covid19(c("CHN"),verbose = FALSE,level=1,start = "2020-01-19", end = "2020-11-30")

#data<-covid19(c("KOR"),verbose = FALSE,level=1,start ="2020-02-20", end = "2020-11-25")
#all.data<-covid19(c("KOR"),verbose = FALSE,level=1,start = "2020-02-20", end = "2020-11-30")

#data<-covid19(c("IND"),verbose = FALSE,level=1,start ="2020-03-14", end = "2020-11-25")
#all.data<-covid19(c("IND"),verbose = FALSE,level=1,start = "2020-03-14", end = "2020-11-30")

#data<-covid19(c("IRN"),verbose = FALSE,level=1,start ="2020-02-25", end = "2020-11-25")
#all.data<-covid19(c("IRN"),verbose = FALSE,level=1,start = "2020-02-25", end = "2020-11-30")

#data<-covid19(c("JPN"),verbose = FALSE,level=1,start ="2020-02-22", end = "2020-11-25")
#all.data<-covid19(c("JPN"),verbose = FALSE,level=1,start = "2020-02-22", end = "2020-11-30")

#data<-covid19(c("RUS"),verbose = FALSE,level=1,start ="2020-03-16", end = "2020-11-25")
#all.data<-covid19(c("RUS"),verbose = FALSE,level=1,start = "2020-03-16", end = "2020-11-30")

#data<-covid19(c("Germany"),verbose = FALSE,level=1,start ="2020-02-28", end = "2020-11-25")
#all.data<-covid19(c("Germany"),verbose = FALSE,level=1,start = "2020-02-28", end = "2020-11-30")

#data<-covid19(c("FRA"),verbose = FALSE,level=1,start ="2020-02-29", end = "2020-11-25")
#all.data<-covid19(c("FRA"),verbose = FALSE,level=1,start = "2020-02-29", end = "2020-11-30")

#data<-covid19(c("ESP"),verbose = FALSE,level=1,start ="2020-03-01", end = "2020-11-25")
#all.data<-covid19(c("ESP"),verbose = FALSE,level=1,start = "2020-03-01", end = "2020-11-30")

#data<-covid19(c("ITA"),verbose = FALSE,level=1,start ="2020-02-24", end = "2020-11-25")
#all.data<-covid19(c("ITA"),verbose = FALSE,level=1,start = "2020-02-24", end = "2020-11-30")

#data<-covid19(c("BRA"),verbose = FALSE,level=1,start ="2020-03-12", end = "2020-11-25")
#all.data<-covid19(c("BRA"),verbose = FALSE,level=1,start = "2020-03-12", end = "2020-11-30")

#data<-covid19(c("COL"),verbose = FALSE,level=1,start ="2020-03-18", end = "2020-11-25")
#all.data<-covid19(c("COL"),verbose = FALSE,level=1,start = "2020-03-18", end = "2020-11-30")

#data<-covid19(c("USA"),verbose = FALSE,level=1,start ="2020-03-04", end = "2020-11-25")
#all.data<-covid19(c("USA"),verbose = FALSE,level=1,start = "2020-03-04", end = "2020-11-30")

#data<-covid19(c("CAN"),verbose = FALSE,level=1,start ="2020-03-11", end = "2020-11-25")
#all.data<-covid19(c("CAN"),verbose = FALSE,level=1,start = "2020-03-11", end = "2020-11-30")

#data<-covid19(c("AUS"),verbose = FALSE,level=1,start ="2020-03-09", end = "2020-11-25")
#all.data<-covid19(c("AUS"),verbose = FALSE,level=1,start = "2020-03-09", end = "2020-11-30")

#China_gunluk_iyilesen<-matrix(c(28,2,6,3,10,9,43,19,15,79,61,188,151,229,272,362,522,597,623,699,718,446,1135,1760,1321,1457,1707,1744,1756,2052,690,3995,488,1828,2661,2408,2846,3399,2991,2842,2692,2596,2551,2291,1652,1595,1849,1416,1377,1463,1257,1295,1464,1357,893,888,957,780,731,591,505,452,466,493,408,539,380,482,341,283,199,160,195,186,261,103,100,157,112,112,86,79,83,161,111,90,-849,62,76,55,54,62,122,126,66,102,97,48,52,49,50,13,98,108,78,59,48,16,134,40,31,24,24,15,20,12,13,4,0,0,0,22,3,8,9,6,9,4,11,4,3,9,2,4,11,5,4,10,10,7,5,8,8,3,7,7,4,17,5,0,19,1,12,8,7,10,8,11,18,10,13,18,15,15,26,12,7,29,48,0,0,74,31,0,60,38,33,26,28,45,25,42,40,53,44,68,50,7,51,77,86,107,120,111,95,121,183,175,196,181,156,122,193,193,171,152,152,92,83,125,158,97,130,131,64,70,110,83,84,84,65,62,48,58,43,42,45,55,36,27,32,25,48,20,29,32,15,28,26,26,23,22,13,18,18,29,19,20,17,21,17,25,37,19,15,17,19,10,26,22,24,29,17,11,21,17,13,23,19,29,28,21,33,26,30,21,24,28,16,13,18,33,29,24,27,27,23,29,23,37,43,25,24,28,30,37,30,25,29,44,51,33,27,24,18,23,24,31,41),ncol=1)
#toplam_iyilesen=matrix(rep(0,309),ncol=1)
#toplam_iyilesen[1]=China_gunluk_iyilesen[1]
#for (i in 2:309) {
#  toplam_iyilesen[i] <- toplam_iyilesen[i-1]+China_gunluk_iyilesen[i]
#}
#toplam_iyilesen
#Chinasonbesgun<-c(86974,87072,87167,87300,87388)


n=dim(data)[1]
tarih=data$date
Sira=as.matrix(seq(1,n),ncol=1)

###Toplam vaka, toplam iyileþen ve toplam ölüm  sayýsý#####
tarih=data$date
toplam_test=as.vector(data$tests)
gunluk_test=diff(toplam_test)
toplam_vaka=data$confirmed
gunluk_vaka=diff(data$confirmed)
toplam_iyilesen=data$recovered
gunluk_iyilesen=diff(data$recovered)
toplam_olum=data$deaths
gunluk_olum=diff(data$deaths)
gunluk_entube=data$vent
gunluk_younbakim=data$icu
toplam_nufus=data$population
gunluk_iyilesen[9]=4
gunluk_iyilesen[10]=4
toplam_iyilesen[8]=28
toplam_iyilesen[9]=30
toplam_iyilesen[10]=34
toplam_iyilesen2=toplam_iyilesen[-c(1,2,3,4,5,6)]


aktifvaka=toplam_vaka- toplam_olum - toplam_iyilesen

y=as.matrix(aktifvaka,nrow=1)
n=length(y)
x=as.matrix(Sira[1:n],nrow=1)

####Explore data
par(mfrow=c(1,2))
boxplot(y, horizontal=TRUE,main="Aktif vaka")
rug(jitter(y), side=1)
par(new=FALSE)
distplot(as.numeric(y), type='poisson')

fit <- goodfit(as.vector(y), type='poisson')
summary(fit)
rootogram(fit)
distplot(as.vector(y), type='poisson')

s1<-tsglm(aktifvaka, model = list(past_obs = 1), link = "log",distr = "poisson")
plot(s1)
summary(s1)
se(s1)
fitted(s1)
residuals(s1)
AIC(s1)
plot(fitted(s1),aktifvaka)

#############Time trend model


#p=poly_degree(x, as.matrix(log(y[1:n]),ncol=1), prange=0:10, type="AIC", control = list("tm_limit" = 700))
#model <- lm(y[1:n] ~ poly(x,p))
#model<-glm(aktifvaka~tarih,family="poisson")
#ss1=summary(model)
#ss1

boy=6
Akaike=as.matrix(rep(0,boy),nrow=boy,ncol=1)
R2=as.matrix(rep(0,boy),nrow=boy,ncol=1)
times <- 1:n
times2 <- times^2
times3 <- times^3
times4<-times^4
times5<-times^5
times6<-times^6
times7<-times^7
times8<-times^8
timesnew=cbind(times,times2,times3,times4,times5,times6)  ##Derece deðiþtirilebilir
timesson=times

for (k in 1:boy) {
  model <- glm(formula = y ~ timesson  , family = poisson(link = log))
  ss=summary(model)
  Akaike[k]=AIC(model)
  timesson=cbind(timesson,times^(k+1))
  k
}

timesson=times
for (k in 2:(boy)) {
  timesson=cbind(timesson,times^(k))
}

model <- glm(formula = y ~ timesson  , family = poisson(link = log))
fits=model$fitted.values
res=residuals.glm(model)
beta=as.matrix(model$coefficients,ncol=1)

timesn <- 1:(n+5)
times22 <- timesn^2
times33 <- timesn^3
times44<-timesn^4
times55<-timesn^5
times66<-timesn^6
times77<-timesn^7
times88<-timesn^8
timesnew2=t(rbind(timesn,times22,times33,times44,times55,times66)) ##Derece 6 alýndýðý için bu þekilde tanýmlý.

pred=exp(cbind(matrix(rep(1,5),ncol=1),timesnew2[(n+1):(n+5),])%*%matrix(beta,ncol=1))
beta

######Plots
predicted.intervals <-exp (predict(model,data.frame(x=x),interval='confidence',level=0.90))
plot(y~as.Date(tarih[1:n]),xlab="Tarih",ylab = "Toplam vaka sayýsý",col="deepskyblue4",pch=20, main="",lwd=1,lty=1)
lines(predicted.intervals[,1]~as.Date(tarih[1:n]),col='red',lwd=2,lty=1)
legend("bottomright",c("Aktif vaka sayýsý","Tahmin"), col=c("deepskyblue4","red"), lty=c(NA,1),pch=c(c(20),NA),lwd=c(2))

n1<-n+1
n5<-n+5
a<-as.matrix(all.data[n1:n5,4],ncol=1)
b<-as.matrix(all.data[n1:n5,5],ncol=1)
c<-as.matrix(all.data[n1:n5,6],ncol=1)

