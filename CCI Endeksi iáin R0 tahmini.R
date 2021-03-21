rm(list=ls())
library(Metrics)
library(MASS)
library(COVID19)
library(EnvStats)
library(xlsx)
library(writexl)
library(MASS)
library(npbr)
library(maxLik)
library(npbr)
library(robustbase)
library(tscount) ##tsglm iÃ§in 
library(coronavirus)
library(ggpubr)
library(ggpmisc)
library(scales)
library(tseries)
library(forecast)
library(ggplot2)
library(deSolve)
library(R0)
options("scipen"=100, digits = 2)
Sys.setlocale(category = "LC_ALL", locale = "turkish")
#####TÃ¼rkiye
data<-covid19(c("Turkey"),verbose = FALSE,level=1,start = "2020-03-18", end = "2020-11-30")



n=dim(data)[1]

tarih=data$date
Sira=as.matrix(seq(1,n),ncol=1)
tarih=data$date
toplam_test=as.vector(data$tests)
#gunluk_test=diff(all.data$tests)
toplam_vaka=data$confirmed
gunluk_vaka=diff(data$confirmed)
toplam_iyilesen=data$recovered
#gunluk_iyilesen=diff(all.data$recovered)
toplam_olum=data$deaths
gunluk_olum=diff(data$deaths)
toplam_nufus=data$population
##SIR##
S=56875101#etkilenmesi beklenen 20 yaş üstü nüfus
Ind=gunluk_vaka ##30 kasım tarihindeki toplam hastalanan sayısı
R=gunluk_olum ## 30 kasım tarihindeki toplam ölüm sayısı
N=toplam_nufus[1]
R0=1.2 #ortalama R0 

oran_vaka=Ind/S*1000000
oran_olum=R/S*1000000


plot(oran_vaka)
mGT=generation.time("gamma", c(1,30))
mGT
BS=estimate.R(gunluk_vaka,mGT, begin="2020-03-18", end="2020-11-30", t=tarih, range=c(0.01,100),methods = 'TD')


R0est=BS[["estimates"]][["TD"]][["R"]]
plot(R0est)
mean(R0est)

hist(R0est)
d=density(R0est)
plot(d,xlab="R0 degerleri",main="R0 Yogunluk Grafigi",ylab="Yogunluk")
polygon(d,col="light blue", border="black")
