rm(list=ls())
library(Metrics)
library(MASS)
library(mvtnorm)
library(astsa)
library(readxl)
library(foreign)
library(car)
library(quantreg)
library(Hmisc)
library(xlsx)
library(drc)
library(aTSA)
library(tseries)
library(forecast)
library(ggplot2)
library(lmtest)
library(coronavirus)
library(ggpubr)
library(ggpmisc)
library(scales)
library(COVID19)
library(EnvStats)
options("scipen"=100, digits = 2)
Sys.setlocale(category = "LC_ALL", locale = "turkish")

#####Türkiye
data<-covid19(c("Turkey"),verbose = FALSE,level=1,start = "2020-03-18", end = "2020-11-25")
all.data<-covid19(c("Turkey"),verbose = FALSE,level=1,start = "2020-03-18", end = "2020-11-30")


####dunya ülkelerinin verileri##
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
#all.data<-covid19(c("COL"),verbose = FALSE,level=1,start = "2020-03-18", end = "2020-11-28")

#data<-covid19(c("USA"),verbose = FALSE,level=1,start ="2020-03-04", end = "2020-11-25")
#all.data<-covid19(c("USA"),verbose = FALSE,level=1,start = "2020-03-04", end = "2020-11-30")

#data<-covid19(c("CAN"),verbose = FALSE,level=1,start ="2020-03-11", end = "2020-11-25")
#all.data<-covid19(c("CAN"),verbose = FALSE,level=1,start = "2020-03-11", end = "2020-11-30")

#data<-covid19(c("AUS"),verbose = FALSE,level=1,start ="2020-03-09", end = "2020-11-25")
#all.data<-covid19(c("AUS"),verbose = FALSE,level=1,start = "2020-03-09", end = "2020-11-30")

n=dim(data)[1]
tarih=data$date
Sira=as.matrix(seq(1,n),ncol=1)
tarih=data$date
toplam_test=as.vector(data$tests)
gunluk_test=diff(all.data$tests)[-257][-256][-255][-254]
toplam_vaka=data$confirmed
gunluk_vaka=diff(all.data$confirmed)[-257][-256][-255][-254]
toplam_iyilesen=data$recovered
gunluk_iyilesen=diff(all.data$recovered)[-257][-256][-255][-254]
toplam_olum=data$deaths
gunluk_olum=diff(all.data$deaths)[-257][-256][-255][-254]
toplam_nufus=data$population

x=as.matrix(Sira[1:n],nrow=1)
reg.top.vaka=lm(toplam_vaka~as.vector(x))
reg.top.iyilesen=lm(toplam_iyilesen~as.vector(x))
reg.top.olum=lm(toplam_olum~as.vector(x))
reg.gun.vaka=lm(gunluk_vaka~as.vector(x))
reg.gun.iyi=lm(gunluk_iyilesen~as.vector(x))
reg.gun.olum=lm(gunluk_olum~as.vector(x))

summary(reg.top.vaka)
summary(reg.top.iyilesen)
summary(reg.top.olum)

summary(reg.gun.vaka)
summary(reg.gun.iyi)
summary(reg.gun.olum)


datafr1=data.frame(toplam_vaka,tarih)
datafr2=data.frame(toplam_iyilesen,tarih)
datafr3=data.frame(toplam_olum,tarih)
datafr4=data.frame(gunluk_vaka,tarih)
datafr5=data.frame(gunluk_iyilesen,tarih)
datafr6=data.frame(gunluk_olum,tarih)

Legend=c("Veri")

regp1=ggplot(data=datafr1, aes(x = tarih, y = toplam_vaka, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Toplam Vaka")+
  geom_smooth(aes(group = 1, colour = "Regresyon dogrusu"),
              method = "lm", formula = toplam_vaka~x, se = FALSE, linetype = "solid") +
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+

  theme(legend.title=element_blank(),legend.position = c(0.8, 0.2),text = element_text(size=20))
###

regp2=ggplot(data=datafr2, aes(x = tarih, y = toplam_iyilesen, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Toplam Iyilesen")+
  geom_smooth(aes(group = 1, colour = "Regresyon dogrusu"),
              method = "lm", formula = toplam_iyilesen ~ x, se = FALSE, linetype = "solid") +
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  theme(legend.title=element_blank(),legend.position = c(0.8, 0.2),text = element_text(size=20))
###???
regp3=ggplot(data=datafr3, aes(x = tarih, y = toplam_olum, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Toplam Ölüm")+
  geom_smooth(aes(group = 1, colour = "Regresyon dogrusu"),
              method = "lm", formula = toplam_olum ~ x, se = FALSE, linetype = "solid") +
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+

  theme(legend.title=element_blank(),legend.position = c(0.8, 0.2),text = element_text(size=20))
regp1
regp2
regp3
##Tek sayfada Grafikleri düzenleme#
ggarrange( regp1,regp2,regp3+ rremove("x.text"), 
          labels = c("a", "b", "c"),
          ncol = 1, nrow = 3)
####################################
####GUNLUK VERILERIN GRAFIKLERI###
######################################

regp4=ggplot(data=datafr4, aes(x = tarih, y = gunluk_vaka, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Günlük Vaka")+
  geom_smooth(aes(group = 1, colour = "Regresyon dogrusu"),
              method = "lm", formula = gunluk_vaka~x, se = FALSE, linetype = "solid") +
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  
  theme(legend.title=element_blank(),legend.position = c(0.1, 0.95),text = element_text(size=20))
###

regp5=ggplot(data=datafr5, aes(x = tarih, y = gunluk_iyilesen, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Günlük Iyilesen")+
  geom_smooth(aes(group = 1, colour = "Regresyon dogrusu"),
              method = "lm", formula = gunluk_iyilesen ~ x, se = FALSE, linetype = "solid") +
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  theme(legend.title=element_blank(),legend.position = c(0.1, 0.95),text = element_text(size=20))
###???
regp6=ggplot(data=datafr6, aes(x = tarih, y = gunluk_olum, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Günlük Ölüm")+
  geom_smooth(aes(group = 1, colour = "Regresyon dogrusu"),
              method = "lm", formula = gunluk_olum ~ x, se = FALSE, linetype = "solid") +
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  
  theme(legend.title=element_blank(),legend.position = c(0.1, 0.95),text = element_text(size=20))
regp4
regp5
regp6
##Tek sayfada Grafikleri düzenleme#
ggarrange( regp4,regp5,regp6+ rremove("x.text"), 
           labels = c("a", "b", "c"),
           ncol = 1, nrow = 3)

############MODEL YETERLILIK ANALIZI##########
##############################################
res.gun.olum=scale(reg.gun.olum$residuals)
fit.gun.olum=reg.gun.olum$fitted.values
dwtest(reg.gun.olum)

auto.arima(res.gun.olum)


par(mfrow=c(2,2))
acf(res.gun.olum,main="ACF Grafigi",xlab="Gecikme")
pacf(res.gun.olum,main="PACF Grafigi",xlab="Gecikme",ylab="Kismi ACF")

h<-hist(res.gun.olum,main="Artiklarin Histogrami",xlim=c(-4,4),xlab="St. Artiklar",ylab = "Siklik")
xfit <- seq(-3, 3, length = 350) 
par(new=TRUE)
yfit1 <- dnorm(xfit, mean = mean(res.gun.olum), sd = sd(res.gun.olum)) 
yfit1 <- yfit1 * diff(h$mids[1:2]) * n
lines(xfit, yfit1, col = "black",xlab="St. Artiklar", lwd = 3,lty=4,xlim=c(-3,3),main="Artiklarin Histogrami")
par(new=FALSE)
qqnorm(res.gun.olum,xlab = "Teorik Kantiller", ylab ="Örneklem Kantilleri",main = "Normal Dagilim Q-Q Grafigi")
qqline(res.gun.olum)

#.Breush Pagan Test# Degisen varyans testi

lmtest::bptest(reg.gun.olum)  # Breusch-Pagan test

##5 günlük zaman için öngörü heaplari#
#prediction#
predict(reg.top.vaka, newdata = data.frame(x=c(n+1,n+2,n+3,n+4,n+5)), interval = "confidence")

predict(reg.top.iyilesen, newdata = data.frame(x=c(n+1,n+2,n+3,n+4,n+5)), interval = "confidence")

predict(reg.top.olum, newdata = data.frame(x=c(n+1,n+2,n+3,n+4,n+5)), interval = "confidence")

#predict(reg.gun.vaka, newdata = data.frame(x=c(n+1,n+2,n+3,n+4,n+5)), interval = "confidence")

#predict(reg.gun.iyi, newdata = data.frame(x=c(n+1,n+2,n+3,n+4,n+5)), interval = "confidence")

#predict(reg.gun.olum, newdata = data.frame(x=c(n+1,n+2,n+3,n+4,n+5)), interval = "confidence")


###########NONLINEERR###
#Dogrusal Olmayan Regresyon Dose Response#

ryegrass.m1 <- drm(((toplam_vaka)) ~ x, fct = LL.5(fixed = c(NA, NA, NA, NA, NA), names = c("b", "c", "d", "e", "f")))
ryegrass.m2 <- drm(((toplam_iyilesen)) ~ x, fct = LL.5(fixed = c(NA, NA, NA, NA, NA), names = c("b", "c", "d", "e", "f")))
ryegrass.m3 <- drm(((toplam_olum)) ~ x, fct = LL.5(fixed = c(NA, NA, NA, NA, NA), names = c("b", "c", "d", "e", "f")))
ryegrass.m4 <- drm(((gunluk_vaka)) ~ x, fct = LL.5(fixed = c(NA, NA, NA, NA, NA), names = c("b", "c", "d", "e", "f")))
ryegrass.m5 <- drm(((gunluk_iyilesen)) ~ x, fct = LL.5(fixed = c(NA, NA, NA, NA, NA), names = c("b", "c", "d", "e", "f")))
ryegrass.m6 <- drm(((gunluk_olum)) ~ x, fct = LL.5(fixed = c(NA, NA, NA,NA,NA), names = c("b", "c", "d","e","f")))



ryegrass.m1$conc0 <- ryegrass.m1$conc
ryegrass.m1$conc0[ryegrass.m1$conc0 == 0] <- 0.5
newdata <- expand.grid(conc=exp(seq(log(0.5), log(253), length=253)))

pm1=predict(ryegrass.m1, newdata=newdata, interval="confidence")
pm2=predict(ryegrass.m2, newdata=newdata, interval="confidence")
pm3=predict(ryegrass.m3, newdata=newdata, interval="confidence")
pm4=predict(ryegrass.m4, newdata=newdata, interval="confidence")
pm5=predict(ryegrass.m5, newdata=newdata, interval="confidence")
pm6=predict(ryegrass.m6, newdata=newdata, interval="confidence")


newdata$p1=pm1[,1]
newdata$p2=pm2[,1]
newdata$p3=pm3[,1]
newdata$p4=pm4[,1]
newdata$p5=pm5[,1]
newdata$p6=pm6[,1]

datafr1=data.frame(toplam_vaka,tarih)
datafr2=data.frame(toplam_iyilesen,tarih)
datafr3=data.frame(toplam_olum,tarih)
datafr4=data.frame(gunluk_vaka,tarih)
datafr5=data.frame(gunluk_iyilesen,tarih)
datafr6=data.frame(gunluk_olum,tarih)
Legend=c("Veri")

nlin1=ggplot(datafr1, aes(x = x, y = toplam_vaka, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Toplam Vaka")+
  geom_line(data=newdata,aes(x = conc, y = p1,colour = "Model Fit"),lwd=1)+
  
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  
  theme(legend.title=element_blank(),legend.position = c(0.8, 0.2),text = element_text(size=20))
##
nlin2=ggplot(datafr2, aes(x = x, y = toplam_iyilesen, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Toplam Iyilesen")+
  geom_line(data=newdata,aes(x = conc, y = p2,colour = "Model Fit"),lwd=1)+
  
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  
  theme(legend.title=element_blank(),legend.position = c(0.8, 0.2),text = element_text(size=20))
##
nlin3=ggplot(datafr3, aes(x = x, y = toplam_olum, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Toplam Ölüm")+
  geom_line(data=newdata,aes(x = conc, y = p3,colour = "Model Fit"),lwd=1)+
  
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  
  theme(legend.title=element_blank(),legend.position = c(0.8, 0.2),text = element_text(size=20))
ggarrange( nlin1,nlin2,nlin3+ rremove("x.text"), 
           labels = c("a", "b", "c"),
           ncol = 1, nrow = 3)

####Gunluk Vaka grafikleri###

nlin4=ggplot(datafr4, aes(x = x, y = gunluk_vaka, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Günlük Vaka")+
  geom_line(data=newdata,aes(x = conc, y = p4,colour = "Model Fit"),lwd=1)+
  
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  
  theme(legend.title=element_blank(),legend.position = c(0.1, 0.95),text = element_text(size=20))
##
nlin5=ggplot(datafr5, aes(x = x, y = gunluk_iyilesen, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Günlük Iyilesen")+
  geom_line(data=newdata,aes(x = conc, y = p5,colour = "Model Fit"),lwd=1)+
  
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  
  theme(legend.title=element_blank(),legend.position = c(0.1, 0.95),text = element_text(size=20))
##
nlin6=ggplot(datafr6, aes(x = x, y = gunluk_olum, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Günlük Ölüm")+
  geom_line(data=newdata,aes(x = conc, y = p6,colour = "Model Fit"),lwd=1)+
  
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  
  theme(legend.title=element_blank(),legend.position = c(0.1, 0.95),text = element_text(size=20))

ggarrange( nlin4,nlin5,nlin6 + rremove("x.text"), 
           labels = c("a", "b", "c"),
           ncol = 1, nrow = 3)


