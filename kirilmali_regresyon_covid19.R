rm(list=ls())
library(COVID19)
library(strucchange)
library(segmented)
options("scipen"=100, digits = 4)

data<-covid19(c("Turkey"),verbose = FALSE,level=1,start = "2020-03-18", end = "2020-11-25")  #Di�er �lkeler i�in de hesaplamalar yap�labilir.
all.data<-covid19(c("Turkey"),verbose = FALSE,level=1,start = "2020-03-18", end = "2020-11-30")

##A�a��daki 3 sat�r T�rkiye verisine �zg� olarak girildi.#####################
##Pakette vaka say�s� verildi�i ancak 25 Kas�m 2020 sonras�nda verilen say�lar�n hasta say�s� oldu�u bildirildi�i ve hasta say�s� ile �al��t���m�z i�in bu k�s�m manuel girildi. 
all.data[255:259,4]=as.matrix(c(474606, 481198,487912,494351,500865),ncol=1) 
all.data[255:259,5]=as.matrix(c(388771,392616,396227,400242,404727),ncol=1)
all.data[255:259,6]=as.matrix(c(13014, 13191,13373,	13558,13746),ncol=1)
##############################################################################


n=dim(data)[1]
tarih=data$date
Sira=as.matrix(seq(1,n),ncol=1)
pop=as.numeric(all.data[1,11])

###Toplam vaka, toplam iyile�en ve toplam �l�m  say�s�#####
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
gunluk_iyilesen[7]=4
gunluk_iyilesen[8]=4
toplam_iyilesen[9]=30
toplam_iyilesen[10]=34

aktif_vaka=toplam_vaka- toplam_olum - toplam_iyilesen

##Analiz edilen de�i�ken
y=aktif_vaka      ###gunluk_iyilesen ve gunluk_olum say�lar� i�in benzer analiz yap�labilir
n1=length(y)
x=Sira[1:n1]
datam=data.frame(x,y)

olm<-lm(y~1+x)
br=breakpoints(y~1+x)

os<-segmented(olm, seg.Z= ~ x, psi=list(x=c(br$breakpoints)))
ylim1=c(min(y,predict.segmented(os)),max(y,predict.segmented(os)))


#Grafikler
d=as.Date(tarih)
plot(as.numeric(y), xaxt='n',ylim=ylim1,col="deepskyblue4",pch=20,xlab="",ylab="")
axis.Date(1, at = seq(d[1], d[length(d)], by="month"),
          labels= seq(d[1], d[length(d)], by="month"),
          format="%d-%m-%Y", las = 2)
par(new=TRUE)
plot(os,axes="",xaxt='n', ann=FALSE,ylim=ylim1,col="red",ylab="",main="",xlab="",lwd=2,labels=FALSE,)
par(new=TRUE)
lines.segmented(os,xaxt='n',col=4,pch=19,bottom=FALSE,lwd=2) #for the CI for the breakpoint
points.segmented(os,xaxt='n',col=4, link=FALSE,pch=17)
legend("bottomright",c("Aktif Vaka ","K�r�lma Noktas�","K�r�lma Noktas� ve GA","Regresyon Do�rultusu"), col=c("deepskyblue4",4,4,"red"),lty=c(NA,NA,NA,1), pch=c(16,17,16,NA))

##��kt�lar
summary(os)
print(os)
slope(os,apc=TRUE)
sonuc=t(as.matrix(aapc(os, exp.it=FALSE)*100,ncol=4))




