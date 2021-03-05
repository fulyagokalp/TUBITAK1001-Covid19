rm(list=ls())
library(COVID19)
library(robustbase)
options("scipen"=100, digits = 4)

#China_gunluk_iyilesen<-matrix(c(28,2,6,3,10,9,43,19,15,79,61,188,151,229,272,362,522,597,623,699,718,446,1135,1760,1321,1457,1707,1744,1756,2052,690,3995,488,1828,2661,2408,2846,3399,2991,2842,2692,2596,2551,2291,1652,1595,1849,1416,1377,1463,1257,1295,1464,1357,893,888,957,780,731,591,505,452,466,493,408,539,380,482,341,283,199,160,195,186,261,103,100,157,112,112,86,79,83,161,111,90,-849,62,76,55,54,62,122,126,66,102,97,48,52,49,50,13,98,108,78,59,48,16,134,40,31,24,24,15,20,12,13,4,0,0,0,22,3,8,9,6,9,4,11,4,3,9,2,4,11,5,4,10,10,7,5,8,8,3,7,7,4,17,5,0,19,1,12,8,7,10,8,11,18,10,13,18,15,15,26,12,7,29,48,0,0,74,31,0,60,38,33,26,28,45,25,42,40,53,44,68,50,7,51,77,86,107,120,111,95,121,183,175,196,181,156,122,193,193,171,152,152,92,83,125,158,97,130,131,64,70,110,83,84,84,65,62,48,58,43,42,45,55,36,27,32,25,48,20,29,32,15,28,26,26,23,22,13,18,18,29,19,20,17,21,17,25,37,19,15,17,19,10,26,22,24,29,17,11,21,17,13,23,19,29,28,21,33,26,30,21,24,28,16,13,18,33,29,24,27,27,23,29,23,37,43,25,24,28,30,37,30,25,29,44,51,33,27,24,18,23,24,31,41),ncol=1)
#toplam_iyilesen=matrix(rep(0,309),ncol=1)
#toplam_iyilesen[1]=China_gunluk_iyilesen[1]
#for (i in 2:309) {
#  toplam_iyilesen[i] <- toplam_iyilesen[i-1]+China_gunluk_iyilesen[i]
#}


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
##Güney Afrika için 
#all.data[255:259,5]=c(716444,722876,723347,730633,731242)
#all.data[255:259,6]=c(21289,21378,21439,21477,21535)


#data<-covid19(c("CHN"),verbose = FALSE,level=1,start ="2020-01-22", end = "2020-11-25")
#all.data<-covid19(c("CHN"),verbose = FALSE,level=1,start = "2020-01-22", end = "2020-11-30")
#toplam_iyilesen1=as.matrix(c(28,30,36,39,49,58,101,120,135,214,275,463,614,843,1115,1477,1999,2596,3219,3918,4636,5082,6217,7977,9298,10755,12462,14206,15962,18014,18704,22699,23187,25015,27676,30084,32930,36329,39320,42162,44854,47450,50001,52292,53944,55539,57388,58804,60181,61644,62901,64196,65660,67017,67910,68798,69755,70535,71266,71857,72362,72814,73280,73773,74181,74720,75100,75582,75923,76206,76405,76565,76760,76946,77207,77310,77410,77567,77679,77791,77877,77956,78039,78200,78311,78401,77552,77614,77690,77745,77799,77861,77983,78109,78175,78277,78374,78422,78474,78523,78573,78586,78684,78792,78870,78929,78977,78993,79127,79167,79198,79222,79246,79261,79281,79293,79306,79310,79310,79310,79310,79332,79335,79343,79352,79358,79367,79371,79382,79386,79389,79398,79400,79404,79415,79420,79424,79434,79444,79451,79456,79464,79472,79475,79482,79489,79493,79510,79515,79515,79534,79535,79547,79555,79562,79572,79580,79591,79609,79619,79632,79650,79665,79680,79706,79718,79725,79754,79802,79802,79802,79876,79907,79907,79967,80005,80038,80064,80092,80137,80162,80204,80244,80297,80341,80409,80459,80466,80517,80594,80680,80787,80907,81018,81113,81234,81417,81592,81788,81969,82125,82247,82440,82633,82804,82956,83108,83200,83283,83408,83566,83663,83793,83924,83988,84058,84168,84251,84335,84419,84484,84546,84594,84652,84695,84737,84782,84837,84873,84900,84932,84957,85005,85025,85054,85086,85101,85129,85155,85181,85204,85226,85239,85257,85275,85304,85323,85343,85360,85381,85398,85423,85460,85479,85494,85511,85530,85540,85566,85588,85612,85641,85658,85669,85690,85707,85720,85743,85762,85791,85819,85840,85873,85899,85929,85950,85974,86002,86018,86031,86049,86082,86111,86135,86162,86189,86212,86241,86264,86301,86344,86369,86393,86421,86451,86488,86518,86543,86572,86616,86667,86700,86727,86751,86769,86792,86816,86847,86888,86900,86937,86968,86998,87017),ncol=1)
#all.data[1:314,5]=toplam_iyilesen1
#data[1:309,5]=toplam_iyilesen1[1:309]


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



name=c("Turkey")

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
toplam_iyilesen[9]=30
toplam_iyilesen[10]=34




##logaritmik ölçekte günlük vaka, günlük iyileþen ve günlük ölüm sayýsý
loggv=as.matrix(log(gunluk_vaka))
loggv[is.infinite(loggv)] <- NA

loggi=as.matrix(log(gunluk_iyilesen))
loggi[is.infinite(loggi)] <- NA

loggö=as.matrix(log(gunluk_olum))
loggö[is.infinite(loggö)] <- NA

##logaritmik ölçekte toplam vaka, toplam iyileþen ve toplam ölüm sayýsý
logtv=as.matrix(log(toplam_vaka))
logtv[is.infinite(logtv)] <- NA

logti=as.matrix(log(toplam_iyilesen))
logti[is.infinite(logti)] <- NA

logto=as.matrix(log(toplam_olum))
logto[is.infinite(logto)] <- NA

##Milyon baþýna günlük vaka, günlük ölüm
xmv=as.matrix(gunluk_vaka*1000000/as.numeric(data[1,10]))
xmö=as.matrix(gunluk_olum*1000000/as.numeric(data[1,10]))

##Toplam ölüm/toplam vaka
xöv=as.matrix(toplam_olum/toplam_vaka)

### Power Law Grafikleri 1 
par(mfrow=c(3,1))
plot(toplam_vaka[1:n-1],toplam_vaka[2:n],xlab="C(n)",ylab = "C(n+1)",col="deepskyblue4",pch=16)
lines(toplam_vaka[1:n-1],toplam_vaka[2:n],col="red")
legend("topleft",c("Toplam Vaka"))

plot(toplam_iyilesen[1:n-1],toplam_iyilesen[2:n],xlab="R(n)",ylab = "R(n+1)",col="deepskyblue4",pch=16)
lines(toplam_iyilesen[1:n-1],toplam_iyilesen[2:n],col="red")
legend("topleft",c("Toplam Ýyileþen"))

plot(toplam_olum[1:n-1],toplam_olum[2:n],xlab="D(n)",ylab = "D(n+1)",col="deepskyblue4",pch=16)
lines(toplam_olum[1:n-1],toplam_olum[2:n],col="red")
legend("topleft",c("Toplam Ölüm"))


##ML ve Robust Parametre Tahminleri: (ML için "lm", robust için "rlm" komutu kullanýldý)
s1=summary(lm(logtv[2:n]~logtv[1:n-1]),converge=FALSE)  #"lm" komutu yerine "rlm" yazýlarak dayanýklý parametre tahminleri elde edilir.
s2=summary(lm(logti[2:n]~logti[1:n-1]),converge=FALSE)
s3=summary(lm(logto[2:n]~logto[1:n-1]),converge=FALSE)
alpha1=exp(s1$coef[1])
beta1=s1$coef[2]
alpha2=exp(s2$coef[1])
beta2=s2$coef[2]
alpha3=exp(s3$coef[1])
beta3=s3$coef[2]

alpha1
beta1
alpha2
beta2
alpha3
beta3

#######power-law grafikler 2
n2=n-1  #Grafik için tanýmlanan deðer
np=5  #########prediction sayýsý
Cnfit=toplam_vaka
for (i in 2:n) {
  Cnfit[i] <- round(alpha1*((toplam_vaka[i-1])^beta1),0)  #Tahmin
}

Cnpredict=as.matrix(rep(0,np),ncol=1)
for (i in 1:np) {
  cn=all.data[n-1+i,4]
  Cnpredict[i] <- round(alpha1*(cn^beta1),0)
}
p1=round(alpha1^(1/(1-beta1)),0)     #####limit pn deðeri
xcmin=min(Cnfit,toplam_vaka,as.numeric(Cnpredict))
xcmax=max(Cnfit,toplam_vaka,as.numeric(Cnpredict))

par(mfrow=c(3,1))

plot(toplam_vaka[1:n2],toplam_vaka[2:n],xlim=c(xcmin,xcmax),ylim=c(xcmin,xcmax),xlab="C(n)",ylab = "C(n+1)",col="deepskyblue4",pch=16)
par(new=TRUE)
plot(Cnfit[1:n2],Cnfit[2:n],xlim=c(xcmin,xcmax),ylim=c(xcmin,xcmax),xlab="C(n)",ylab = "C(n+1)",col="green",pch=17)
par(new=TRUE)
plot(Cnpredict[1:np-1],Cnpredict[2:np],xlim=c(xcmin,xcmax),ylim=c(xcmin,xcmax),xlab="C(n)",ylab = "C(n+1)",col="red",pch=8)
legend("bottomright",c("Vaka","Fit","Öngörü"), col=c("deepskyblue4","green","red"),pch=c(16,17,8))

par(new=FALSE)


toplam_iyilesen2=toplam_iyilesen[14:n]
Rnfit=toplam_iyilesen2
nR=length(Rnfit)
for (i in 2:nR) {
  Rnfit[i] <- round(alpha2*((toplam_iyilesen2[i-1])^beta2),0)
}
Rnpredict=as.matrix(rep(0,np),ncol=1)
Rnpredict[1]=toplam_iyilesen[n]
for (i in 2:np) {
  rn=all.data[n-1+i,5]
  Rnpredict[i] <- round(alpha2*(rn^beta2),0)
}
p2=round(alpha2^(1/(1-beta2)),0)     #####limit pn deðeri

xrmin=min(Rnfit,toplam_iyilesen2,as.numeric(Rnpredict))
xrmax=max(Rnfit,toplam_iyilesen2,as.numeric(Rnpredict))

plot(toplam_iyilesen2[1:n2],toplam_iyilesen2[2:n],xlim=c(xrmin,xrmax),ylim=c(xrmin,xrmax),xlab="R(n)",ylab = "R(n+1)",col="deepskyblue4",pch=16)
par(new=TRUE)
plot(Rnfit[1:nR-1],Rnfit[2:nR],xlim=c(xrmin,xrmax),ylim=c(xrmin,xrmax),xlab="R(n)",ylab = "R(n+1)",col="green",pch=17)
par(new=TRUE)
plot(Rnpredict[1:np-1],Rnpredict[2:np],xlim=c(xrmin,xrmax),ylim=c(xrmin,xrmax),xlab="R(n)",ylab = "R(n+1)",col="red",pch=8)
legend("bottomright",c("Ýyileþen","Fit","Öngörü"), col=c("deepskyblue4","green","red"),pch=c(16,17,8))


par(new=FALSE)

toplam_olum2=toplam_olum[5:n]
Dnfit=toplam_olum2
nD=length(Dnfit)
for (i in 2:nD) {
  Dnfit[i] <- round(alpha3*((toplam_olum2[i-1])^beta3),0)
}
Dnpredict=as.matrix(rep(0,np),ncol=1)
Dnpredict[1]=toplam_olum[n]
for (i in 2:np) {
  dn=all.data[n-1+i,6]
  Dnpredict[i] <- round(alpha3*(dn^beta3),0)
}

p3=round(alpha3^(1/(1-beta3)),0)     #####limit pn deðeri
xdmin=min(Dnfit,toplam_olum2,as.numeric(Dnpredict))
xdmax=max(Dnfit,toplam_olum2,as.numeric(Dnpredict))

plot(toplam_olum2[1:nD-1],toplam_olum2[2:nD],xlim=c(xdmin,xdmax),ylim=c(xdmin,xdmax),xlab="D(n)",ylab = "D(n+1)",col="deepskyblue4",pch=16)
par(new=TRUE)
plot(Dnfit[1:nD-1],Dnfit[2:nD],xlim=c(xdmin,xdmax),ylim=c(xdmin,xdmax),xlab="D(n)",ylab = "D(n+1)",col="green",pch=17)
par(new=TRUE)
plot(Dnpredict[1:np-1],Dnpredict[2:np],xlim=c(xdmin,xdmax),ylim=c(xdmin,xdmax),xlab="D(n)",ylab = "D(n+1)",col="red",pch=8)
legend("bottomright",c("Ölüm","Fit","Öngörü"), col=c("deepskyblue4","green","red"),pch=c(16,17,8))

a1=as.matrix(cbind(as.numeric(Cnpredict),as.numeric(Rnpredict),as.numeric(Dnpredict)),ncol=1)

########### 1 fark bitti






