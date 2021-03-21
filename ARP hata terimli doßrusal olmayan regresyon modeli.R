rm(list=ls())
library(Metrics)
library(MASS)
library(COVID19)
library(EnvStats)
library(writexl)
library(MASS)
library(npbr)
library(maxLik)
library(robustbase)
library(tscount) ##tsglm için 
library(coronavirus)
library(ggpubr)
library(ggpmisc)
library(scales)
library(tseries)
library(forecast)
library(ggplot2)
library(drc)
options("scipen"=100, digits = 2)
Sys.setlocale(category = "LC_ALL", locale = "turkish")
#####Türkiye
data<-covid19(c("Turkey"),verbose = FALSE,level=1,start = "2020-03-18", end = "2020-11-25")
#all.data<-covid19(c("Turkey"),verbose = FALSE,level=1,start = "2020-03-18", end = "2020-11-30")
#all.data[254:258,4]=as.matrix(c(474606, 481198,487912,494351,500865),ncol=1)
#all.data[254:258,5]=as.matrix(c(388771,392616,396227,400242,404727),ncol=1)
#all.data[254:258,6]=as.matrix(c(13014, 13191,13373,	13558,13746),ncol=1)

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

#data<-covid19(c("RUS"),verbose = FALSE,level=1,start ="2020-03-19", end = "2020-11-25")
#all.data<-covid19(c("RUS"),verbose = FALSE,level=1,start = "2020-03-16", end = "2020-11-30")

#data<-covid19(c("Germany"),verbose = FALSE,level=1,start ="2020-02-28", end = "2020-11-25")
#all.data<-covid19(c("Germany"),verbose = FALSE,level=1,start = "2020-02-28", end = "2020-11-30")

#data<-covid19(c("FRA"),verbose = FALSE,level=1,start ="2020-02-29", end = "2020-11-25")
#all.data<-covid19(c("FRA"),verbose = FALSE,level=1,start = "2020-02-29", end = "2020-11-30")

#data<-covid19(c("ESP"),verbose = FALSE,level=1,start ="2020-03-01", end = "2020-11-25")
#all.data<-covid19(c("ESP"),verbose = FALSE,level=1,start = "2020-03-01", end = "2020-11-30")

#data<-covid19(c("ITA"),verbose = FALSE,level=1,start ="2020-02-24", end = "2020-11-25")
#all.data<-covid19(c("ITA"),verbose = FALSE,level=1,start = "2020-02-24", end = "2020-11-30")

#data<-covid19(c("BRA"),verbose = FALSE,level=1,start ="2020-03-18", end = "2020-11-25")
#all.data<-covid19(c("BRA"),verbose = FALSE,level=1,start = "2020-03-12", end = "2020-11-30")

#data<-covid19(c("COL"),verbose = FALSE,level=1,start ="2020-03-23", end = "2020-11-25")
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
#gunluk_test=diff(all.data$tests)
toplam_vaka=data$confirmed
#gunluk_vaka=diff(all.data$confirmed)
toplam_iyilesen=data$recovered
#gunluk_iyilesen=diff(all.data$recovered)
toplam_olum=data$deaths
#gunluk_olum=diff(all.data$deaths)
toplam_nufus=data$population

xrank=as.matrix(Sira[1:n],nrow=1)
reg.top.vaka=lm(toplam_vaka~as.vector(xrank))
reg.top.iyilesen=lm(toplam_iyilesen~as.vector(xrank))
reg.top.olum=lm(toplam_olum~as.vector(xrank))
#reg.gun.vaka=lm(gunluk_vaka~as.vector(x))
#reg.gun.iyi=lm(gunluk_iyilesen~as.vector(x))
#reg.gun.olum=lm(gunluk_olum~as.vector(x))



###########HER BIR VERI ICIN YENIDEN DEGISECEK KISIM####
rezi=reg.top.vaka$residuals
y=toplam_vaka
beta=c(reg.top.vaka$coefficients[1],reg.top.vaka$coefficients[2])

#######################
arder=auto.arima(rezi)
sigma=sqrt(sum(rezi^2))
phi=arder$coef[-5:-2]##kontrol edilecek##

betahatols=beta
phihatols=phi
sigmahatols=sigma
betahesols=beta
phihesols=phi
sigols=sigma


ryegrass.m1 <- drm(y ~ xrank, fct = LL.5(fixed = c(1, 1, NA, NA, NA), names = c("b", "c", "d", "e", "f")))

par.vec=ryegrass.m1$fit$par
bm=1
cm=1
dm=par.vec[1]
em=par.vec[2]
fm=par.vec[3]

x1=as.vector(xrank)
y=as.vector((toplam_vaka))
mxt=cm+(dm-cm)/(1+exp((bm*(log(x1)-log(em)))))^fm

n=length(y)
x<-mxt
sig=sigols

p=length(beta)
q=length(phi)
q2=q+1
et=matrix(rep(0,n),ncol=1)

phihat=phi
sigmahats=sigma

betafarkols=matrix(rep(1,p),ncol=1)
phifarkols=matrix(rep(1,q),ncol=1)
sigmafarkols=1

phihes=phi
sighes=sigma
saydurols=0

#####################WHILE Döngü  BASI############################

while (norm(rbind(phifarkols,sigmafarkols),c("2"))>0.01)
{
 
  saydurols=saydurols+1

  ##SIGMA TAHMINI##########
  oldsig=sighes
  tsigmahesap=function(x,y,phihes)                                 
  {
    F=matrix(rep(0,n),ncol=1)
    G=matrix(rep(0,n),ncol=1)
    A=matrix(rep(0,n),ncol=1)
    D=matrix(rep(0,n),ncol=1)
    C=matrix(rep(0,n),ncol=1)
    
  ##SYGMA YÇYN MATRYS VE VEKTÖR OLU?UMLARI##
  for (t in q2:n)
  {
    for (v in 1:q)
    {
      F[t-q,]=F[t-q,]+(x[t-v]*phihes[v])
      G[t-q]=G[t-q]+(y[t-v]*phihes[v])
    }
    A[t-q]=(x[t]-F[t-q])
    D[t-q]=y[t]-G[t-q]
    C[t-q]=(D[t-q]-A[t-q])%*%(D[t-q]-A[t-q])
    sigma=sigma+C[t-q]
  }
  sighes=sqrt(sigma/(n-q))
  return(sighes)
  
}#sigma function sonu


  sighes=as.numeric(tsigmahesap(x,y,phihes))  
  sigfark=oldsig-sighes

  
  ###########2- PHY LQ TAHMYNY Y??YN FONKSYYON########
  
  
  ###################### PHi  TAHMYNY YÇYN FONKSYYON################
 oldphi=phihat
  tphihesap=function(x,y)                  
  {
    phipay=matrix(rep(0,q),ncol=1)
    phipayda=matrix(rep(0,q^2),ncol=q)
    C=NULL
    D=NULL
    for (t in q2:n)
    {
      ######################PHi OLS iÇiN MATRYS VE VEKTÖR OLU?UMLARI##########
      
      C[t-q]=y[t]-(x[t])
      for (j in 1:q)
      {
        D[j]=y[t-j]-(x[t-j])
      }
      phipay=phipay+(C[t-q]*t(t(D)))
      phipayda=phipayda+(t(t(D))%*%D)
    }
    phihat=ginv(phipayda)%*%phipay
    return(phihat)
  }##phi fonksiyonu sonu

  phihat=tphihesap(x,y)  
  phifark=oldphi-phihat
  
  

  phihes=phihat
  A=matrix(rep(0,n),ncol=1)
  D=matrix(rep(0,n),ncol=1)
  F=matrix(rep(0,n*p),ncol=p)
  G=matrix(rep(0,n),ncol=1)
  for (t in q2:n)
  {
    for (v in 1:q)
    {
      F[t]=F[t]+(x[t-v]*phihes[v])
      G[t]=G[t]+(y[t-v]*phihes[v])
    }
    A[t]=(x[t]-F[t])
    D[t]=y[t]-G[t]
    
  }
  ryegrass.m2 <- drm(y ~ xrank, fct = LL.5(fixed = c(NA, NA, NA, NA, NA), names = c("b", "c", "d", "e", "f")))
  par.vec=ryegrass.m1$fit$par
  bm=par.vec[1]
  cm=par.vec[2]
  dm=par.vec[3]
  em=par.vec[4]
  fm=par.vec[5]
  mxt=cm+(dm-cm)/(1+exp((bm*(log(x1)-log(em)))))^fm
  
  
  if (saydurols>50)
  {
    break
  }
}##WHILE SONU
pred=suppressWarnings(predict(ryegrass.m2, newdata = data.frame(x=c(n+1,n+2,n+3,n+4,n+5),CURVE=c("1","2","3","4","5")),interval = "confidence"))

matrix(pred[,1],ncol=1)

##Grafikler


ryegrass.m2$conc0 <- ryegrass.m1$conc
ryegrass.m2$conc0[ryegrass.m1$conc0 == 0] <- 0.5
newdata <- expand.grid(conc=exp(seq(log(0.5), log(253), length=253)))

pm1=predict(ryegrass.m2, newdata=newdata, interval="confidence")



newdata$p1=pm1[,1]


datafr1=data.frame(toplam_vaka,tarih)
datafr2=data.frame(toplam_iyilesen,tarih)
datafr3=data.frame(toplam_olum,tarih)

Legend=c("Veri")
x=seq(1:n)

nlin1=ggplot(datafr1, aes(x = x, y = toplam_vaka, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Toplam Vaka")+
  geom_line(data=newdata,aes(x = conc, y = p1,colour = "Model Fit"),lwd=1)+
  
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  
  theme(legend.title=element_blank(),legend.position = c(0.8, 0.2),text = element_text(size=20))
nlin1