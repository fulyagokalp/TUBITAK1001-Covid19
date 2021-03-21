rm(list=ls())
library(Metrics)
library(MASS)
library(COVID19)
library(EnvStats)
library(writexl)
library(MASS)
library(npbr)
library(maxLik)
library(npbr)
library(robustbase)
library(tscount) ##tsglm için 
library(coronavirus)
library(ggpubr)
library(ggpmisc)
library(scales)
library(tseries)
library(forecast)
library(ggplot2)
library(dplyr)

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

#???data<-covid19(c("RUS"),verbose = FALSE,level=1,start ="2020-03-19", end = "2020-11-25")
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
gunluk_test=diff(all.data$tests)
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
#reg.gun.vaka=lm(gunluk_vaka~as.vector(x))
#reg.gun.iyi=lm(gunluk_iyilesen~as.vector(x))
#reg.gun.olum=lm(gunluk_olum~as.vector(x))

##Degisecek bölüm##
rezi=reg.top.olum$residuals
y=toplam_olum
beta=c(reg.top.olum$coefficients[1],reg.top.olum$coefficients[2])
#######################
arder=auto.arima(rezi)
sigma=sqrt(sum(rezi^2))
phi=arder$coef[-7:-4]##kontrol edilecek##
rep=1

nu=100

saydur=0
saydurols=0

beta0=matrix(beta,ncol=1)
phi0=matrix(phi,ncol=1)
sigma0=sigma

q=length(phi)
p=length(beta)
et=matrix(rep(0,n),ncol=1)
betahats=beta0
phihats=phi0
sigmahats=sigma0


betahatols=beta0
phihatols=phi0
sigmahatols=sigma0


OLSbetasum=matrix(rep(0,p),ncol=1)
OLSphisum=matrix(rep(0,q),ncol=1)
OLSsigmasum=0

sayacols=matrix(rep(0,p),ncol=1)

BICt=NULL
likt=NULL
syc=NULL

ptm <- proc.time()
  x1=cbind(rep(1,n), x)                              

  ###### AR(P)  PARAMETERS AUTOMATED BUILD ####################
  for(u in 1:q) { 
    name2 <- paste("phi", u, sep = "")
    assign(name2, phi0[u])
  }
  ########################################
  q2=q+1
  saydur=0
  betatfark=matrix(1,p)
  phitfark=matrix(1,q)
  sigmatfark=1

  
  saydur=0
  saydurols=0
  saydur2=0
  betafark=matrix(rep(1,p),ncol=1)
  phifark=matrix(rep(1,q),ncol=1)
  sigmafark=1
  betafarkols=matrix(rep(1,p),ncol=1)
  phifarkols=matrix(rep(1,q),ncol=1)
  sigmafarkols=1
  
  
  betahatols=beta0
  phihatols=phi0
  sigmahatols=sigma0
  betahesols=beta0
  phihesols=phi0
  sighesols=sigma0

  #######################ARP OLS#########################################################
  #####################WHILE Döngü  BASI############################
  while (norm(rbind(betafarkols,phifarkols,sigmafarkols),c("2"))>0.01)
  {
    saydurols=saydurols+1
    
    ###################### PHi  TAHMYNY YÇYN FONKSYYON################
    oldphiols=phihesols
    tphihesap=function(x1,y,betahesols,OLS)                  
    {
      phipay=matrix(rep(0,q),ncol=1)
      phipayda=matrix(rep(0,q^2),ncol=q)
      C=NULL
      D=NULL
      for (t in q2:n)
      {
        ######################PHi OLS iÇiN MATRYS VE VEKTÖR OLU?UMLARI##########
        
        C[t-q]=y[t]-(x1[t,]%*%betahesols)
        for (j in 1:q)
        {
          D[j]=y[t-j]-(x1[t-j,]%*%betahesols)
        }
        phipay=phipay+(C[t-q]*t(t(D)))
        phipayda=phipayda+(t(t(D))%*%D)
      }
      phihesols=ginv(phipayda)%*%phipay
      return(phihesols)
    }##phi fonksiyonu sonu
    phihatols=tphihesap(x1,y,betahesols)   
    phifarkols=oldphiols-phihatols
    
    ####################BETA OLS tahmini için fonksiyon#################
    oldbetaols=betahesols
    tbetahesap=function(x1,y,phihesols)                  
    {
      tn1=matrix(rep(0,p^2),ncol=p)
      tn2=matrix(rep(0,p),ncol=1)
      sn1=matrix(rep(0,p),ncol=1)
      K=matrix(rep(0,n*p),ncol=p)
      L=matrix(rep(0,n),ncol=1)
      A=matrix(rep(0,n*p),ncol=p)
      B=matrix(rep(0,n*p),ncol=p)
      betahatols=matrix(rep(0,p),ncol=1)
      
    
      for (t in q2:n)
      {
        for (v in 1:q)
        {
          K[t-q,]=K[t-q,]+x1[t-v,]*phihesols[v]
          L[t-q]=L[t-q]+y[t-v]*phihesols[v]
        }
        A[t-q,]=x1[t,]-K[t-q,]
        B[t-q,]=A[t-q,]*as.numeric((y[t]-L[t-q]))
        
        tn1=tn1+t(t(as.vector(A[t-q,])))%*%(as.vector(A[t-q,]))
        sn1=sn1+(B[t-q,])
      }
      
      betahesols=ginv(tn1)%*%sn1
      return(betahesols)
    }
    ####### beta fonksiyon sonu
    betahatols=tbetahesap(x1,y,phihesols)  
    betafarkols=oldbetaols-betahatols
    ##SIGMA TAHMINI##########
    oldsigols=sighesols
    tsigmahesap=function(x1,y,betahes,phihes)                                 
    {
      F=matrix(rep(0,n*p),ncol=p)
      G=matrix(rep(0,n),ncol=1)
      A=matrix(rep(0,n),ncol=1)
      D=matrix(rep(0,n),ncol=1)
      C=matrix(rep(0,n),ncol=1)
      
      ##SYGMA YÇYN MATRYS VE VEKTÖR OLU?UMLARI##
      for (t in q2:n)
      {
        for (v in 1:q)
        {
          F[t-q,]=F[t-q,]+(x1[t-v,]*phihesols[v])
          G[t-q]=G[t-q]+(y[t-v]*phihesols[v])
        }
        A[t-q]=(x1[t,]-F[t-q,])%*%betahesols
        D[t-q]=y[t]-G[t-q]
        C[t-q]=(D[t-q]-A[t-q])%*%(D[t-q]-A[t-q])
        sigma=sigma+C[t-q]
      }
      sighesols=sqrt(sigma/(n-q))
      return(sighesols)
      
    }#sigma function sonu
    sigmahatols=tsigmahesap(x1,y,betahesols,phihesols)
    sigfark=oldsigols-sigmahatols
    if (saydurols>1000)
    {
      break
    }
    
    
  } 
  
########################## OLS ESTIMATIONS #################################################
OLSbeta=betahatols
OLSphi=phihatols
OLSsigma=sigmahatols


OLSbeta
OLSphi
OLSsigma
#Prediction#
F=matrix(rep(0,6*p),ncol=p)
AX=matrix(rep(0,6),ncol=1)
xp=c(254,255,256,257,258)#PREDICT



pr=5
yp=NULL

for (i in 1:pr) {
  yp[i]=OLSbeta[1]+OLSbeta[2]*x1[i]
}

yp=matrix(yp,ncol=1)
rownames(yp) <- NULL

  
alarm()
proc.time() - ptm


beta0_1=32211
beta1_1=1514

beta0_2=-4495
beta1_2=1513

beta0_3=493
beta1_3=43

beta0_4=1626.4
beta1_4=2.6

beta0_5=1631.2
beta1_5=1.6

beta0_6=35.4
beta1_6=0.13

pm1=beta0_1+beta1_1*x
pm2=beta0_2+beta1_2*x
pm3=beta0_3+beta1_3*x
pm4=beta0_4+beta1_4*x
pm5=beta0_5+beta1_5*x
pm6=beta0_6+beta1_6*x

newdata <- expand.grid(conc=seq(1,253))

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
datafr5=data.frame(gunluk_iyilesen,seq(1,length(gunluk_iyilesen)))
datafr6=data.frame(gunluk_olum,tarih)
Legend=c("Veri")

ARP_plot1=ggplot(datafr1, aes(x = x, y = toplam_vaka, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Toplam Vaka")+
  geom_line(data=newdata,aes(x = conc, y = p1,colour = "Model Fit"),lwd=1)+
  
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  
  theme(legend.title=element_blank(),legend.position = c(0.8, 0.2),text = element_text(size=20))
##
ARP_plot2=ggplot(datafr2, aes(x = x, y = toplam_iyilesen, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Toplam Iyilesen")+
  geom_line(data=newdata,aes(x = conc, y = p2,colour = "Model Fit"),lwd=1)+
  
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  
  theme(legend.title=element_blank(),legend.position = c(0.8, 0.2),text = element_text(size=20))
##
ARP_plot3=ggplot(datafr3, aes(x = x, y = toplam_olum, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Toplam Ölüm")+
  geom_line(data=newdata,aes(x = conc, y = p3,colour = "Model Fit"),lwd=1)+
  
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  
  theme(legend.title=element_blank(),legend.position = c(0.8, 0.2),text = element_text(size=20))
ggarrange( ARP_plot1,ARP_plot2,ARP_plot3+ rremove("x.text"), 
           labels = c("a", "b", "c"),
           ncol = 1, nrow = 3)
####Gunluk Vaka grafikleri###

ARP_plot4=ggplot(datafr4, aes(x = x, y = gunluk_vaka, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Günlük Vaka")+
  geom_line(data=newdata,aes(x = conc, y = p4,colour = "Model Fit"),lwd=1)+
  
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  
  theme(legend.title=element_blank(),legend.position = c(0.1, 0.95),text = element_text(size=20))
##
ARP_plot5=ggplot(datafr5, aes(x = seq(1,length(gunluk_iyilesen)), y = gunluk_iyilesen, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Günlük Iyilesen")+
  geom_line(data=newdata,aes(x = conc, y = p5,colour = "Model Fit"),lwd=1)+
  
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  
  theme(legend.title=element_blank(),legend.position = c(0.1, 0.95),text = element_text(size=20))
##
ARP_plot6=ggplot(datafr6, aes(x = x, y = gunluk_olum, colour = Legend)) + 
  geom_point(size = 3) + labs(x="Tarih", y="Günlük Ölüm")+
  geom_line(data=newdata,aes(x = conc, y = p6,colour = "Model Fit"),lwd=1)+
  
  scale_colour_manual(values = c("red","black"),
                      guide = guide_legend(override.aes = list(
                        linetype = c("solid","blank"),
                        shape = c(NA,16))))+
  
  theme(legend.title=element_blank(),legend.position = c(0.1, 0.95),text = element_text(size=20))

ggarrange( ARP_plot4,ARP_plot5,ARP_plot6 + rremove("x.text"), 
           labels = c("a", "b", "c"),
           ncol = 1, nrow = 3)


