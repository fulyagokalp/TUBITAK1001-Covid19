
# Bu döküman COVID-19 verilerinin loess fonksiyonu ile grafiklerini içermektedir. 
# This document contains graphs of COVID-19 data with loess function.

library(COVID19) 
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)
library(scales)
options(scipen = 999)

library(coronavirus)
update_dataset(silence = FALSE)
data("coronavirus")

cov19 <- coronavirus[coronavirus$country != "", ]

# 30.11.2020 son gün olarak al:
# The last day is 30.11.2020:

cov19 <- cov19[cov19$date < "2020-12-01",] 
tail(cov19)
# Türkiye 26-30 Aralık 2020 için vaka sayılarını hasta sayılarıyla değiştiriyoruz.
# We're changing the number of cases with the number of patients for 26 to 30 December 2020 for Turkey.

cov19[cov19$country == "Turkey" ,]$cases[310:314] <- c(6876,6592,6714,6439,6514)

# Her ülke için verilen veileri, gün bazında toplat. Böylece her gün için tek bir vaka, iyileşen ve ölüm sayısı olacaktır.
# Aggregate the data given for each country on a daily basis. Thus, for each day there will be a single case, the number of recovery and deaths.

cov19 <- cov19 %>%
  group_by(date, country, type) %>%
  summarise(cases = sum(cases))

# Vakanın 100'den büyük olduğu günden başlat. türkiye için farklı ülkeler yazılarak, analizler tekrarlanabilir.
# Starting from the day the case is greater than 100. 
# The analysis can be repeated replacing different countries with Turkey 
Ulke <- "Turkey"
Tr <- cov19[cov19$country  == Ulke,] #Turkey, South Africa, China, "Korea, South", India, Iran,Japan, Russia, Germany, France, Spain, Italy, Brazil, Colombia,  US, Canada, Australia
Tr <- Tr %>% spread(type, cases, fill = NA, convert = FALSE) #Geniş formata çevir
Tr <- Tr[which(Tr$confirmed > 100)[1]:length(Tr$confirmed),] #Vakanın 100'dan büyük olduğu günden başlat
Tr <- Tr %>% gather(type, cases, confirmed, death, recovered) #Uzun formata tekrar çevir

Tr[Tr$type == "confirmed", ]$type = "vaka"
Tr[Tr$type == "recovered", ]$type = "iyileşen"
Tr[Tr$type == "death", ]$type = "ölüm"

# Prediction for Loess - with log transformation *****************************
Tr_vaka <- Tr[Tr$type == "vaka",] #vaka,iyileşen,ölüm için çalıştırılabilir
Tr_vaka$log_cases <- log(Tr_vaka$cases) 
Tr_vaka$log_cases[Tr_vaka$log_cases == "-Inf"] <- 0
Tr_vaka$log_cases[Tr_vaka$log_cases == "NaN"] <- 0 

dim_train <- dim(Tr_vaka)[1]-5  # son 5 gün için prediction yapılacaktır.
Tr_train <- Tr_vaka[1:dim_train,]
Tr_test <- Tr_vaka[(dim_train+1):dim(Tr_vaka)[1],]

# Train veri ile modeli çalıştır

Tr_Loess <- loess(log_cases ~ as.numeric(date), Tr_train, 
                  control = loess.control(surface = "direct"))

# Çalıştırılan model ile test verisinden predicion yap

predict_test <- predict(Tr_Loess, as.numeric(Tr_test$date), se = FALSE)

# Yeni bir pred verisi yarat, test verisini klonlayarak

Tr_pred <- Tr_test

# Pred verisinin log_cases değişkenini tahmin değerleri ile değiştir.

Tr_pred$log_cases <- predict_test

# Gerçek veri ile tahmin verisi row bind et

df3 <- Tr_vaka %>%
  mutate(Type = 'veri') %>%
  bind_rows(Tr_pred %>%
              mutate(Type = 'tahmin'))

# Karesel artıkların toplamı 
KAT_Tr_Vaka <- sum(Tr_pred$log_cases - Tr_test$log_cases)^2
KAT_Tr_Vaka

# Loess fonksiyonuyla birlikte, tahmin değerlerini de çizdir.

p <- ggplot(df3, aes(date, log_cases, color = Type)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = loess) +
  ylab("log(vaka)") +
  xlab("Tarih") +
  labs(title = "Türkiye, Günlük Vaka Sayıları - Loess Fonksiyonu") +
  labs(caption = "Günlük vaka sayısının 100'ü geçtiği günden başlatılmıştır. 
       \n KAT = 3.23") +
  theme(legend.title=element_blank()) + 
  scale_x_date(breaks=date_breaks("1 months"), labels=date_format("%m-%y")) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
p
ggplotly(p)
#############################################################################

