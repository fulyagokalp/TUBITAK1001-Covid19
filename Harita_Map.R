
# Bu döküman COVID-19 verilerinin dinamik zaman bükme ile zaman serisi analizi ile kümeleyerek, dünya haritası üzerinde kümeleri göstermeye ilişkindir. 

library(tidyverse)
library(readr)
library(maps)

library(dplyr) # data wrangling
library(tidyr) # datawrangling
# analysis
library(dtwclust) # dynamic time warpping
library(depmixS4) # Hidden Markov Model
library(WaveletComp) # Wavelet Analysis
# graphics
library(ggplot2) # grammar of graphics
library(ggdendro) # grammar of dendrograms
library(gtable) # plot organisation
library(grid) # plot organisation
library(gridExtra) # plot organisation
library(colorspace)

library(coronavirus)
update_dataset(silence = FALSE)
data("coronavirus")

cov19 <- coronavirus[coronavirus$country != "", ]

#30.11.2020 son gün olarak al:
cov19 <- cov19[cov19$date < "2021-01-23",] 

# türkiye 26-30 Aralık 2020 için vaka sayılarını hasta sayılarıyla değiştiriyoruz.
cov19[cov19$country == "Turkey" ,]$cases[310:314] <- c(6876,6592,6714,6439,6514)
tail(cov19)

# Her ülke için verilen veileri, gün bazında toplat. Böylece her gün için tek bir vaka, iyileşen ve ölüm sayısı olacaktır.
cov19 <- cov19 %>%
  group_by(date, country, type) %>%
  summarise(cases = sum(cases))

cov19_conf <- cov19[cov19$type == "confirmed", ] #confirmed, recovered, death
cov19_conf$type <- NULL

df <- data.frame(country = cov19_conf$country, value = cov19_conf$cases, 
                 date = cov19_conf$date)
df$value <- log(df$value)
df$value[df$value == "-Inf"] <- 0
df$value[df$value == "NaN"] <- 0 

plan_list <- df %>% 
  tidyr::spread(country, value) %>%
  dplyr::select(-date) %>%
  purrr::map(~(.))

Nclust <- 4
dtw_model <- dtwclust::tsclust(series = plan_list, 
                               type = "h", 
                               k = Nclust,  
                               distance = "dtw_basic", 
                               control = hierarchical_control(method = "complete"),
                               preproc = NULL, 
                               #args = tsclust_args(dist = list(window.size = 5L)),
                               trace = TRUE)

dtw_data <- ggdendro::dendro_data(dtw_model, type="rectangle")
#
labels_order <- dtw_data$labels$label
#
dtw_result <- data.frame(label = names(plan_list), 
                         cluster = factor(stats::cutree(dtw_model, k = Nclust)))
## Dünya haritası
world <- map_data("world")
world[world$region == "USA",]$region <- "US"
world[world$region == "Democratic Republic of the Congo",]$region <- "Congo (Kinshasa)"
world[world$region == "Republic of Congo",]$region <- "Congo (Brazzaville)"

MapData_World = dtw_result %>%
  left_join(. , world, by=c("label"="region"))

#Map
ggplot(MapData_World, aes(fill=cluster)) + 
  geom_map(aes(map_id=label), map = world)+
  expand_limits(x=world$long, y=c(-60,max(world$lat))) +
  labs(title = "Vaka Sayılarına Göre Kümeler") +
  labs( x = "", y = "")+
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette="Paired",name = "Kümeler")+
  #labs(fill = "Kümeler")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())

ggplot(MapData_World, aes(fill=cluster)) + 
  geom_map(aes(map_id=label), map = world)+
  expand_limits(x=world$long, y=c(-60,max(world$lat))) +
  #labs(title = "Clusters of countries by number of cases") +
  labs( x = "", y = "")+
  theme(legend.position = "bottom", legend.text = element_text(size=7))+
  scale_fill_brewer(palette="Paired",name = "Clusters", 
                    labels = c("Medium", "High", "Low", "Very high"))+
  #labs(fill = "Kümeler")+
  #scale_fill_discrete(name = "Kümeler", labels = c("Az yoğun", "Yoğun", "En az yoğun", "Çok yoğun"))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())

#İyileşen

ggplot(MapData_World, aes(fill=cluster)) + 
  geom_map(aes(map_id=label), map = world)+
  expand_limits(x=world$long, y=c(-60,max(world$lat))) +
  #labs(title = "İyileşen Sayılarına Göre Kümeler") +
  labs( x = "", y = "")+
  theme(legend.position = "bottom", legend.text = element_text(size=7))+
  scale_fill_brewer(palette="Paired",name = "Clusters", 
                    labels = c("Medium", "High", "Low", "Very high"))+
  #labs(fill = "Kümeler")+
  #scale_fill_discrete(name = "Kümeler", labels = c("Az yoğun", "Yoğun", "En az yoğun", "Çok yoğun"))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())

#Ölüm

ggplot(MapData_World, aes(fill=cluster)) + 
  geom_map(aes(map_id=label), map = world)+
  expand_limits(x=world$long, y=c(-60,max(world$lat))) +
  #labs(title = "Ölüm Sayılarına Göre Kümeler") +
  labs( x = "", y = "")+
  theme(legend.position = "bottom", legend.text = element_text(size=7))+
  scale_fill_brewer(palette="Paired",name = "Clusters", 
                    labels = c("Low", "Very high", "Medium", "High"))+
  #labs(fill = "Kümeler")+
  #scale_fill_discrete(name = "Kümeler", labels = c("Az yoğun", "Yoğun", "En az yoğun", "Çok yoğun"))+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())

# Boxplot
MapData_World2 <- dtw_result %>%
  left_join(. , df, by=c("label"="country"))

ggplot(MapData_World2, aes(cluster, value)) +
  geom_boxplot(aes(color = cluster,
                   fill = after_scale(desaturate(lighten(color, .6), .6))),
               size = 1) +
  scale_color_brewer(palette="Paired")+
  labs(x = "Clusters", y = "log(death)")+
  theme(legend.position = "none")





