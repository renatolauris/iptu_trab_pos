# instala <- c("ggmap","rgeos","sp","maptools")
# install.packages(instala)
library(ggmap)
library(rgeos)
library(sp)
library(maptools)
require(rgdal)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
options("scipen"=100, "digits"=2)


iptu2017 <- rec.2013_2017_real %>% filter(ano==2017) %>% select(codMun7,ufSigla,iptuPc)
iptu2017$codMun7 <- as.character(iptu2017$codMun7)
plot(density(na.omit(log(iptu2017$iptuPc))))

iptu2017_rs <- iptu2017 %>% filter(ufSigla=="RS")

iptuPib2015 <- vars_2013_2017 %>% filter(ano==2015) %>% select(codMun7,ufSigla,iptuPib)
iptuPib2015$codMun7 <- as.character(iptuPib2015$codMun7)

iptuPib2015_rs <- iptuPib2015 %>% filter(ufSigla=="RS")
summary(iptuPib2015)
plot(density(na.omit(log(iptuPib2015$iptuPib))))

# fonte dos shapefiles: https://mapas.ibge.gov.br/bases-e-referenciais/bases-cartograficas/malhas-digitais

# Definir o diretório de trabalho
getwd()
BR <- readOGR("BR", "BRMUE250GC_SIR")

BR.f <- fortify(BR, region = "CD_GEOCMU")
BR.f <- merge(BR.f, BR@data, by.x = "id", by.y = "CD_GEOCMU")
BR.f <- left_join(BR.f,iptu2017,by=c("id"="codMun7"))
BR.f <- left_join(BR.f,iptuPib2015,by=c("id"="codMun7"))

RS <- readOGR("RS", "43MUE250GC_SIR")
RS.f <- fortify(RS, region = "CD_GEOCMU")
RS.f <- merge(RS.f, RS@data, by.x = "id", by.y = "CD_GEOCMU")
RS.f <- left_join(RS.f,iptu2017_rs,by=c("id"="codMun7"))
RS.f <- left_join(RS.f,iptuPib2015_rs,by=c("id"="codMun7"))

# plot(BR)
# head(BR@data)
# head(RS@data)
# n <- nrow(BR@data)
# BR@data$Y <- rnorm(n)
# BR@data$X <- factor(sample(LETTERS[1:4], size = n, replace = T))
# 
# spplot(BR, "X", col.regions = heat.colors(4))

# mapa RS Iptu sobre o PIB
RS.f %>% ggplot(., aes(long, lat, group = group, map_id = id, fill = iptuPib)) +  
  geom_polygon(colour='gray40',size = 0.1)  + 
  theme_void() +
  coord_equal() +
  scale_fill_continuous(name = "% Iptu sobre o PIB (em log)", low = brewer.pal(9, "Greens")[1], high = brewer.pal(9, "Greens")[9],
                        na.value = 'white',trans = "log10") + 
  labs(x = NULL, 
       y = NULL, 
       title = "Heterogeneidade da arrecadação do IPTU", 
       subtitle = "Iptu sobre o PIB (%) no RS, 2015", 
       caption = "Fonte: SICONFI/STN.")

# mapa RS iptuPc
RS.f %>% ggplot(., aes(long, lat, group = group, map_id = id, fill = iptuPc)) +  
  geom_polygon(colour='gray40',size = 0.1)  + 
  theme_void() +
  coord_equal() +
  scale_fill_continuous(name = "Iptu per capita (em log)", low = heat.colors(9)[9], high = heat.colors(9)[1],
                        na.value = 'white',trans = "log10") + 
  labs(x = NULL, 
       y = NULL, 
       title = "Heterogeneidade da arrecadação do IPTU", 
       subtitle = "Iptu per capita no RS, 2017", 
       caption = "Fonte: SICONFI/STN.")

# mapa Brasil Iptu sobre o PIB
BR.f %>% ggplot(., aes(long, lat, group = group, map_id = id, fill = iptuPib)) +  
  geom_polygon(colour='gray40',size = 0.1)  + 
  theme_void() +
  coord_equal() +
  scale_fill_continuous(name = "% Iptu sobre o PIB (em log)", low = brewer.pal(9, "Spectral")[4], high = brewer.pal(9, "Spectral")[7],
                        na.value = 'white',trans = "log10") + 
  labs(x = NULL, 
       y = NULL, 
       title = "Heterogeneidade da arrecadação do IPTU", 
       subtitle = "Iptu sobre o PIB (%) no Brasil, 2015", 
       caption = "Fonte: SICONFI/STN.")

# mapa Brasil Iptu per capita
BR.f %>% ggplot(., aes(long, lat, group = group, map_id = id, fill = iptuPc)) +  
  geom_polygon(colour='gray40',size = 0.1)  + 
  theme_void() +
  coord_equal() +
  scale_fill_continuous(name = "% Iptu per capita (em log)", low = brewer.pal(9, "Spectral")[4], high = brewer.pal(9, "Spectral")[7],
                        na.value = 'white',trans = "log10") + 
  labs(x = NULL, 
       y = NULL, 
       title = "Heterogeneidade da arrecadação do IPTU", 
       subtitle = "Iptu per capita no Brasil, 2017", 
       caption = "Fonte: SICONFI/STN.")


constroi_mapa_log(RS.f,RS.f$iptuPc,"Iptu per capita (em log)",
                 "Heterogeneidade da arrecadação do IPTU",
                 "IPTU per capita no RS, 2017",
                 "Elaborado pelo autor, com base nos dados de IPTU do SICONFI/STN.")



constroi_mapa_log <- function(dadosmapa,indicador,nomleg,tit,subtit,fonte){
  dadosmapa %>% ggplot(., aes(long, lat, group = group, map_id = id, fill = indicador)) +  
    geom_polygon(colour='gray50',size = 0.05)  + 
    theme_void() +
    coord_equal() +
    scale_fill_continuous(name = nomleg, low = heat.colors(9)[9], high = heat.colors(9)[1],
                          na.value = 'white',trans = "log10") + 
    labs(x = NULL, 
         y = NULL, 
         title = tit, 
         subtitle = subtit, 
         caption = fonte)
}
