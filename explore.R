library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(rebus)

zt <- function(v,na.rm=TRUE) {
  return((v - mean(v,na.rm=na.rm))/sd(v,na.rm=na.rm))
}

gt <- fread("./data/GlobalTemperatures.csv") %>% 
  mutate(
    dt=ymd(dt),
    year=year(dt),
    LandAverageTemperature_zt = zt(LandAverageTemperature),
    LandAndOceanAverageTemperature_zt = zt(LandAndOceanAverageTemperature)
  )

p <- ggplot(gt,aes(x=dt,y=LandAverageTemperature_zt))+geom_smooth(method='loess')
print(p)

co2 <- fread("./data/API_EN.ATM.CO2E.KT_DS2_en_csv_v2.csv") %>% 
  filter(`Country Name`=='World') %>%
  select(matches("[0-9][0-9][0-9][0-9]")) %>%
  gather("year","kt_co2") %>% 
  mutate(
    year=as.numeric(year),
    kt_co2=as.numeric(kt_co2),
    kt_co2_zt=zt(kt_co2)
  )

p2 <- ggplot(co2,aes(x=year,y=kt_co2_zt))+geom_smooth(method='loess')
print(p2)

co2ppm <- fread("./data/archive.csv") %>%
  mutate(
    co2ppm_zt = zt(`Carbon Dioxide (ppm)`),
    year=Year            
  ) %>%
  select(year,co2ppm_zt) %>%
  gather("year",co2ppm_zt) %>% 
  group_by(year) %>%  
  summarise(co2ppm_zt_ya = mean(co2ppm_zt)) %>%
  filter(!is.na(co2ppm_zt_ya)) %>%
  select(year,co2ppm_zt_ya)


co2small <- co2 %>% 
  select(year,kt_co2_zt) %>%
  filter(!is.na(kt_co2_zt))

gtsmall <- gt %>%
  select(
    year,
    LandAverageTemperature_zt,
    LandAndOceanAverageTemperature_zt
  ) %>%
  group_by(year) %>%
  summarise(
    LandAverageTemperature_zt_ya = mean(LandAverageTemperature_zt),
    LandAndOceanAverageTemperature_zt_ya = mean(LandAndOceanAverageTemperature_zt)
  ) %>%
  filter(
    !is.na(LandAverageTemperature_zt_ya) & 
    !is.na(LandAndOceanAverageTemperature_zt_ya)
  ) 

wt_co2 <- co2small %>% 
  left_join(gtsmall,by="year") %>% 
  left_join(co2ppm,by="year") %>%
  gather("key","value",2:5)

p3 <- ggplot(wt_co2,aes(x=year,y=value,color=key)) +geom_line() +geom_smooth(method='loess')
print(p3)

wt_co2_corr <- co2small %>% 
  left_join(gtsmall,by="year") 

cor(wt_co2_corr$LandAverageTemperature_zt_ya,wt_co2_corr$kt_co2_zt)
cor(wt_co2_corr$LandAndOceanAverageTemperature_zt_ya,wt_co2_corr$kt_co2_zt)



