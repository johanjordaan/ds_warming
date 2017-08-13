library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(rebus)

gt <- fread("./data/GlobalTemperatures.csv")
gt <- gt %>% mutate(dt=ymd(dt))
p <- ggplot(gt,aes(x=dt,y=LandAverageTemperature))+geom_smooth(method='loess')
print(p)

co2 <- fread("./data/API_EN.ATM.CO2E.KT_DS2_en_csv_v2.csv")
co2 <- co2 %>% filter(`Country Name`=='World')
co2 <- select(co2,matches("[0-9][0-9][0-9][0-9]"))
co2 <- gather(co2,"year","kt_co2")
co2 <- co2 %>% mutate(
  year=ymd(paste(year,"01","01",sep="-")),
  kt_co2=as.numeric(kt_co2)
)



p2 <- ggplot(co2,aes(x=year,y=kt_co2))+geom_smooth(method='loess')
print(p2)
