library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)

#reading in data from climate engine on Hayman fire
ndvi<- read.csv('Data/hayman_ndvi.csv')%>%
  rename(burned=2,unburned=3)%>%
  filter(!is.na(burned),!is.na(unburned))

#chaning data from wide to long  
ndvi_long <- gather(ndvi,key='site',value='NDVI',-DateTime)
head(ndvi_long)

#cleaning data to observe the ndvi in the growing months (may-sep)
ndvi_annual <- ndvi_long %>%
  mutate(year=year(DateTime)) %>%
  mutate(month=month(DateTime)) %>%
  filter(month %in% c(5,6,7,8,9)) %>%
  group_by(site,year) %>%
  summarize(mean_NDVI=mean(NDVI))

#plot of ndvi_long
ggplot(ndvi_long,aes(x=DateTime,y=NDVI,color=site,group=1)) +
  geom_line() +
  theme_few() +
  scale_color_few() +
  theme(legend.position=c(0.2,0.3))

#plot of ndvi_annual
ggplot(ndvi_annual,aes(x=year,y=mean_NDVI,color=site)) +
  geom_point(shape=1) +
  geom_line() +
  theme_few() +
  scale_color_few() +
  theme(legend.position=c(0.2,0.3))



