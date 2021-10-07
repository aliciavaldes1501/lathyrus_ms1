# Check if/how GS length varies among years
# in a way similar to Ensing & Eckert, New. Phyt 2019.
# We expect less year-to-year variation in GS length for Lathyrus

library(dplyr)
library(timetk)
library(tidyverse)

# Read data

weather_GDD<-read.table(
  "C:/Users/User/Dropbox/SU/Projects/lathyrus/lathyrus_ms1/data/clean/weather_GDD.csv",
  header=T,sep=",",dec=".")
head(weather_GDD)

# Annual accumulation of GDD (AnGDD)

# AnGDD makes no assumptions about when the growing season starts and ends

AnGDD<-weather_GDD %>%
  group_by(year) %>%
  dplyr::summarise(AnGDD=sum(GDD5))
AnGDD

ggplot(AnGDD,aes(year,AnGDD))+geom_point()+geom_smooth(method="lm")
summary(lm(AnGDD~year,AnGDD)) # Significantly larger with year
summary(lm(AnGDD~year,subset(AnGDD,year>1986))) # Also since start of the study

# Start and end of GS
  
# Start of the GS 
# estimated as first date that mean temperature in a 6-d sliding window was > 4°C 
  
# Make the rolling function
rolling_sum_6_start<-slidify(.f=sum,.period=6,.align="left",.partial=F)

# Apply rolling Function
GSstart<-weather_GDD%>%mutate(temp_over_4=ifelse(mean>4,1,0))%>%
  group_by(year)%>%
  mutate(rolling_sum_6 = rolling_sum_6_start(temp_over_4))%>%
  filter(rolling_sum_6==6) %>% 
  slice(1)%>%
  mutate(GSstart=as.Date(date))%>%
  select(GSstart,year,month,day)%>%
  mutate(DOY=as.numeric(format(GSstart, "%j"))) # Not correcting for leap years
GSstart  

ggplot(GSstart,aes(year,DOY))+geom_point()+geom_smooth(method="lm")
summary(lm(DOY~year,GSstart)) # Significantly earlier with year
summary(lm(DOY~year,subset(GSstart,year>1986))) # Not since start of study

# End of the GS 
# estimated as last date that mean temperature in a 6-d sliding window was > 4°C 

# Make the rolling function
rolling_sum_6_end<-slidify(.f=sum,.period=6,.align="right",.partial=F)

# Apply rolling Function
GSend<-weather_GDD%>%mutate(temp_over_4=ifelse(mean>4,1,0))%>%
  group_by(year)%>%
  mutate(rolling_sum_6 = rolling_sum_6_end(temp_over_4))%>%
  filter(month>5)%>%
  filter(rolling_sum_6<6) %>% 
  slice(1)%>%
  mutate(GSend=as.Date(date))%>%
  select(GSend,year,month,day)%>%
  mutate(DOY=as.numeric(format(GSend, "%j"))) # Not correcting for leap years
GSend  

ggplot(GSend,aes(year,DOY))+geom_point()+geom_smooth(method="lm")
summary(lm(DOY~year,GSend)) # Not significantly later with year
summary(lm(DOY~year,subset(GSend,year>1986))) # Not since start of study

# GDD accumulated from the beginning to the end of the GS (GsGDD)

GsGDD<-weather_GDD %>%
  inner_join(GSstart%>%select(GSstart,year))%>%
  inner_join(GSend%>%select(GSend,year))%>%
  filter(date>=GSstart&date<=GSend)%>%
  group_by(year)%>%
  dplyr::summarise(GsGDD=sum(GDD5))
GsGDD

ggplot(GsGDD,aes(year,GsGDD))+geom_point()+geom_smooth(method="lm")
summary(lm(GsGDD~year,GsGDD)) # Significantly larger with year
summary(lm(GsGDD~year,subset(GsGDD,year>1986))) # Also since start of the study

# Number of days during the estimated growing season (GsDays)

GsDays<-inner_join(GSstart%>%select(GSstart,year),GSend%>%select(GSend,year))%>%
  mutate(GsDays=as.numeric(GSend-GSstart))
GsDays

ggplot(GsDays,aes(year,GsDays))+geom_point()+geom_smooth(method="lm")
summary(lm(GsDays~year,GsDays)) # Significantly larger with year
summary(lm(GsDays~year,subset(GsDays,year>1986))) # Not since start of the study

# All three measures of season length

season_length<-inner_join(AnGDD,GsGDD)%>%
  inner_join(GsDays%>%select(year,GsDays))%>%
  pivot_longer(cols=AnGDD:GsDays,names_to="measure",values_to="GS_length")
season_length

ggplot(season_length,aes(x=year,y=GS_length))+geom_point()+
  geom_smooth(method="lm")+facet_wrap(~measure,scales="free")

ggplot(subset(season_length,year>1986),aes(x=year,y=GS_length))+geom_point()+
  geom_smooth(method="lm")+facet_wrap(~measure,scales="free")

# Differences in GS length between max and min values

with(subset(season_length,year>1986&measure=="AnGDD"),max(GS_length)-min(GS_length))
# 637.825 vs ~800 in Ensing & Eckert 2019 (estimated from Fig. 3)

with(subset(season_length,year>1986&measure=="GsGDD"),max(GS_length)-min(GS_length))
# 598.275 vs ~1000 in Ensing & Eckert 2019 (estimated from Fig. 3)

with(subset(season_length,year>1986&measure=="GsDays"),max(GS_length)-min(GS_length))
# 111 vs ~90 in Ensing & Eckert 2019 (estimated from Fig. 3)

# There is considerable variation, especially in AnGDD




