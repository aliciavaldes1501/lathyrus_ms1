library(ggplot2)
library(ggridges)
library(tidyverse)
library(ggthemes)

ggplot(data_imput, aes(x = FFD_julian, y = as.factor(year))) + geom_density_ridges(fill="red",alpha = .5)+theme_ridges()+ 
  scale_x_continuous(breaks = c(110,120,130,140,150,160,70),labels = paste(c("20-Apr", "30-Apr", "10-May", "20-May", "30-May", "9-Jun","19 Jun")))+
  ylab("Year")+xlab("First flowering date")+theme(text = element_text(size=30))+
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20))

ggplot(data_imput, aes(x = grazing, y = as.factor(year))) + geom_density_ridges(fill="red",alpha = .5)+theme_ridges()+ 
    ylab("Year")+xlab("Proportion of grazing")+theme(text = element_text(size=30))+
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20))


data_8796<-read.table("C:/Users/User/Dropbox/SU/Projects/lathyrus/data/clean/data_19871996_FFD.txt",
                       header=T,sep="\t",dec=",")
head(data_8796)
str(data_8796)

ggplot(data_8796, aes(x = FFD_julian, y = as.factor(year))) + geom_density_ridges(fill="red",alpha = .5)+theme_ridges()+ 
  scale_x_continuous(breaks = c(110,120,130,140,150,160,70),labels = paste(c("20-Apr", "30-Apr", "10-May", "20-May", "30-May", "9-Jun","19 Jun")))+
    ylab("Year")+xlab("First flowering date")+theme(text = element_text(size=30))+
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20))

FFD_all<-rbind(data_8796,data_imput[c(1,13)])
ggplot(FFD_all, aes(x = FFD_julian, y = as.factor(year))) + geom_density_ridges(fill="red",alpha = .5,scale=3)+theme_ridges()+ 
  scale_x_continuous(breaks = c(110,120,130,140,150,160,70),labels = paste(c("20-Apr", "30-Apr", "10-May", "20-May", "30-May", "9-Jun","19 Jun")))+
  ylab("Year")+xlab("First flowering date")+theme(text = element_text(size=30))+
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20))
