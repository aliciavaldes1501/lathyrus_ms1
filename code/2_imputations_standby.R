library(lubridate)
library(effects)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)

data_imput<-read.table("C:/Users/User/Dropbox/SU/Projects/lathyrus/data/clean/data_20062017_forimput.txt",
                       header=T,sep="\t",dec=",")
head(data_imput)
data_imput$FFD_corr<-as.Date(as.character(data_imput$FFD_corr),format="%d/%m/%Y")
str(data_imput)

levels(data_imput$FFD_action)
levels(data_imput$shoot_vol_action)
levels(data_imput$cum_n_fl_action)

data_imput$FFD_action_new<-as.factor(ifelse(data_imput$FFD_action=="impute"|data_imput$FFD_action=="impute_again","impute","ok"))
data_imput$shoot_vol_action_new<-as.factor(ifelse(data_imput$shoot_vol_action=="impute","impute","ok"))

#Impute FFD from shoot_vol and n_fl####
nrow(subset(data_imput,FFD_action_new=="ok")) #FFD is ok - 953
nrow(subset(data_imput,FFD_action_new=="impute")) #FFD needs to be imputed - 101 (101/1054=9.6% to be imputed)
nrow(subset(data_imput,is.na(FFD_action_new))) #FFD is NA

data_imput$FFD_julian<-yday(data_imput$FFD_corr) #Julian dates - what to do with leap years?

subset1<-subset(data_imput,FFD_action_new=="ok"&shoot_vol_action_new=="ok"&cum_n_fl_action=="ok")
nrow(subset1) #all 3 traits - 923

with(subset1,hist(FFD_julian))#Quite normal

#Remove by now 1 plant with shoot_vol=0
subset1<-subset(subset1,shoot_vol>0)

with(subset1,plot(FFD_julian~shoot_vol))
with(subset1,plot(FFD_julian~log(shoot_vol)))
abline(lm(FFD_julian~log(shoot_vol),data=subset1))
summary(lm(FFD_julian~log(shoot_vol),data=subset1))

with(subset1,plot(FFD_julian~cum_n_fl))
with(subset1,plot(FFD_julian~log(cum_n_fl)))
abline(lm(FFD_julian~log(cum_n_fl),data=subset1))
summary(lm(FFD_julian~log(cum_n_fl),data=subset1))

model_FFD1<-lm(FFD_julian~shoot_vol*cum_n_fl,data=subset1)
nobs(model_FFD1)
summary(model_FFD1)
plot(model_FFD1) #OK

model_FFD1<-lm(FFD_julian~log(shoot_vol)*log(cum_n_fl),data=subset1)
nobs(model_FFD1)
summary(model_FFD1)
plot(model_FFD1) #OK

interaction_FFD<-data.frame(effect(term="shoot_vol:cum_n_fl", mod=model_FFD1,
                 xlevels=list(cum_n_fl=seq(0,290,5), shoot_vol=seq(0,30500,100))))

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

ggplot(interaction_FFD, aes(shoot_vol,fit, group = as.factor(cum_n_fl)))+
  geom_smooth(method=lm,se=F,size=0.3,aes(shoot_vol,fit,color=cum_n_fl))+
  xlab("Shoot volume")+ylab("FFD")+theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+labs(colour="Number of flowers")+
  scale_x_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000))+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))

#We can use model_FFD1 to impute values when both shoot_vol and cum_n_fl are available
#What to do when only one of these variables is available? - set the other to the mean, or use univariate regression?

