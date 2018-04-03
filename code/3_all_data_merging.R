library(dplyr)

# #Matching of ids from 88 onwards to ruta+genet in 87
# data87<-read.table("C:/Users/User/Dropbox/SU/Projects/lathyrus/data/edited/matching87-88/data87.csv",header=T,sep="\t",dec=",") 
# data88<-read.table("C:/Users/User/Dropbox/SU/Projects/lathyrus/data/edited/matching87-88/data88.csv",header=T,sep="\t",dec=",") 
# head(data87)
# head(data88)
# 
# data8788<-merge(data87,data88,all.x=T,all.y=F)
# 
# head(data8788)
# 
# write.table(data8788,file="C:/Users/User/Dropbox/SU/Projects/lathyrus/data/edited/matching87-88/data8788.csv",sep="\t",dec=".",col.names=T)

#OK, matched

head(data_imput) #Data 2006-2017
data_imput$id<-paste("new",data_imput$id,sep="_") #Add "new" to the ids

data_8796<-read.table("C:/Users/User/Dropbox/SU/Projects/lathyrus/data/clean/data_19871996.csv",header=T,sep="\t",dec=",") 
head(data_8796)
str(data_8796)
data_8796$id<-paste("old",data_8796$id,sep="_") #Add "old" to the ids
data_8796$FFD1<-NULL
data_8796$start<-NULL
data_8796$FFD_corr<-as.Date(as.character(data_8796$FFD_date),format="%d/%m/%Y")
data_8796$FFD_date<-NULL
data_imput$ruta<-NA
data_imput$genet<-NA
data_imput<-data_imput[c("year", "id", "ruta","genet","data","vernal","FFD_action","grazing","shoot_vol","shoot_vol_action",
            "cum_n_fl","cum_n_fl_action","n_fr","n_ovules","total_n_seeds","total_n_intact_seeds","FFD_imputed","FFD_corr","FFD")]
data_8796$vernal<-as.POSIXct(data_8796$vernal,format="%d/%m/%y %H:%M")
data_8796$FFD<-as.numeric(with(data_8796,as.POSIXct(FFD_corr)-vernal))

head(data_8796)
head(data_imput)
alldata<-rbind(data_8796,data_imput) #Join both "old" and "new" data
head(alldata)
str(alldata)

alldata$period<-with(alldata,ifelse(grepl("old", alldata$id), "old", "new"))
alldata$year<-as.factor(alldata$year)
alldata$id<-as.factor(alldata$id)
alldata$data<-as.factor(alldata$data)
alldata$FFD_imputed<-as.factor(alldata$FFD_imputed)
alldata$period<-as.factor(alldata$period)

subset(alldata,n_ovules>0&n_fr==0) #No pls with ovules but not fruits
subset(alldata,total_n_seeds>0&n_ovules==0) #No pls with seeds but not ovules
subset(alldata,total_n_seeds>0&n_fr==0) #No pls with seeds but not fruits
subset(alldata,total_n_intact_seeds>0&n_fr==0) #No pls with intact seeds but not fruits

ov_per_fr<-alldata$n_ovules/alldata$n_fr
hist(ov_per_fr)
mean(ov_per_fr[!is.na(ov_per_fr)]) #mean n_ovules per fr is 12.9 (should be around 13)

#Pls with data=0 and actually some data?
subset(alldata,data==0&!is.na(FFD)) #No
subset(alldata,data==0&!is.na(cum_n_fl)) #No
subset(alldata,data==0&!is.na(n_fr)) #No
subset(alldata,data==0&!is.na(n_ovules))  #No
subset(alldata,data==0&!is.na(total_n_seeds))  #No
subset(alldata,data==0&!is.na(total_n_intact_seeds)) #No
subset(alldata,data==0&!is.na(shoot_vol)) #Yes, but OK
subset(alldata,data==0&!is.na(grazing)) #No

#Pls with data=1 and FFD missing
nrow(subset(alldata,data==1&period=="new"&is.na(FFD))) #46 from new period (where data could not be imputed)-OK
nrow(subset(alldata,data==1&period=="old"&is.na(FFD))) #85 from old period

#See them per years
data.frame(table(subset(alldata,data==1&is.na(FFD))$year))

subset(alldata,data==1&period=="old"&is.na(FFD)&year==1987) #0
subset(alldata,data==1&period=="old"&is.na(FFD)&year==1988) #6 - with n_fl but no FFD
subset(alldata,data==1&period=="old"&is.na(FFD)&year==1989) #30 - with n_fl but no FFD
subset(alldata,data==1&period=="old"&is.na(FFD)&year==1990) #3 - with n_fl but no FFD
subset(alldata,data==1&period=="old"&is.na(FFD)&year==1991) #1 - with n_fl but no FFD
subset(alldata,data==1&period=="old"&is.na(FFD)&year==1992) #1 - with n_fl but no FFD
subset(alldata,data==1&period=="old"&is.na(FFD)&year==1993) #1 - with n_fl but no FFD
subset(alldata,data==1&period=="old"&is.na(FFD)&year==1994) #8 - with n_fl but no FFD
subset(alldata,data==1&period=="old"&is.na(FFD)&year==1995) #35
subset(alldata,data==1&period=="old"&is.na(FFD)&year==1996) #0

#Pls with data=1 and cum_n_fl missing
nrow(subset(alldata,data==1&period=="new"&is.na(cum_n_fl))) #24 from new period-OK, to be imputed from shoot_vol
nrow(subset(alldata,data==1&period=="old"&is.na(cum_n_fl))) #51 from old period-All have shoot_vol, could also impute fl from there

subset(alldata,cum_n_fl==0) #No pls with 0 flowers

#Pls with data=1 and grazing missing
subset(alldata,data==1&is.na(grazing)) #No

subset(alldata,data==1&is.na(n_fr)) #0
subset(alldata,data==1&is.na(n_ovules)) #159, 1987-88-89-90-91-92-93-94
#Calculate mean n ovules per fr for each of these years --> USE FOR CALCULATING N_OVULES
n_ovules_per_fr<-with(subset(alldata,year==1987|year==1988|year==1989|year==1990|year==1991|year==1992|year==1993|year==1994|year==1995),aggregate(n_ovules/n_fr~year, FUN=mean))

alldata$n_ovules<-ifelse(is.na(alldata$n_ovules)&alldata$year=="1987",n_ovules_per_fr$`n_ovules/n_fr`[1]*alldata$n_fr,
                  ifelse(is.na(alldata$n_ovules)&alldata$year=="1988",n_ovules_per_fr$`n_ovules/n_fr`[2]*alldata$n_fr,
                  ifelse(is.na(alldata$n_ovules)&alldata$year=="1989",n_ovules_per_fr$`n_ovules/n_fr`[3]*alldata$n_fr,
                  ifelse(is.na(alldata$n_ovules)&alldata$year=="1990",n_ovules_per_fr$`n_ovules/n_fr`[4]*alldata$n_fr,
                  ifelse(is.na(alldata$n_ovules)&alldata$year=="1991",n_ovules_per_fr$`n_ovules/n_fr`[5]*alldata$n_fr,
                  ifelse(is.na(alldata$n_ovules)&alldata$year=="1992",n_ovules_per_fr$`n_ovules/n_fr`[6]*alldata$n_fr,
                  ifelse(is.na(alldata$n_ovules)&alldata$year=="1993",n_ovules_per_fr$`n_ovules/n_fr`[7]*alldata$n_fr,
                  ifelse(is.na(alldata$n_ovules)&alldata$year=="1994",n_ovules_per_fr$`n_ovules/n_fr`[8]*alldata$n_fr,
                  ifelse(is.na(alldata$n_ovules)&alldata$year=="1995",n_ovules_per_fr$`n_ovules/n_fr`[9]*alldata$n_fr,
                  alldata$n_ovules)))))))))

with(subset(alldata,year=="1987"),hist(n_ovules,breaks=200))
with(subset(alldata,year=="1988"),hist(n_ovules,breaks=200))
with(subset(alldata,year=="1989"),hist(n_ovules,breaks=200))
with(subset(alldata,year=="1990"),hist(n_ovules,breaks=200))
with(subset(alldata,year=="1991"),hist(n_ovules,breaks=200))
with(subset(alldata,year=="1992"),hist(n_ovules,breaks=200))
with(subset(alldata,year=="1993"),hist(n_ovules,breaks=200))
with(subset(alldata,year=="1994"),hist(n_ovules,breaks=200))
with(subset(alldata,year=="1995"),hist(n_ovules,breaks=200))

mean(ov_per_fr[!is.na(ov_per_fr)]) #Similar

subset(alldata,data==1&is.na(total_n_seeds)) #66
subset(alldata,data==1&is.na(total_n_intact_seeds)) #66

#Calculate mean total_n_seeds per fr for each of these years --> USE FOR CALCULATING total_n_seeds
total_n_seeds_per_fr<-with(subset(alldata,year==1990|year==1991|year==1992|year==1993|year==1994|year==1995),
                      aggregate(total_n_seeds/n_fr~year, FUN=mean))

alldata$total_n_seeds<-ifelse(is.na(alldata$total_n_seeds)&alldata$year=="1990",total_n_seeds_per_fr$`total_n_seeds/n_fr`[1]*alldata$n_fr,
                  ifelse(is.na(alldata$total_n_seeds)&alldata$year=="1991",total_n_seeds_per_fr$`total_n_seeds/n_fr`[2]*alldata$n_fr,
                  ifelse(is.na(alldata$total_n_seeds)&alldata$year=="1992",total_n_seeds_per_fr$`total_n_seeds/n_fr`[3]*alldata$n_fr,
                  ifelse(is.na(alldata$total_n_seeds)&alldata$year=="1993",total_n_seeds_per_fr$`total_n_seeds/n_fr`[4]*alldata$n_fr,
                  ifelse(is.na(alldata$total_n_seeds)&alldata$year=="1994",total_n_seeds_per_fr$`total_n_seeds/n_fr`[5]*alldata$n_fr,
                  ifelse(is.na(alldata$total_n_seeds)&alldata$year=="1995",total_n_seeds_per_fr$`total_n_seeds/n_fr`[6]*alldata$n_fr,
                  alldata$total_n_seeds))))))

hist(alldata$total_n_seeds)

#Calculate mean total_n_intact_seeds per fr for each of these years --> USE FOR CALCULATING total_n_intact_seeds
total_n_intact_seeds_per_fr<-with(subset(alldata,year==1990|year==1991|year==1992|year==1993|year==1994|year==1995),
                           aggregate(total_n_intact_seeds/n_fr~year, FUN=mean))

alldata$total_n_intact_seeds<-ifelse(is.na(alldata$total_n_intact_seeds)&alldata$year=="1990",total_n_intact_seeds_per_fr$`total_n_intact_seeds/n_fr`[1]*alldata$n_fr,
                      ifelse(is.na(alldata$total_n_intact_seeds)&alldata$year=="1991",total_n_intact_seeds_per_fr$`total_n_intact_seeds/n_fr`[2]*alldata$n_fr,
                      ifelse(is.na(alldata$total_n_intact_seeds)&alldata$year=="1992",total_n_intact_seeds_per_fr$`total_n_intact_seeds/n_fr`[3]*alldata$n_fr,
                      ifelse(is.na(alldata$total_n_intact_seeds)&alldata$year=="1993",total_n_intact_seeds_per_fr$`total_n_intact_seeds/n_fr`[4]*alldata$n_fr,
                      ifelse(is.na(alldata$total_n_intact_seeds)&alldata$year=="1994",total_n_intact_seeds_per_fr$`total_n_intact_seeds/n_fr`[5]*alldata$n_fr,
                      ifelse(is.na(alldata$total_n_intact_seeds)&alldata$year=="1995",total_n_intact_seeds_per_fr$`total_n_intact_seeds/n_fr`[6]*alldata$n_fr,
                      alldata$total_n_intact_seeds))))))

hist(alldata$total_n_intact_seeds)

subset(alldata,total_n_seeds>n_ovules) #none
subset(alldata,n_fr>0&total_n_seeds==0) #none
subset(alldata,n_fr>0&is.na(total_n_seeds)) #none
subset(alldata,total_n_intact_seeds>total_n_seeds) #none
subset(alldata,total_n_seeds>0&is.na(total_n_intact_seeds)) #none
subset(alldata,total_n_intact_seeds>0&is.na(total_n_seeds)) #none

#Status
#missing FFD / missing n_fl (still to be imputed from shoot_vol) / nodata / ok
alldata$status<-as.factor(ifelse(is.na(alldata$FFD)&alldata$data==1,"missFFD",ifelse(alldata$data==0,"nodata",
                ifelse(is.na(alldata$cum_n_fl)&alldata$data==1,"missnfl","ok"))))
plot(alldata$status)
plot(subset(alldata,period=="old")$status)
plot(subset(alldata,period=="new")$status)

nrow(subset(alldata,status=="missFFD")) #131
nrow(subset(alldata,status=="missnfl")) #66 
nrow(subset(alldata,status=="missnfl"&!is.na(shoot_vol))) #63 to impute cum_n_fl from shoot_vol

nrow(subset(alldata,data==1&shoot_vol==0)) #1 pl with shoot_vol=0, impute later if we need shoot volume?

#Imputation of cum_n_fl from shoot_vol

with(alldata,hist(shoot_vol))
with(alldata,hist(log(shoot_vol)))
with(alldata,hist(cum_n_fl))
with(alldata,hist(log(cum_n_fl)))

model_vol1<-lm(log(cum_n_fl)~log(shoot_vol),subset(alldata,data==1&shoot_vol>0&!is.na(cum_n_fl)))
summary(model_vol1) #R2=0.3427 
plot(model_vol1)
with(subset(alldata,data==1&shoot_vol>0),plot(log(cum_n_fl)~log(shoot_vol)))
abline(model_vol1)

model_vol2<-glm.nb(cum_n_fl~shoot_vol,subset(alldata,data==1&shoot_vol>0&!is.na(cum_n_fl)))
summary(model_vol2)
NagelkerkeR2(model_vol2) #R20.5709834 --> better, use
plot(model_vol2)

ggplot(subset(alldata,data==1&shoot_vol>0),aes(x=shoot_vol,y=cum_n_fl))+geom_point()+geom_smooth(method="lm")
ggplot(subset(alldata,data==1&shoot_vol>0),aes(x=log(shoot_vol),y=log(cum_n_fl)))+geom_point()+geom_smooth(method="lm")
ggplot(subset(alldata,data==1&shoot_vol>0),aes(x=shoot_vol,y=cum_n_fl))+geom_point()+geom_smooth(method="glm.nb")

predict_fl<-as.data.frame(predict(model_vol2,newdata=subset(alldata,status=="missnfl"&!is.na(shoot_vol)),type="response"))
names(predict_fl)<-c("cum_n_fl_i")
alldata<-merge(alldata, predict_fl, by="row.names",all.x=T) #New colum with imputed cum_n_fl (cum_n_fl_i) for pls where status=="missnfl" that had shoot_vol
alldata$Row.names<-NULL

alldata$n_fl<-ifelse(is.na(alldata$cum_n_fl),alldata$cum_n_fl_i,alldata$cum_n_fl)
alldata$n_fl_imputed<-ifelse(alldata$cum_n_fl_action=="impute",1,0) #Says if n_fl was imputed (1) or not (0)

#Actualize status
alldata$status<-as.factor(ifelse(is.na(alldata$FFD)&alldata$data==1,"missFFD",ifelse(alldata$data==0,"nodata",
                ifelse(is.na(alldata$n_fl)&alldata$data==1,"missnfl","ok"))))
plot(alldata$status)
nrow(subset(alldata,status=="missFFD")) #131
nrow(subset(alldata,status=="missnfl")) #3 

#Remove unnecesary columns
names(alldata)
alldata<-alldata[c(1:6,8,9,13:21,23:24)]
alldata$n_seeds<-alldata$total_n_seeds
alldata$n_intact_seeds<-alldata$total_n_intact_seeds
alldata$total_n_seeds<-NULL
alldata$total_n_intact_seeds<-NULL

names(alldata)
alldata<-alldata[c("year","period","id","ruta","genet","data","status","vernal","FFD_corr",
            "FFD","FFD_imputed","n_fl","n_fl_imputed","shoot_vol","grazing","n_fr",
            "n_ovules","n_seeds","n_intact_seeds")]
head(alldata)
subset(alldata,status=="missFFD"|status=="missnfl") #134
subset(subset(alldata,status=="missFFD"|status=="missnfl"),period=="new") #49
subset(subset(alldata,status=="missFFD"|status=="missnfl"),period=="old") #85
subset(subset(alldata,status=="missFFD"),period=="old") #85
#All these individuals without FFD from old period were either not included in the phenology study (most)
#or damaged by trampling before the recordings started (2 ind:s)
#Remove them (Johan)

alldata<-setdiff(alldata, subset(subset(alldata,status=="missFFD"),period=="old"))

alldata_ok<-subset(alldata,status=="ok")
plot(alldata_ok$year)  

write.table(alldata,file="C:/Users/User/Dropbox/SU/Projects/lathyrus/data/clean/alldata.csv",sep="\t",dec=".",col.names=T)
write.table(alldata_ok,file="C:/Users/User/Dropbox/SU/Projects/lathyrus/data/clean/alldata_ok.csv",sep="\t",dec=".",col.names=T)

save(alldata, file="alldata.RData")

