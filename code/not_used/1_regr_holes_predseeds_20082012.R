library(fmsb)
library(mgcv)

data_holes<-read.table("C:/Users/User/Dropbox/SU/Projects/lathyrus/data/clean/holes_predseeds_20082012.txt",
            header=T,sep="\t",dec=",")
head(data_holes)
str(data_holes)
names(data_holes)<-c("year","id","n_holes","n_p_seeds","n_seeds")
# data_holes<-subset(data_holes,n_p_seeds>0) #Use or not?

hist(data_holes$n_p_seeds,breaks=50)
hist(log(data_holes$n_p_seeds),breaks=50)
hist(log(data_holes$n_p_seeds+1),breaks=50)
hist(data_holes$n_holes,breaks=150)
hist(data_holes$n_seeds,breaks=50)

data_holes$prop_holes<-data_holes$n_holes/data_holes$n_seeds
data_holes$prop_p_seeds<-data_holes$n_p_seeds/data_holes$n_seeds

with(data_holes,hist(prop_holes))
with(data_holes,hist(prop_p_seeds))

subset1<-subset(data_holes,!is.na(prop_holes)&!is.na(prop_p_seeds))
with(subset1,plot(prop_p_seeds~prop_holes))

summary(lm(prop_p_seeds~prop_holes,subset1))
abline(lm(prop_p_seeds~prop_holes,subset1))

attach(subset1)

mod1<-nls(prop_p_seeds~a*(1-exp(-b*prop_holes)),start=list(a=1,b=0.92))
mod2<-nls(prop_p_seeds~a*(1-exp(-prop_holes)),start=list(a=1))
anova(mod1,mod2) #Keep simplified mod2
summary(mod1)
summary(mod2)

av <- seq(0,12,0.1)
bv <- predict(mod2,list(prop_holes=av))
lines(av,bv,col="red")

bv <- predict(mod1,list(prop_holes=av))
lines(av,bv,col="green")

sse <- as.vector((summary(mod2)[[3]])^2*124)
sse

null <- lm(prop_p_seeds~1)
sst <- as.vector(unlist(summary.aov(null)[[1]][2]))
sst

100*(sst-sse)/sst #Percentage variation explained

#Try 3-parameter asymptotic
mod3<-nls(prop_p_seeds~a-b*exp(-c*prop_holes),start=list(a=1,b=1,c=0.92))
summary(mod3)
anova(mod1,mod3)
anova(mod2,mod3)

bv <- predict(mod3,list(prop_holes=av))
lines(av,bv,col="blue")

sse <- as.vector((summary(mod3)[[3]])^2*122)
sse
100*(sst-sse)/sst #Percentage variation explained = little more

#Michaelis-Menten
mod4 <- nls(prop_p_seeds~a*prop_holes/(1+b*prop_holes),start=list(a=0.6,b=0.006))
summary(mod4)

yv <- predict(mod4, list(prop_holes=av))
lines(av,yv,col="orange")

sse <- as.vector((summary(mod4)[[3]])^2*123)
sse
100*(sst-sse)/sst #Percentage variation explained = little more

#Generalized additive models
mod5 <- gam(prop_p_seeds~s(prop_holes))
summary(mod5)

yv <- predict(mod5, list(prop_holes=av))
lines(av,yv)

#Self-starting Michaelis-Menten
mod6<-nls(prop_p_seeds~SSmicmen(prop_holes,a,b))
#Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : NA/NaN/Inf in 'x'

#Self-starting asymptotic exponential model through the origin
mod7 <- nls(prop_p_seeds~SSasympOrig(prop_holes,a,b))
summary(mod7)

yv <- predict(mod7,list(prop_holes=av))
lines(av,yv)

#Self-startingWeibull growth function
mod8<-nls(prop_p_seeds ~ SSweibull(prop_holes, Asym, Drop, lrc, pwr))
#Error in qr.default(.swts * attr(rhs, "gradient")) : NA/NaN/Inf in foreign function call (arg 1)

detach(subset1)

#2012####
subset2<-subset(subset1,year==2012)
with(subset2,plot(prop_p_seeds~prop_holes))


attach(subset2)

mod1<-nls(prop_p_seeds~a*(1-exp(-b*prop_holes)),start=list(a=1,b=0.92)) #Error
mod2<-nls(prop_p_seeds~a*(1-exp(-prop_holes)),start=list(a=1))

summary(mod2)

av <- seq(0,12,0.1)
bv <- predict(mod2,list(prop_holes=av))
lines(av,bv,col="red")

#Try 3-parameter asymptotic
mod3<-nls(prop_p_seeds~a-b*exp(-c*prop_holes),start=list(a=1,b=1,c=0.92)) #Error

#Michaelis-Menten
mod4 <- nls(prop_p_seeds~a*prop_holes/(1+b*prop_holes),start=list(a=0.6,b=0.006)) #Error

#Generalized additive models
mod5 <- gam(prop_p_seeds~s(prop_holes))
summary(mod5)

yv <- predict(mod5, list(prop_holes=av))
lines(av,yv)

#Self-starting Michaelis-Menten
mod6<-nls(prop_p_seeds~SSmicmen(prop_holes,a,b))
summary(mod6)

yv <- predict(mod6, list(prop_holes=av))
lines(av,yv)

#Self-starting asymptotic exponential model through the origin
mod7 <- nls(prop_p_seeds~SSasympOrig(prop_holes,a,b)) #Error

#Self-startingWeibull growth function
mod8<-nls(prop_p_seeds ~ SSweibull(prop_holes, Asym, Drop, lrc, pwr)) #Error

#n_p_seeds>0####
subset3<-subset(subset1,n_p_seeds>0)
with(subset3,plot(prop_p_seeds~prop_holes))

mod9<-nls(prop_p_seeds~a*(1-exp(-b*prop_holes)),start=list(a=1,b=0.51))
mod10<-nls(prop_p_seeds~a*(1-exp(-prop_holes)),start=list(a=1))
anova(mod9,mod10) #Keep simplified mod2
summary(mod9)
summary(mod10)

av <- seq(0,12,0.1)
bv <- predict(mod10,list(prop_holes=av))
lines(av,bv,col="red")

bv <- predict(mod9,list(prop_holes=av))
lines(av,bv,col="green")

sse <- as.vector((summary(mod10)[[3]])^2*124)
sse

null <- lm(prop_p_seeds~1)
sst <- as.vector(unlist(summary.aov(null)[[1]][2]))
sst

100*(sst-sse)/sst #Percentage variation explained

