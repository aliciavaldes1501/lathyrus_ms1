library(glmmLasso)
library(MASS)
library(nlme)
library(beepr)

LASSO_FFD<-glmmLasso(fix=FFD~max_3+max_4+max_5+mean_3+mean_4+mean_5+
                       min_3+min_4+min_5+precipitation_3+precipitation_4+
                       precipitation_5+n_fl, rnd=list(id=~1),data=subset1,
                     lambda=9000, family = gaussian(link="identity"),
                     switch.NR=F, final.re=T, control = list())
summary(LASSO_FFD)

subset1$id<-droplevels(subset1$id)

# ESTIMATE LAMBDA

# lambda sequence starts at a value big enough such that all covariates are
# shrinked to zero;

# 3 different methods

##### 1 ####
# Using BIC (or AIC, respectively) to determine the optimal tuning parameter lambda
lambda <- seq(10000,0,by=-100)

BIC_vec<-rep(Inf,length(lambda))

# first fit good starting model
PQL_FFD<-glmmPQL(FFD~1,random = ~1|id,family=gaussian(link="identity"),data=subset1)

Delta.start_FFD<-c(as.numeric(PQL_FFD$coef$fixed),rep(0,13), #13 for nr of fixed effs
                   as.numeric(t(PQL_FFD$coef$random$id)))
Q.start_FFD<-as.numeric(VarCorr(PQL_FFD)[1,1])

for(j in 1:length(lambda))
{
  print(paste("Iteration ", j,sep=""))
  
  glm1 <- try(glmmLasso(FFD~max_3+max_4+max_5+mean_3+mean_4+mean_5+
                          min_3+min_4+min_5+precipitation_3+precipitation_4+
                          precipitation_5+n_fl, rnd = list(id=~1),
                        family=gaussian(link="identity"),data=subset1, lambda=lambda[j],switch.NR=T,final.re=TRUE,
                        control=list(start=Delta.start_FFD,q_start=Q.start_FFD)), silent=TRUE)  
  
  
  if(class(glm1)!="try-error")
  {  
    BIC_vec[j]<-glm1$bic
  }
  
}

opt<-which.min(BIC_vec)

glm1_final <- glmmLasso(FFD~max_3+max_4+max_5+mean_3+mean_4+mean_5+
                          min_3+min_4+min_5+precipitation_3+precipitation_4+
                          precipitation_5+n_fl, rnd = list(id=~1),  
                        family = gaussian(link="identity"), data = subset1,
                        lambda=lambda[opt],switch.NR=F,final.re=TRUE,
                        control=list(start=Delta.start_FFD,q_start=Q.start_FFD))

summary(glm1_final)

#lambda[opt]=200

##### 2 ####
# Using 5-fold CV to determine the optimal tuning parameter lambda

# set seed
set.seed(123)

N<-dim(subset1)[1]

ind<-sample(N,N)

kk<-5

nk <- floor(N/kk)

Devianz_ma<-matrix(Inf,ncol=kk,nrow=length(lambda))

for(j in 1:length(lambda))
{
  print(paste("Iteration ", j,sep=""))
  
  for (i in 1:kk)
  {
    if (i < kk)
    {
      indi <- ind[(i-1)*nk+(1:nk)]
    }else{
      indi <- ind[((i-1)*nk+1):N]
    }
    
    subset1.train<-subset1[-indi,]
    subset1.test<-subset1[indi,]
    
    glm2 <- try(glmmLasso(FFD~max_3+max_4+max_5+mean_3+mean_4+mean_5+
                            min_3+min_4+min_5+precipitation_3+precipitation_4+
                            precipitation_5+n_fl, rnd = list(id=~1),
                          family=gaussian(link="identity"),data=subset1.train, lambda=lambda[j],switch.NR=F,final.re=TRUE,
                          control=list(start=Delta.start_FFD,q_start=Q.start_FFD))
                ,silent=TRUE) 
    
    if(class(glm2)!="try-error")
    {  
      y.hat<-predict(glm2,subset1.test)    
      
      Devianz_ma[j,i]<-sum(family$dev.resids(subset1.test$FFD,
                                             y.hat,wt=rep(1,length(y.hat))))
    }
  }
  print(sum(Devianz_ma[j,]))
}

Devianz_vec<-apply(Devianz_ma,1,sum)
opt2<-which.min(Devianz_vec)

glm2_final <- glmmLasso(FFD~max_3+max_4+max_5+mean_3+mean_4+mean_5+
                          min_3+min_4+min_5+precipitation_3+precipitation_4+
                          precipitation_5+n_fl, rnd = list(id=~1),  
                        family = gaussian(link="identity"), data = subset1, lambda=lambda[opt2],switch.NR=F,final.re=TRUE,
                        control=list(start=Delta.start_FFD,q_start=Q.start_FFD))

summary(glm2_final)

#lambda[opt2]=2900


##### 3 ####
# More Elegant Method 
# Idea: start with big lambda and use the estimates of the previous fit 
# (BUT: before the final re-estimation Fisher scoring is performed!) as starting
# values for the next fit; make sure, that your lambda sequence starts at a value
# big enough such that all covariates are shrinked to zero;

# specify starting values for the very first fit; pay attention that Delta.start has suitable length! 
Delta.start_FFD_1<-as.matrix(t(rep(0,14+length(levels(subset1$id)))))
Q.start_FFD_1<-0.1  
# using the 3rd example in the demo that is linked in the question where it uses 
# previous run results as the initialization of the parameters.

for(j in 1:length(lambda))
{
  print(paste("Iteration ", j,sep=""))
  
  glm3 <- glmmLasso(FFD~max_3+max_4+max_5+mean_3+mean_4+mean_5+
                      min_3+min_4+min_5+precipitation_3+precipitation_4+
                      precipitation_5+n_fl, rnd = list(id=~1),
                    family=gaussian(link="identity"),data=subset1, 
                    lambda=lambda[j], switch.NR=F,final.re=TRUE,
                    control = list(start=Delta.start_FFD_1[j,],
                                   q_start=Q.start_FFD_1[j]))  
  
  print(colnames(glm3$Deltamatrix)[2:7][glm3$Deltamatrix[glm3$conv.step,2:7]!=0])
  BIC_vec[j]<-glm3$bic
  Delta.start_FFD_1<-rbind(Delta.start_FFD_1,glm3$Deltamatrix[glm3$conv.step,])
  Q.start_FFD_1<-c(Q.start_FFD_1,glm3$Q_long[[glm3$conv.step+1]])
}

opt3<-which.min(BIC_vec)

glm3_final <- glmmLasso(FFD~max_3+max_4+max_5+mean_3+mean_4+mean_5+
                          min_3+min_4+min_5+precipitation_3+precipitation_4+
                          precipitation_5+n_fl, rnd = list(id=~1),
                        family=gaussian(link="identity"),data=subset1,
                        lambda=lambda[opt3],switch.NR=F,final.re=TRUE,
                        control = list(start=Delta.start_FFD_1[opt3,],
                                       q_start=Q.start_FFD_1[opt3]))  

summary(glm3_final)

# lambda[opt3]=100

## plot coefficient paths
par(mar=c(6,6,4,4))
plot(lambda,Delta.start_FFD_1[2:(length(lambda)+1),2],type="l",ylim=c(-1e-1,1e-1),
     ylab=expression(hat(beta[j])))
lines(c(-1000,1000),c(0,0),lty=2)
for(i in 3:7){
  lines(lambda[1:length(lambda)],Delta.start_FFD_1[2:(length(lambda)+1),i])
}
abline(v=lambda[opt3],lty=2)

beep()

