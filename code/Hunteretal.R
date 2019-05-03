library(MCMCglmm)
library(arm)
library(lme4)

datos<-merge(subset(seldiffs_FFD,term=="FFD_std")[c(1,3:4)],data_sel_agg[c(1,128,139:140)])
datos$seldiff<-datos$estimate
datos$SE_seldiff<-datos$std.error
datos$estimate<-NULL
datos$std.error<-NULL
datos<-merge(datos,subset(selgrads_FFD,term=="FFD_std")[c(1,3:4)])
datos$selgrad<-datos$estimate
datos$SE_selgrad<-datos$std.error
datos$estimate<-NULL
datos$std.error<-NULL
datos$SE_seldiff_sq<-(datos$SE_seldiff)^2
datos$SE_selgrad_sq<-(datos$SE_selgrad)^2
head(datos)

# Defining priors: diffuse inverse-gamma priors (from http://devillemereuil.legtux.org/wp-content/uploads/2012/12/tuto_en.pdf)

prior_fix <- list(R = list(V=1, nu=0.002))
prior_ran <- list(R = list(V=1, nu=0.002), G = list(G1 = list(V=1, nu=0.002),
                                                    G2 = list(V=1, nu=0.002), G3=list(V=1, nu=0.002)))
# Selection differentials

seldiff_fix<-MCMCglmm(seldiff~min_4+precipitation_3+precipitation_4,mev=datos$SE_seldiff_sq,
                      data=datos,family="gaussian",nitt=500000,burnin=10000,thin=10,prior=prior_fix)
seldiff_ran<-MCMCglmm(seldiff~1,random=~min_4+precipitation_3+precipitation_4,mev=datos$SE_seldiff_sq,
                      data=datos,family="gaussian",nitt=500000,burnin=10000,thin=10,prior=prior_ran)
plot(seldiff_fix$Sol)
plot(seldiff_fix$VCV)
autocorr.diag(seldiff_fix$Sol)
autocorr.diag(seldiff_fix$VCV)
summary(seldiff_fix)

plot(seldiff_ran$Sol)
plot(seldiff_ran$VCV)
autocorr.diag(seldiff_ran$Sol)
autocorr.diag(seldiff_ran$VCV)
summary(seldiff_ran)

propvar_seldiff<-(seldiff_ran$VCV[, 1]+seldiff_ran$VCV[, 2]+seldiff_ran$VCV[, 3])/
  (seldiff_ran$VCV[, 1]+seldiff_ran$VCV[, 2]+seldiff_ran$VCV[, 3] + seldiff_ran$VCV[, 4] + seldiff_ran$VCV[, 5])
mean(propvar_seldiff) ### variance in selection explained by climatic variables
effectiveSize(propvar_seldiff)
HPDinterval(propvar_seldiff)
plot(propvar_seldiff)
posterior.mode(propvar_seldiff)

# Plot the posterior distribution as a histogram to check for significance and whether it's been well estimated or not
# Variance cannot be zero, and therefore if the mean value is pushed up against zero your effect is not significant
# The larger the spread of the histogram, the less well estimated the distribution is.

par(mfrow = c(1,3))

hist(mcmc(seldiff_ran$VCV)[,"min_4"])
hist(mcmc(seldiff_ran$VCV)[,"precipitation_3"])
hist(mcmc(seldiff_ran$VCV)[,"precipitation_4"])

par(mfrow=c(1,1)) # Reset the plot panel back to single plots

# Selection gradients

selgrad_fix<-MCMCglmm(selgrad~min_4+precipitation_3+precipitation_4,mev=datos$SE_selgrad_sq,
                      data=datos,family="gaussian",nitt=500000,burnin=10000,thin=10,prior=prior_fix)
selgrad_ran<-MCMCglmm(selgrad~1,random=~min_4+precipitation_3+precipitation_4,mev=datos$SE_selgrad_sq,
                      data=datos,family="gaussian",nitt=500000,burnin=10000,thin=10,prior=prior_ran)
plot(selgrad_fix$Sol)
plot(selgrad_fix$VCV)
autocorr.diag(selgrad_fix$Sol)
autocorr.diag(selgrad_fix$VCV)
summary(selgrad_fix)

plot(selgrad_ran$Sol)
plot(selgrad_ran$VCV)
autocorr.diag(selgrad_ran$Sol)
autocorr.diag(selgrad_ran$VCV)
summary(selgrad_ran)

propvar_selgrad<-(selgrad_ran$VCV[, 1]+selgrad_ran$VCV[, 2]+selgrad_ran$VCV[, 3])/
  (selgrad_ran$VCV[, 1]+selgrad_ran$VCV[, 2]+selgrad_ran$VCV[, 3] + selgrad_ran$VCV[, 4] + selgrad_ran$VCV[, 5])
mean(propvar_selgrad) ### variance in selection explained by climatic variables
effectiveSize(propvar_selgrad)
HPDinterval(propvar_selgrad)
plot(propvar_selgrad)
posterior.mode(propvar_selgrad)

# Plot the posterior distribution as a histogram to check for significance and whether it's been well estimated or not
# Variance cannot be zero, and therefore if the mean value is pushed up against zero your effect is not significant
# The larger the spread of the histogram, the less well estimated the distribution is.

par(mfrow = c(1,3))

hist(mcmc(selgrad_ran$VCV)[,"min_4"])
hist(mcmc(selgrad_ran$VCV)[,"precipitation_3"])
hist(mcmc(selgrad_ran$VCV)[,"precipitation_4"])

par(mfrow=c(1,1)) # Reset the plot panel back to single plots

# From: https://www.researchgate.net/post/How_can_I_calculate_R2_for_an_Bayesian_MCMC_multilevel_model

# SELECTION DIFFERENTIALS

# Extraction of fitted value 

# MCMCglmm (it is probably better to get a posterior distribuiton of R2 
# rather than getting each variance component - we do this below as an alternative)
mFixed <- mean(seldiff_fix$Sol[,2]) * seldiff_fix$X[, 2] + 
          mean(seldiff_fix$Sol[, 3]) * seldiff_fix$X[, 3] + 
          mean(seldiff_fix$Sol[ ,4]) * seldiff_fix$X[, 4]

# Calculation of the variance in fitted values
mVarF<- var(mFixed)

# An alternative way for getting the same result
mVarF <- var(as.vector(apply(seldiff_fix$Sol,2,mean) %*% t(seldiff_fix$X)))

# R2GLMM(m) - marginal R2GLMM
# Equ. 26, 29 and 30
# MCMCglmm - marginal
mVarF/(mVarF+sum(apply(seldiff_fix$VCV,2,mean)))

# alternative with crebile intervals
vmVarF<-numeric(1000)
for(i in 1:1000){
  Var<-var(as.vector(seldiff_fix$Sol[i,] %*% t(seldiff_fix$X)))
  vmVarF[i]<-Var}
R2m<-vmVarF/(vmVarF+seldiff_fix$VCV[,1]+seldiff_fix$VCV[,2])
mean(R2m)
posterior.mode(R2m)
HPDinterval(R2m)

# R2GLMM(c) - conditional R2GLMM for full model
# Equ. 30
# MCMCglmm - conditional
(mVarF+sum(apply(seldiff_fix$VCV,2,mean)[-2]))/(mVarF+sum(apply(seldiff_fix$VCV,2,mean)))
# alternative with crebile intervals
R2c<-(vmVarF+seldiff_fix$VCV[,1])/(vmVarF+seldiff_fix$VCV[,1]+seldiff_fix$VCV[,2])
mean(R2c)
posterior.mode(R2c)
HPDinterval(R2c)

# SELECTION GRADIENTS

# Extraction of fitted value 

# MCMCglmm (it is probably better to get a posterior distribuiton of R2 
# rather than getting each variance component - we do this below as an alternative)
mFixed <- mean(selgrad_fix$Sol[,2]) * selgrad_fix$X[, 2] + 
  mean(selgrad_fix$Sol[, 3]) * selgrad_fix$X[, 3] + 
  mean(selgrad_fix$Sol[ ,4]) * selgrad_fix$X[, 4]

# Calculation of the variance in fitted values
mVarF<- var(mFixed)

# An alternative way for getting the same result
mVarF <- var(as.vector(apply(selgrad_fix$Sol,2,mean) %*% t(selgrad_fix$X)))

# R2GLMM(m) - marginal R2GLMM
# Equ. 26, 29 and 30
# MCMCglmm - marginal
mVarF/(mVarF+sum(apply(selgrad_fix$VCV,2,mean)))

# alternative with crebile intervals
vmVarF<-numeric(1000)
for(i in 1:1000){
  Var<-var(as.vector(selgrad_fix$Sol[i,] %*% t(selgrad_fix$X)))
  vmVarF[i]<-Var}
R2m<-vmVarF/(vmVarF+selgrad_fix$VCV[,1]+selgrad_fix$VCV[,2])
mean(R2m)
posterior.mode(R2m)
HPDinterval(R2m)

# R2GLMM(c) - conditional R2GLMM for full model
# Equ. 30
# MCMCglmm - conditional
(mVarF+sum(apply(selgrad_fix$VCV,2,mean)[-2]))/(mVarF+sum(apply(selgrad_fix$VCV,2,mean)))
# alternative with crebile intervals
R2c<-(vmVarF+selgrad_fix$VCV[,1])/(vmVarF+selgrad_fix$VCV[,1]+selgrad_fix$VCV[,2])
mean(R2c)
posterior.mode(R2c)
HPDinterval(R2c)

# Suggested by Morrissey in email

var_env<-t(apply(seldiff_fix$Sol[,2:4],2,mean))%*%var(seldiff_fix$Sol)%*%(apply(seldiff_fix$Sol[,2:4],2,mean))

# mVarF <- var(as.vector(apply(selgrad_fix$Sol,2,mean) %*% t(selgrad_fix$X)))


