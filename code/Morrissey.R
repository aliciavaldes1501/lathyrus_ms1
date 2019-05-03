#load("./mymodel")

# me checking out what we've got:
mymodel<-MCMCglmm(seldiff~precipitation_4,mev=new_S[1:21,]$SE.S^2,
                      data=subset(datos,!year==2017),family="gaussian",nitt=500000,burnin=10000,thin=10,prior=prior_fix)
summary(mymodel)
head(mymodel$Sol)
head(mymodel$VCV)

nMCMC<-dim(mymodel$Sol)[1] # Sample size

# to contain posterior distribution of the variance
# in S associated with the environment
post.Var<-array(dim=nMCMC)

# The posterior samples of the partial regression coefficients of S on your four predictor variables
# relate to the variance in S associated with the multivariate environment according to:
# V(Y) = T(B) %*% V(X) %*% B (standard expression for a variance of a linear transformation)
# V(Y) = variance of selection differentials arising from environmental variation
# V(X) = covariance matrix of the environmental variable
# B = linear transformation of X onto Y (partial regression coefficients)

# Apply this transformation to each posterior sample to generate a posterior distribution of 
# the variance in S associated with environmental variables
# This is the numerator of the expression given by Darren for the proportion of variance explained
# The denominator is this quantity plus the residual variance (equation 12)

# calculate the variance, over all posterior samples
Sigma<-var(as.matrix(mymodel$X[,2])) # Covariance matrix of the environmental variables
for(i in 1:nMCMC){
  post.Var[i]<- t(mymodel$Sol[i,2]) %*% Sigma %*%mymodel$Sol[i,2] # Variance of seldiffs arising from environmental variation
}

# two flavours of the estimate of the variance associated with the climate variables
# (Numerator of equation 12 in paper)
mean(post.Var)
posterior.mode(as.mcmc(post.Var))

# 95% credible interval of the variance
HPDinterval(as.mcmc(post.Var))

# visualise
par(mfrow=c(1,2))
plot(density(post.Var))


# two flavours of the estimate of the proportion of variance associated with the climate variables
# (Equation 12 in paper)
mean(post.Var/(post.Var+mymodel$VCV[,2]))
posterior.mode(as.mcmc(post.Var/(post.Var+mymodel$VCV[,2])))

# 95% credible interval of the variance
HPDinterval(as.mcmc(post.Var/(post.Var+mymodel$VCV[,2])))

# visualise
plot(density(post.Var/(post.Var+mymodel$VCV[,2])))


# Function that calculates the SEs of selection differentials by bootstrapping

differential.SE<-function(z,W,n.boot=10000){
  n<-length(z)
  b<-array(dim=n.boot)
  d<-data.frame(z=z,W=W)
  for(i in 1:n.boot){
    db<-d[sample(1:n,n,replace=TRUE),]
    b[i]<-weighted.mean(db$z,db$W)-mean(db$z)
  }
  return(list(S=weighted.mean(z,W)-mean(z),SE.S=sd(b)))
}

# Here is a little example of how the function could be applied, 
# and compared to the standard regression-based approach

z<-rnorm(50,0,1) # trait
W<-rpois(50,0.5) # fitness
differential.SE((z-mean(z))/sd(z),W)

summary(lm(I(W/mean(W))~I((z-mean(z))/sd(z))))$coefficients

# With my data

differential.SE((z-mean(z))/sd(z),W)

differential.SE((subset(data_sel,year==2017)$FFD-mean(subset(data_sel,year==2017)$FFD))/sd(subset(data_sel,year==2017)$FFD),subset(data_sel,year==2017)$n_intact_seeds)


new_S<-as.data.frame(
  data_sel %>%
  group_by(year) %>%
  summarize(S = differential.SE((FFD-mean(FFD))/sd(FFD),n_intact_seeds)$S,
            SE.S = differential.SE((FFD-mean(FFD))/sd(FFD),n_intact_seeds)$SE.S))

# No SE calculated in 2017, probably because only 7 ids where fitness>0

comparison<-merge(new_S,subset(seldiffs_FFD,term=="FFD_std")[c(1,3:4)])
comparison$diffSE<-with(comparison,std.error-SE.S)
comparison
hist(comparison$diffSE)

