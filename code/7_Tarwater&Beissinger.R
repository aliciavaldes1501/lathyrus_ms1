library(car)
library(lme4)
library(lmerTest)
library(dplyr)
library(broom)
library(MCMCglmm)

#Selection gradients accross 22 years of study
summary(lmer(n_intact_seeds_rel ~ FFD_std*n_fl_std+I(FFD_std^2)+I(n_fl_std^2)+(1|id), data = data_sel)) 
Anova(lmer(n_intact_seeds_rel ~ FFD_std*n_fl_std+I(FFD_std^2)+I(n_fl_std^2)+(1|id), data = data_sel),type="II") 

#Estimation of selection gradients
#1
sel_models_linear<-data_sel %>% 
  group_by(year) %>% 
  do(model = lm(n_intact_seeds_rel ~ FFD_std+n_fl_std, data = .)) %>%
  mutate(AIC=AIC(model))

sel_models_nonlinear<-data_sel %>% 
  group_by(year) %>% 
  do(model = lm(n_intact_seeds_rel ~ FFD_std+n_fl_std+FFD_std:n_fl_std+I(FFD_std^2)+I(n_fl_std^2), data = .)) %>%
  mutate(AIC=AIC(model))

compare<-as.data.frame(cbind(sel_models_linear$AIC,sel_models_nonlinear$AIC))
names(compare)<-c("AIC_linear","AIC_nonlinear")
compare$best<-as.factor(with(compare,ifelse(AIC_linear<AIC_nonlinear,"linear","nonlinear")))
compare$year<-as.factor(c(1987:1996,2006:2017))
compare

sel_models_linear_coefs<-data.frame(sel_models_linear %>% tidy(model))
sel_models_linear_coefs$sig<-ifelse(sel_models_linear_coefs$p.value<0.05,"*","") 

sel_models_nonlinear_coefs<-data.frame(sel_models_nonlinear %>% tidy(model))
sel_models_nonlinear_coefs$sig<-ifelse(sel_models_nonlinear_coefs$p.value<0.05,"*","") 

sel_models_linear_coefs
sel_models_nonlinear_coefs

#2

linear_coefs<-subset(sel_models_linear_coefs,term=="FFD_std")[c(1,4:5)]

plot(linear_coefs$estimate, I(1/linear_coefs$std.error)) # this makes the funnel plot.

randomtest <- MCMCglmm (estimate~1, data = linear_coefs,nitt = 60000)
#The intercept is going to estimate the average change in arrival date across all data points. 
summary(randomtest)

plot(randomtest$Sol) # Fixed effects
plot(randomtest$VCV) # Random effects

xsim <- simulate(randomtest) # reruns 100 new models, based around the same variance/covariance structures but with simulated data.

plot(linear_coefs$estimate, I(1/linear_coefs$std.error))
points(xsim, I(1/linear_coefs$std.error), col = "red") # here you can plot the data from both your simulated and real datasets and compare them

prior3<-list(R=list(V=diag(1),nu=0.002), G=list(G1=list(V=diag(1), nu=1, alpha.mu=0, alpha.V=diag(1)*a)))

randomerror3 <- MCMCglmm (estimate~1, random = ~idh(std.error):units, 
                          data = linear_coefs,  prior=prior3, nitt = 100000,burnin = 10000,thin = 10)
summary(randomerror3)

plot(randomerror3$Sol) # Fixed effects
plot(randomerror3$VCV) # Random effects

xsim<-simulate(randomerror3)

plot(linear_coefs$estimate, I(1/linear_coefs$std.error))
points(xsim, I(1/linear_coefs$std.error), col = "red") # here you can plot the data from both your simulated and real datasets and compare them

xsim<-simulate(randomerror3, 1000) # 1000 represents the number of simulations, 
# and for some reason needs to be higher than the default to work in this case
hist(apply(xsim, 2, max), breaks = 30) # plot your simulation data

abline(v = max(linear_coefs$estimate), col = "red") 

#3

summary(lmer(n_intact_seeds_rel ~ FFD_std*n_fl_std+I(FFD_std^2)+I(n_fl_std^2)+year+(1|id), data = data_sel,REML=F)) 
Anova(lmer(n_intact_seeds_rel ~ FFD_std*n_fl_std+I(FFD_std^2)+I(n_fl_std^2)+year+(1|id), data = data_sel,REML=F)) 

step(lmer(n_intact_seeds_rel ~ FFD_std*n_fl_std+I(FFD_std^2)+I(n_fl_std^2)+year+(1|id), data = data_sel,REML=F)) 
dredge1<-dredge(lmer(n_intact_seeds_rel ~ FFD_std*n_fl_std+I(FFD_std^2)+I(n_fl_std^2)+year+(1|id), data = data_sel,REML=F)) 
summary(model.avg(dredge1,subset = delta < 2))

dredge2<-dredge(lmer(n_intact_seeds_rel ~ FFD_std+n_fl_std+year+(1|id), data = data_sel,REML=F)) 
dredge2 #Only one with delta<2

#Drivers

#frequentist
step(lm(selgradFFD~grazing+prop_pred_seeds+seeds_per_fl+precipitation_3,data=data_sel_agg)) 
summary(lm(formula = selgradFFD ~ grazing + seeds_per_fl, data = data_sel_agg)) #grazing*

step(lm(selgradFFD~grazing+prop_pred_seeds+fr_set+seed_set+precipitation_3,data=data_sel_agg)) 
summary(lm(formula = selgradFFD ~ grazing + fr_set, data = data_sel_agg)) #grazing*

#bayesian

#Where are the corrected coefficients?






