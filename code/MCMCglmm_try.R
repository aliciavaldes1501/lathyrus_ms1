library(MCMCglmm)
library(lme4)

prior<-list(G=list(G1=list(V=diag(2), nu=3, alpha.mu=c(0,0), alpha.v=diag(2)*1000),
                   G1=list(V=diag(2), nu=3, alpha.mu=c(0,0), alpha.v=diag(2)*1000)),
            R=list(V=diag(2), nu=3))

#cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl

mod1<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                 trait-1+trait:cumGDD5+trait:grazing+trait:prop_pred_seeds+trait:n_fl_std+trait:seeds_per_fl,
               ## RANDOM EFFECTS:
               ## us=unstructured, fits var/trait & covar traits
               ## us relaxes assumption of independence of groups
               ## us as both resp.vars measured for same inds
               random=~us(trait):year+us(trait):id ,
               rcov=~us(trait):units, #RESID variance
               family=c("gaussian", "poisson"), #for each resp.var.
               data=data_sel,
               prior=prior,
               verbose = FALSE,nitt=3000,burnin=500)
summary(mod1)

mod2<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                 cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
               ## RANDOM EFFECTS:
               ## us=unstructured, fits var/trait & covar traits
               ## us relaxes assumption of independence of groups
               ## us as both resp.vars measured for same inds
               random=~us(trait):year+us(trait):id ,
               rcov=~us(trait):units, #RESID variance
               family=c("gaussian", "poisson"), #for each resp.var.
               data=data_sel,
               prior=prior,
               verbose = FALSE,nitt=3000,burnin=500)
summary(mod2)

## AUTOCORRELATION
## OK if 2nd component between -0.1 and 0.1

AC <- as.data.frame(autocorr(mod2$VCV))
range(AC["Lag 500",])

plot(mod2$Sol)
plot(mod2$VCV)

posterior.mode(mod2$Sol[, "cumGDD5"])
HPDinterval(mod2$Sol[, "cumGDD5"], 0.95)

#Try different priors

################################################################################################

summary(glmer(round(n_intact_seeds_rel)~FFD_std*cumGDD5+(1|year)+(1|id),family="poisson",data=data_sel))
#Significant interaction!


###################################
# NOTES MEETING JOHAN #

#Always include n fl!

#1: random (?) effect of year and id --> see if there are differences among years in selection (covariance
#of fitness and FFD)

mod3<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 1,
               ## RANDOM EFFECTS:
               ## us=unstructured, fits var/trait & covar traits
               ## us relaxes assumption of independence of groups
               ## us as both resp.vars measured for same inds
               random=~us(trait):year+us(trait):id , #year and id as random
               rcov=~us(trait):units, #RESID variance
               family=c("gaussian", "poisson"), #for each resp.var.
               data=data_sel,
               prior=prior,
               verbose = FALSE,nitt=3000,burnin=500)
summary(mod3)
plot(mod3$VCV)

prior<-list(G=list(G1=list(V=diag(2), nu=3, alpha.mu=c(0,0), alpha.v=diag(2)*1000)),
            R=list(V=diag(2), nu=3))

mod4<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ year, #year as fixed
               ## RANDOM EFFECTS:
               ## us=unstructured, fits var/trait & covar traits
               ## us relaxes assumption of independence of groups
               ## us as both resp.vars measured for same inds
               random=~us(trait):id ,
               rcov=~us(trait):units, #RESID variance
               family=c("gaussian", "poisson"), #for each resp.var.
               data=data_sel,
               prior=prior,
               verbose = FALSE,nitt=3000,burnin=500)
summary(mod4) #One coefficient for each year, but how to get main effect?
plot(mod4$Sol)

#2: effect of FFD in the covariance - no relation among when you flower and when it is better?

prior<-list(G=list(G1=list(V=diag(2), nu=3, alpha.mu=c(0,0), alpha.v=diag(2)*1000),
                   G1=list(V=diag(2), nu=3, alpha.mu=c(0,0), alpha.v=diag(2)*1000)),
            R=list(V=diag(2), nu=3))

mod5<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ FFD,
               ## RANDOM EFFECTS:
               ## us=unstructured, fits var/trait & covar traits
               ## us relaxes assumption of independence of groups
               ## us as both resp.vars measured for same inds
               random=~us(trait):year+us(trait):id ,
               rcov=~us(trait):units, #RESID variance
               family=c("gaussian", "poisson"), #for each resp.var.
               data=data_sel,
               prior=prior,
               verbose = FALSE,nitt=3000,burnin=500)
summary(mod5) #Significant and positive effect of FFD (cannot use FFD_std)
plot(mod5$Sol)
#Repeat with mean FFD_std and mean seeds per year (n=22)
#and with GLMM

#timing / weather influences selection

#2: "selection source analysis" to see effects of "interactions" on selection/covariance

#3: Are differences among years due to differences in climate?

#4: yearly analyses MCMCglmm to see effects of different interactions
prior<-list(R=list(V=diag(2), nu=3))
mod_87<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
               rcov=~us(trait):units, family=c("gaussian", "poisson"), 
               data=subset(data_sel,year==1987),prior=prior,
               verbose = FALSE,nitt=3000,burnin=500)
mod_88<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==1988),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_89<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==1989),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_90<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==1990),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_91<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==1991),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_92<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==1992),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_93<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==1993),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_94<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==1994),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_95<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==1995),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_96<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==1996),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_06<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==2006),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_07<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==2007),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_08<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==2008),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_09<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==2009),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_10<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==2010),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_11<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==2011),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_12<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==2012),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_13<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==2013),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_14<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==2014),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_15<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==2015),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_16<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==2016),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)
mod_17<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                   cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl,
                 rcov=~us(trait):units, family=c("gaussian", "poisson"), 
                 data=subset(data_sel,year==2017),prior=prior,
                 verbose = FALSE,nitt=3000,burnin=500)

summary(mod_87)$solutions #Get coefficients and significances for each yr
summary(mod_88)$solutions
summary(mod_89)$solutions
summary(mod_90)$solutions
summary(mod_91)$solutions
summary(mod_92)$solutions
summary(mod_93)$solutions
summary(mod_94)$solutions
summary(mod_95)$solutions
summary(mod_96)$solutions
summary(mod_06)$solutions
summary(mod_07)$solutions
summary(mod_08)$solutions
summary(mod_09)$solutions
summary(mod_10)$solutions
summary(mod_11)$solutions
summary(mod_12)$solutions
summary(mod_13)$solutions
summary(mod_14)$solutions
summary(mod_15)$solutions
summary(mod_16)$solutions
summary(mod_17)$solutions

#5: interaction fixed factors:year

prior<-list(G=list(G1=list(V=diag(2), nu=3, alpha.mu=c(0,0), alpha.v=diag(2)*1000),
                   G1=list(V=diag(2), nu=3, alpha.mu=c(0,0), alpha.v=diag(2)*1000)),
            R=list(V=diag(2), nu=3))

mod6<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                 (cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl)*year,
               random=~us(trait):year+us(trait):id ,
               rcov=~us(trait):units, 
               family=c("gaussian", "poisson"), 
               data=data_sel,
               prior=prior,
               verbose = FALSE,nitt=3000,burnin=500)
summary(mod6) #One coefficient for each variable:year, but how to get main effect of the interaction?
 
#6: use fr set  and seed set instead of seeds per fl

mod7<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                 cumGDD5+grazing+prop_pred_seeds+n_fl_std+fr_set+seed_set,
               random=~us(trait):year+us(trait):id,
               rcov=~us(trait):units, 
               family=c("gaussian", "poisson"), 
               data=data_sel,
               prior=prior,
               verbose = FALSE,nitt=3000,burnin=500)
summary(mod7) #seed set is significant but fruit set is not





