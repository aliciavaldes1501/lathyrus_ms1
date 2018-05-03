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
               verbose = FALSE)
summary(mod1)







plot(mod1)

plot(mod1$Sol)
plot(mod1$VCV)

posterior.mode(mod1$Sol[, "cumGDD5"])
HPDinterval(mod1$Sol[, "cumGDD5"], 0.95)

################################################################################################

summary(glmer(round(n_intact_seeds_rel)~FFD*cumGDD5+(1|year),family="poisson",data=data_sel))
#Significant interaction!

summary(MCMCglmm(round(n_intact_seeds_rel) ~ FFD_std*cumGDD5,
         family=c("gaussian"),
         random=~year,prior=prior,
         data=data_sel,verbose=FALSE))

prior <- list(R = list(V = 1, nu = 0.002),G = list(G1 = list(V = 1e+08,fix = 1)))

summary(MCMCglmm(round(n_intact_seeds_rel) ~ FFD_std*year,
                 family=c("gaussian"),
                 data=data_sel,verbose=FALSE))
plot(MCMCglmm(round(n_intact_seeds_rel) ~ FFD_std*year,
                 family=c("gaussian"),
                 data=data_sel,verbose=FALSE))



