library(MCMCglmm)
prior<-list(G=list(G1=list(V=diag(2), nu=3, alpha.mu=c(0,0), alpha.v=diag(2)*1000)),
            R=list(V=diag(2), nu=3))
mod1<-MCMCglmm(cbind(FFD_std, round(n_intact_seeds_rel)) ~ 
                 cumGDD5+grazing+prop_pred_seeds+n_fl_std+seeds_per_fl - 1,
               ## RANDOM EFFECTS:
               ## us=unstructured, fits var/trait & covar traits
               ## us relaxes assumption of independence of groups
               ## us as both resp.vars measured for same inds
               random=~us(trait):id ,
               rcov=~us(trait):units, #RESID variance
               family=c("gaussian", "poisson"), #for each resp.var.
               data=data_sel,
               prior=prior,
               verbose = FALSE)
summary(mod1)
plot(mod1)

plot(mod1$Sol)

