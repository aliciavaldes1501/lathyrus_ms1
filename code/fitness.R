summary(lmer(n_intact_seeds ~ precipitation_5+
               n_fl+(1|id),data = subset2,REML=FALSE,na.action="na.fail"))

# Variables to use
subset2<-data_sel[c(3,20,42,47,56,158:160,170:172,182:184,194:196)]
subset2[,c(3:17)]<-scale(subset2[,c(3:17)])
globmod_fitness<-lmer(n_intact_seeds ~ max_3+max_4+max_5+mean_3+mean_4+mean_5+
                        min_3+min_4+min_5+precipitation_3+precipitation_4+precipitation_5+
                        FFD_mean+days_90_10+n_fl+(1|id),data = subset2,REML=FALSE,na.action="na.fail")

# Excluding collinear variables with r > 0.5
smat2 <- abs(cor(subset2[, -c(1:3)])) <= .5 # TRUE: cor<=0.5,FALSE: cor>0.5
smat2[!lower.tri(smat2)] <- NA

clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust1 <- try(makeCluster(getOption("cl.cores", 3), type = clusterType))
clusterExport(clust1, "subset2")
clusterEvalQ(clust1, library(lme4))
modsel_fitness<-pdredge(globmod_fitness,subset=smat2,fixed="n_fl",cluster=clust1)

summary(model.avg(modsel_fitness,subset=delta<2)) # Summary averaged model - only one
importance(modsel_fitness) # Variable importance

summary(get.models(modsel_fitness,subset=1)$"6165")
r.squaredGLMM(get.models(modsel_fitness,subset=1)$"6165") #R square of best model
