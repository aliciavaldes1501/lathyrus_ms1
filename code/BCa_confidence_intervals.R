library(boot)
library(car)

model1<-lm(grazing ~ FFD_mean,data=data_sel_agg)
summary(model1)

model1.boot <- Boot(model1, f=coef,R=1999)
summary(model1.boot, high.moments=TRUE)
confint(model1.boot, level=.95, type="bca")
