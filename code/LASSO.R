library(caret)
library(glmnet)


globmod_FFD<-lmer(FFD ~ max_3+max_4+max_5+mean_3+mean_4+mean_5+min_3+min_4+min_5+
                    precipitation_3+precipitation_4+precipitation_5+n_fl+(1|id),
                  data = subset1,REML=FALSE,na.action="na.fail")


# Predictor variables
x <- model.matrix(FFD~., subset1[c(1,3:15)])[,-1]
# Outcome variable
y <- subset1$FFD

glmnet(x, y, alpha = 1, lambda = NULL)

# Find the best lambda using cross-validation
set.seed(123) 
cv <- cv.glmnet(x, y, alpha = 1)
# Display the best lambda value
cv$lambda.min

# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)

library(glmmLasso)

model_1<-glmmLasso(fix=FFD ~ max_3+max_4+max_5+mean_3+mean_4+mean_5+min_3+min_4+min_5+
                     precipitation_3+precipitation_4+precipitation_5+n_fl, 
                   rnd=list(id=~1), data=subset1, lambda=10, family = gaussian(link="identity"), 
                   switch.NR=FALSE, final.re=FALSE, control = list())
model_1
summary(model_1)
coef(model_1)

model_2<-glmmLasso(fix=n_intact_seeds_rel ~ FFD_std+n_fl_std+
       FFD_std:max_3+FFD_std:max_4+FFD_std:max_5+
       FFD_std:mean_3+FFD_std:mean_4+FFD_std:mean_5+
       FFD_std:min_3+FFD_std:min_4+FFD_std:min_5+
       FFD_std:precipitation_3+FFD_std:precipitation_4+
       FFD_std:precipitation_5, rnd=list(id=~1),
     data = subset4,lambda=10, family = gaussian(link="identity"), 
     switch.NR=FALSE, final.re=T, control = list())
model_2
summary(model_2)
coef(model_2)

model_3<-glmmLasso(fix=n_intact_seeds_rel ~ FFD_std+n_fl_std+
                     FFD_std:min_3+FFD_std:min_4+FFD_std:min_5+
                     FFD_std:precipitation_3+FFD_std:precipitation_4+
                     FFD_std:precipitation_5, rnd=list(id=~1),
                   data = subset4,lambda=10, family = gaussian(link="identity"), 
                   switch.NR=FALSE, final.re=T, control = list())
summary(model_3)


# SIMPLE MODELS

# min

summary(lmer(FFD ~ min_3+min_4+min_5+precipitation_3+precipitation_4+precipitation_5+
               n_fl+(1|id),data = subset1,REML=FALSE,na.action="na.fail")) # All *

summary(lm(date_10~min_3+min_4+min_5+precipitation_3+precipitation_4+precipitation_5,
           data=mean_weather4)) # min_4, min_5 *

summary(lm(FFD_mean~min_3+min_4+min_5+precipitation_3+precipitation_4+precipitation_5,
           data=mean_weather4)) # min_4, min_5 *

summary(lm(date_90~min_3+min_4+min_5+precipitation_3+precipitation_4+precipitation_5,
           data=mean_weather4)) # min_4, min_5 *

summary(lm(days_90_10~min_3+min_4+min_5+precipitation_3+precipitation_4+precipitation_5,
           data=mean_weather4)) # All NS

summary(lmer(n_intact_seeds~min_3+min_4+min_5+precipitation_3+precipitation_4+precipitation_5+
               n_fl+(1|id),data = subset2,REML=FALSE,na.action="na.fail")) # min_5, prec_4 *

summary(lmer(n_intact_seeds_rel ~ FFD_std+FFD_std:min_3+FFD_std:min_4+FFD_std:min_5+
               FFD_std:precipitation_3+FFD_std:precipitation_4+FFD_std:precipitation_5+
               (1|id),data = subset3,REML=FALSE,na.action="na.fail")) # min_4, prec_3, prec_4 *



