library(interactions)

summary(lmer(FFD ~ (mean_4+mean_5)*(precipitation_3+precipitation_4)+n_fl+(1|id),
             data = subset1,REML=FALSE,na.action="na.fail"))

summary(lm(date_10~(scale(mean_4)+scale(mean_5))*
             (scale(precipitation_3)+scale(precipitation_4)),data=mean_weather4))

summary(lm(FFD_mean~(scale(mean_4)+scale(mean_5))*
             (scale(precipitation_3)+scale(precipitation_4)),data=mean_weather4))

summary(lm(date_90~(scale(mean_4)+scale(mean_5))*
             (scale(precipitation_3)+scale(precipitation_4)),data=mean_weather4))

interact_plot(lm(FFD_mean~(scale(mean_4)+scale(mean_5))*
                   (scale(precipitation_3)+scale(precipitation_4)),data=mean_weather4), 
              pred = mean_4, modx = precipitation_4)

summary(lmer(n_intact_seeds_rel ~ FFD_std+
       FFD_std:min_4*
       (FFD_std:precipitation_3+FFD_std:precipitation_4)+
       (1|id),
     data = subset3,REML=FALSE,na.action="na.fail")) #Interaction NS

summary(lmer(n_intact_seeds_rel ~ FFD_std+n_fl_std+
      FFD_std:min_4*
       (FFD_std:precipitation_3+FFD_std:precipitation_4)+(1|id),
     data = subset4,REML=FALSE,na.action="na.fail"))




