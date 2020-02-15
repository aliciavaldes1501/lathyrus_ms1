library(lme4)
library(effects)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

# Define a theme for ggplot (if you want to! I like this clean theme)

my_theme <- function(){
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
    theme(legend.position="none")+theme(text=element_text(family="serif"))+
    theme(plot.title = element_text(hjust =-0.06))
}

# Fit a model

modsel_sig<-lmer(n_intact_seeds_rel ~ FFD_std+n_fl_std+FFD_std:min_4+FFD_std:precipitation_3+
                   FFD_std:precipitation_4+(1|id),
                 data = subset4,REML=FALSE,na.action="na.fail")

# Effect of the interaction "FFD_std:min_4"

interaction1<-data.frame(effect(term="FFD_std:min_4",mod=modsel_sig,
                                xlevels=list(FFD_std=seq(-3.9,4.2,0.1),
                                             min_4=seq(-0.1,3.7,0.1)))) # I gave levels for the x variables within the range of my values
                                                                        # If you increase/decrease the increment of the sequence,
                                                                        # you can get less/more lines

# Define a color palette (you can use different palettes, see http://www.colorbrewer.org/)

myPalette <- colorRampPalette(brewer.pal(9, "YlOrRd"))

# Plot the graph

ggplot(interaction1, aes(FFD_std,fit, group = as.factor(min_4)))+
  geom_smooth(method=lm,se=F,size=0.5,aes(FFD_std,fit,color=min_4))+
  xlab("Standardized first flowering date")+ylab("Relative number of intact seeds")+
  my_theme()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+labs(colour="Minimum daily\ntemperature April (ºC)")+
  scale_y_continuous(limit=c(0,3.2))
