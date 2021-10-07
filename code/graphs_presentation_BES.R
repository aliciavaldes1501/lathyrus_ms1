library(ggplot2)
library(ggridges)
library(ggthemes)
library(tidyverse)
library(ggeffects)
library(glmmTMB)
library(gridExtra)
library(grid)
library(RColorBrewer)

my_theme <- function(){
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
    theme(legend.position="none")+theme(text=element_text(family="serif"))+
    theme(plot.title = element_text(hjust =-0.06))
}
my_theme_legend <- function(){
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
    theme(text=element_text(family="serif"))+
    theme(plot.title = element_text(hjust =-0.06))
}
myPalette <- colorRampPalette(brewer.pal(8, "BuPu"))

ggplot(data_selag,aes(x=FFD,y=year,group=year))+
  geom_density_ridges(fill = "purple",alpha=0.6)+my_theme()+
  ylab("Year")+xlab("First flowering date")+
  scale_y_continuous(breaks=c(1987,1988,1989,1990,1991,1992,1993,1994,1996,
                              2006,2007,2008,2009,2010,2011,2012,2013,2014,
                              2015,2016,2017))
ggsave(filename=
         "phenology_distr_years.tiff",
       device="tiff",width=10,height=15,units="cm",dpi=300,compression="lzw")  

weather<-read.table("C:/Users/user/Dropbox/SU/Projects/lathyrus/lathyrus_ms3/data/weather.csv",
                    header=T,sep="\t",dec=".") 

weather%>%
  filter(year>1986)%>%
  filter(month==3|month==4|month==5|month==6)%>%
  group_by(year,month)%>%
  summarise(meantemp=mean(mean))%>%
  ggplot(.,aes(x=year,y=meantemp,color=factor(month)))+geom_line(size=1)+
  xlab("Year")+ylab("Temperature (ºC)")+my_theme()+
  scale_x_continuous(breaks=seq(1987,2017,5))+
  scale_colour_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442"))
ggsave(filename=
         "spring_temps.tiff",
       device="tiff",width=10,height=5.3,units="cm",dpi=300,compression="lzw")  

# Grazing
fig1a<-grid.arrange(
  # Grazing <- Temperature March
  ggpredict(globmod_grazing1_A_ns,terms = "mean_3 [all]")%>%
    ggplot(aes(x,predicted))+geom_ribbon(aes(ymin=conf.low,ymax=conf.high),fill="purple",alpha=0.4)+
    geom_line(color="black",size=1)+
    my_theme()+xlab(NULL)+ylab("Grazing")+
    labs(subtitle = "March")+theme(plot.subtitle = element_text(hjust = 0.5,margin=margin(t=10,b=-20))),
  # Grazing <- Temperature April
  ggpredict(globmod_grazing1_A_ns,terms = "mean_4 [all]")%>%
    ggplot(aes(x,predicted))+geom_ribbon(aes(ymin=conf.low,ymax=conf.high),fill="purple",alpha=0.4)+
    geom_line(color="black",size=1)+
    my_theme()+xlab(NULL)+ylab(NULL)+
    labs(subtitle = "April")+theme(plot.subtitle = element_text(hjust = 0.5,margin=margin(t=10,b=-20))),
  # Grazing <- Temperature May
  ggpredict(globmod_grazing1_A_ns,terms = "mean_5 [all]")%>%
    ggplot(aes(x,predicted))+geom_ribbon(aes(ymin=conf.low,ymax=conf.high),fill="purple",alpha=0.4)+
    geom_line(color="black",size=1)+
    my_theme()+xlab(NULL)+ylab(NULL)+
    labs(subtitle = "May")+theme(plot.subtitle = element_text(hjust = 0.5,margin=margin(t=10,b=-20))),
  ncol=3,bottom=textGrob("Temperature (ºC)",gp=gpar(fontsize=20,fontfamily="serif")))
ggsave(filename="grazing.tiff",
       plot=fig1a,device="tiff",width=30,height=9,units="cm",dpi=300,compression="lzw")

# N seeds per flower
ggpredict(globmod_seedsperfl_B_ns,terms = "FFD_sd [all]")%>%
  ggplot(aes(x,predicted))+geom_ribbon(aes(ymin=conf.low,ymax=conf.high),fill="purple",alpha=0.4)+
  geom_line(color="black",size=1)+
  my_theme()+xlab("Standard deviation of FFD")+ylab("N seeds per flower")+
  theme(plot.title=element_text(hjust=-0.15,vjust=1))
ggsave(filename="nseedsfl_1.tiff",device="tiff",width=8,height=7,units="cm",dpi=300,compression="lzw")

ggpredict(globmod_seedsperfl_A_ns,terms = "mean_5 [all]")%>%
  ggplot(aes(x,predicted))+geom_ribbon(aes(ymin=conf.low,ymax=conf.high),fill="purple",alpha=0.4)+
  geom_line(color="black",size=1)+
  my_theme()+xlab("Temperature (ºC)")+ylab("N seeds per flower")+
  theme(plot.title=element_text(hjust=0,vjust=1))+
  labs(subtitle = "May")+theme(plot.subtitle = element_text(hjust = 0.5,margin=margin(t=10,b=-20)))
ggsave(filename="nseedsfl_2.tiff",device="tiff",width=8,height=7,units="cm",dpi=300,compression="lzw")

# Seed predation
fig2<-grid.arrange(
  ggpredict(globmod_pred_GLM1_B_ns,terms = "FFD_mean [all]")%>%
    ggplot(aes(x,predicted))+geom_ribbon(aes(ymin=conf.low,ymax=conf.high),fill="purple",alpha=0.4)+
    geom_line(color="black",size=1)+
    my_theme()+xlab("Mean FFD")+ylab("Seed predation")+
    theme(plot.title=element_text(hjust=-0.15,vjust=1)),
  ggpredict(globmod_pred_GLM1_B_ns,terms = "FFD_sd [all]")%>%
    ggplot(aes(x,predicted))+geom_ribbon(aes(ymin=conf.low,ymax=conf.high),fill="purple",alpha=0.4)+
    geom_line(color="black",size=1)+
    my_theme()+xlab("Standard deviation of FFD")+ylab(NULL)+
    theme(plot.title=element_text(hjust=-0.15,vjust=1)),
  ncol=2)
ggsave(filename=
         "seedpred_1.tiff",
       plot=fig2,device="tiff",width=16,height=7,units="cm",dpi=300,compression="lzw")

fig1c<-grid.arrange(
  # Predation <- Temperature March
  ggpredict(globmod_pred_GLM1_A_ns,terms = "mean_3 [all]")%>%
    ggplot(aes(x,predicted))+geom_ribbon(aes(ymin=conf.low,ymax=conf.high),fill="purple",alpha=0.4)+
    geom_line(color="black",size=1)+
    my_theme()+xlab(NULL)+ylab("Seed predation")+
    labs(subtitle = "March")+theme(plot.subtitle = element_text(hjust = 0.5,margin=margin(t=10,b=-20))),
  # Predation <- Temperature May
  ggpredict(globmod_pred_GLM1_A_ns,terms = "mean_5 [all]")%>%
    ggplot(aes(x,predicted))+geom_ribbon(aes(ymin=conf.low,ymax=conf.high),fill="purple",alpha=0.4)+
    geom_line(color="black",size=1)+
    my_theme()+xlab(NULL)+ylab(NULL)+
    labs(subtitle = "May")+theme(plot.subtitle = element_text(hjust = 0.5,margin=margin(t=10,b=-20))),
  # Predation <- Temperature June
  ggpredict(globmod_pred_GLM1_A_ns,terms = "mean_6 [all]")%>%
    ggplot(aes(x,predicted))+geom_ribbon(aes(ymin=conf.low,ymax=conf.high),fill="purple",alpha=0.4)+
    geom_line(color="black",size=1)+
    my_theme()+xlab(NULL)+ylab(NULL)+
    labs(subtitle = "June")+theme(plot.subtitle = element_text(hjust = 0.5,margin=margin(t=10,b=-20))),
  ncol=3,bottom=textGrob("Temperature (ºC)",gp=gpar(fontsize=20,fontfamily="serif")))
ggsave(filename=
         "seedpred_2.tiff",
       plot=fig1c,device="tiff",width=24,height=7,units="cm",dpi=300,compression="lzw")

# Individual FFD
# Fig. 3: Individual FFD
fig3<-grid.arrange(
  # Grazing <- Individual FFD
  ggpredict(globmod_grazing1_A_ns,terms = "FFD_s_y [all]")%>%
    ggplot(aes(x,predicted))+geom_ribbon(aes(ymin=conf.low,ymax=conf.high),fill="purple",alpha=0.4)+
    geom_line(color="black",size=1)+
    my_theme()+xlab(NULL)+ylab("Grazing"),
  ggpredict(globmod_seedsperfl_A_ns,terms = "FFD_s_y [all]")%>%
    ggplot(aes(x,predicted))+geom_ribbon(aes(ymin=conf.low,ymax=conf.high),fill="purple",alpha=0.4)+
    geom_line(color="black",size=1)+
    my_theme()+xlab(NULL)+ylab("N seeds per flower"),
  # Predation <- Temperature April * FFD
  ggpredict(globmod_pred_GLM1_A_ns,terms = c("FFD_s_y [all]","mean_4 [all]"))%>%
    ggplot(aes(x,predicted, ymin = conf.low, ymax = conf.high, colour = group, fill = group))+
    geom_line(aes(color=as.numeric(as.character(group))),size=0.6) + 
    scale_colour_gradientn(colours = myPalette(100)) +
    my_theme()+xlab(NULL)+ylab("Seed predation")+
    theme(legend.justification=c(1.1,0.99),legend.position=c(1.1,0.99),
          legend.direction="horizontal")+labs(colour="April temperature")+
    guides(colour = guide_colourbar(title.position="top",title.hjust = 7,barheight=0.7))+
    theme(legend.background = element_rect(fill=alpha('blue', 0))),
  ncol=3,bottom=textGrob("First flowering date",just="center",hjust=0.45,
                         gp=gpar(fontsize=16,fontfamily="serif")))
ggsave(filename=
         "indivFFD.tiff",
       plot=fig3,device="tiff",width=30,height=9,units="cm",dpi=300,compression="lzw")

# selection
ggpredict(mod_int_sel_GLMM_nsints,terms = c("FFD_s_y [all]","grazing_mean [0:0.8 by=.1]"))%>%
  ggplot(aes(x,predicted,colour=group,fill=group))+
  geom_line(aes(color=as.numeric(as.character(group))),size=1)+my_theme()+
  scale_colour_gradientn(colours = myPalette(100)) +
  theme(legend.position="right")+labs(colour="Grazing")+
  xlab("First flowering date")+ylab("Fitness (N intact seeds)")
ggsave(filename=
         "selection.tiff",
       device="tiff",width=12,height=9,units="cm",dpi=300,compression="lzw")



