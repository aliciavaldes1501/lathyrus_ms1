ggplot(alldata, aes(x = FFD, y = as.factor(year))) +
geom_density_ridges(fill="red",alpha = .5)+
theme_ridges()+ ylab("Year")+xlab("First flowering date")+
theme(text = element_text(size=30))+
theme(axis.text = element_text(size=20), axis.title = element_text(size=20))+
geom_density_ridges(quantile_lines=T,quantiles=4)+
geom_point(data=group_by(alldata,year)%>%summarize(mean=mean(FFD,na.rm=T)),aes(x=mean,y=year),size=2,color="black")



cdat <- ddply(alldata, "year", summarise, FFD.mean=mean(FFD,na.rm=T))
cdat1 <- ddply(alldata, "year", summarise, FFD.25=quantile(FFD,na.rm=T,probs=0.25))
cdat2 <- ddply(alldata, "year", summarise, FFD.50=quantile(FFD,na.rm=T,probs=0.50))
cdat3 <- ddply(alldata, "year", summarise, FFD.75=quantile(FFD,na.rm=T,probs=0.75))

ggplot(alldata, aes(x = FFD)) + 
  geom_density(colour="black", fill="white") + 
  facet_wrap( ~ year, ncol=4,scales="free_y")+
  geom_vline(data=cdat, aes(xintercept=FFD.mean),size=0.5, colour="red")+
  geom_vline(data=cdat1, aes(xintercept=FFD.25),size=0.5, colour="blue",lty="dashed")+
  geom_vline(data=cdat2, aes(xintercept=FFD.50),size=0.5, colour="blue")+
  geom_vline(data=cdat3, aes(xintercept=FFD.75),size=0.5, colour="blue",lty="dashed")


