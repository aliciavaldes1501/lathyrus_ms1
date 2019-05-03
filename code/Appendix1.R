temperature_wide<-gather(temperature[c(1:6,8,10)], variable, value,mean,min,max) %>%
  unite(var, variable, station) %>% 
  spread(var, value) #Convert to wide format with station variables
names(temperature_wide)<-c("date","year","month","day","max_O","max_S","mean_O","mean_S","min_O","min_S")

# Check for NAs
nrow(subset(temperature_wide,is.na(min_O)|is.na(min_S)))
nrow(subset(temperature_wide,is.na(mean_O)|is.na(mean_S)))
nrow(subset(temperature_wide,is.na(max_O)|is.na(max_S)))

nrow(subset(subset(temperature_wide,is.na(min_O)|is.na(min_S)),month==3|month==4|month==5))
nrow(subset(subset(temperature_wide,is.na(mean_O)|is.na(mean_S)),month==3|month==4|month==5))
nrow(subset(subset(temperature_wide,is.na(max_O)|is.na(max_S)),month==3|month==4|month==5))


unique(as.data.frame(temperature_wide%>%
                       group_by(year, month)%>%
                       filter(is.na(min_O)))[2:3])
unique(as.data.frame(temperature_wide%>%
                       group_by(year, month)%>%
                       filter(is.na(min_S)))[2:3])
unique(as.data.frame(temperature_wide%>%
                       group_by(year, month)%>%
                       filter(is.na(mean_O)))[2:3])
unique(as.data.frame(temperature_wide%>%
                       group_by(year, month)%>%
                       filter(is.na(mean_S)))[2:3])
unique(as.data.frame(temperature_wide%>%
                       group_by(year, month)%>%
                       filter(is.na(max_O)))[2:3])
unique(as.data.frame(temperature_wide%>%
                       group_by(year, month)%>%
                       filter(is.na(max_S)))[2:3])


# Models mean_S vs mean_O for each month
models_mean<-as.data.frame(temperature_wide %>% group_by(month) %>%
  do(models_mean=summary(lm(mean_S ~ mean_O, data = .)))%>%
  tidy(models_mean))
models_mean$sig<-ifelse(models_mean$p.value<0.001,"***",ifelse(models_mean$p.value<0.01,"**",ifelse(models_mean$p.value<0.05,"*","")))
models_mean

models_mean_rsquare<-as.data.frame(temperature_wide %>% group_by(month) %>%
                             do(models_mean=summary(lm(mean_S ~ mean_O, data = .)))%>%
                             glance(models_mean))[c(1,3)]
models_mean_rsquare

# Models min_S vs mi_O for each month
models_min<-as.data.frame(temperature_wide %>% group_by(month) %>%
                             do(models_min=summary(lm(min_S ~ min_O, data = .)))%>%
                             tidy(models_min))
models_min$sig<-ifelse(models_min$p.value<0.001,"***",ifelse(models_min$p.value<0.01,"**",ifelse(models_min$p.value<0.05,"*","")))
models_min

models_min_rsquare<-as.data.frame(temperature_wide %>% group_by(month) %>%
                                     do(models_min=summary(lm(min_S ~ min_O, data = .)))%>%
                                     glance(models_min))[c(1,3)]
models_min_rsquare

# Models max_S vs max_O for each month
models_max<-as.data.frame(temperature_wide %>% group_by(month) %>%
                             do(models_max=summary(lm(max_S ~ max_O, data = .)))%>%
                             tidy(models_max))
models_max$sig<-ifelse(models_max$p.value<0.001,"***",ifelse(models_max$p.value<0.01,"**",ifelse(models_max$p.value<0.05,"*","")))
models_max

models_max_rsquare<-as.data.frame(temperature_wide %>% group_by(month) %>%
                                     do(models_max=summary(lm(max_S ~ max_O, data = .)))%>%
                                     glance(models_max))[c(1,3)]
models_max_rsquare
