library(climwin)

xvar = list(Temp = MassClimate$Temp) # Predictor variable
cdate = MassClimate$Date # Date for climate dataset 
bdate = Mass$Date # Date for biological dataset
# These date variables must be stored in a dd/mm/yyyy format.

baseline = lm(Mass ~ 1, data = Mass) # Define baseline model

cinterval = "day" # resolution

range = c(150, 0) # contains two values (upper and lower limit for tested climate windows)
# Here, testing all possible climate windows anywhere up to 150 days before the biological record

# Two types of climate windows:
# Relative --> the placement of the window will vary depending on the time of the biological response.
# They assume that each individual may have the same relative response to climate, 
# but the exact calendar dates will vary between individuals.
# Absolute --> We expect all individuals to respond to the same climatic period (e.g. average April temperature). 
# In this case, we assume that all climate windows will be calculated from an ‘absolute’ start date which will act as day 0.
# In this case, the additional parameter ‘refday’ will set the day and month of day 0 for the climate window analysis.

type = "absolute"
refday = c(20, 5) # May 20th

stat = "mean" #  aggregate statistics, such as mean, maximum, minimum or slope. Can it use several at one time?

func = "lin" 
# select from a range of possible relationships, including linear (“lin”), 
# quadratic (“quad”), cubic (“cub”), logarithmic (“log”) and inverse (“inv”)

# Test the linear effect of mean temperature on chick mass across all windows between 0 and 150 days before May 20th.

MassWin <- slidingwin(xvar = list(Temp = MassClimate$Temp),
                      cdate = MassClimate$Date,
                      bdate = Mass$Date,
                      baseline = lm(Mass ~ 1, data = Mass),
                      cinterval = "day",
                      range = c(150, 0),
                      type = "absolute", refday = c(20, 05),
                      stat = "mean",
                      func = "lin")

head(MassWin[[1]]$Dataset) # summary of results of all tested climate windows, ordered by ΔAICc 
# when calling the dataset we need to specify that we’re interested in the dataset from the first list item (i.e. [[1]]). 
# it is possible to test multiple combinations of parameter levels (e.g. climate variables), which will output multiple list items.
MassWin[[1]]$BestModel # relationship between temperature and mass within the strongest climate window
head(MassWin[[1]]$BestModelData) # raw climate and biological data used to fit BestModel

# Accounting for over-fitting

# re-run the climate window analyses on a dataset where any true climate signal has been removed. 
# The date information in the original dataset is randomly rearranged 
# to remove any association between climate and the biological response. 
# By running slidingwin on this new randomised dataset
# we can determine the likelihood of finding our original result by random chance.
MassRand <- randwin(repeats = 5, 
                    xvar = list(Temp = MassClimate$Temp),
                    cdate = MassClimate$Date,
                    bdate = Mass$Date,
                    baseline = lm(Mass ~ 1, data = Mass),
                    cinterval = "day",
                    range = c(150, 0),
                    type = "absolute", refday = c(20, 05),
                    stat = "mean",
                    func = "lin")

MassRand[[1]]

# the large negative deltaAICc values we observed in our slidingwin analysis 
# are unlikely to have occurred by chance. 

# However, to make this conclusion with more certainty we need to use the pvalue function.

pvalue(dataset = MassWin[[1]]$Dataset, datasetrand = MassRand[[1]], 
       metric = "C", 
       # Where the number of repeats used in randwin are high (100+) we can use the metric “AIC”. 
       # With a high number of repeats we can confidently determine the distribution of ΔAICc values
       # we would expect in a dataset with no climate signal present, 
       # we can then determine how likely our observed ΔAICc result from slidingwin would be in this distribution.
       # When the number of repeats is limited, we use the metric “C”. 
       # This estimates the distribution of ΔAICc using the information available from the limited number of repeats. 
       sample.size = 47) # The number of years of data used in the analysis, required when using the “C” metric. 
#  measure of confidence in our observed result  (our original slidingwin result is unlikely to be an issue of overfitting)

MassOutput <- MassWin[[1]]$Dataset
MassRand <- MassRand[[1]]
plothist(dataset = MassOutput, datasetrand = MassRand)

plotdelta(dataset = MassOutput)
plotweights(dataset = MassOutput)
plotbetas(dataset = MassOutput)
plotwin(dataset = MassOutput)

MassSingle <- singlewin(xvar = list(Temp = MassClimate$Temp),
                        cdate = MassClimate$Date,
                        bdate = Mass$Date,
                        baseline = lm(Mass ~ 1, data = Mass),
                        cinterval = "day",
                        range = c(72, 15), # start and end time of the single window
                        type = "absolute", refday = c(20, 5),
                        stat = "mean",
                        func = "lin")

plotbest(dataset = MassOutput,
         bestmodel = MassSingle$BestModel, 
         bestmodeldata = MassSingle$BestModelData)

plotall(dataset = MassOutput,
        datasetrand = MassRand,
        bestmodel = MassSingle$BestModel, 
        bestmodeldata = MassSingle$BestModelData)

##############################################################################################

# Try with our data

LatClim<-weather[c(1,5:7,8)]
LatClim$date<-format(LatClim$date,"%d/%m/%Y")

Lat<-subset(alldata[c(1,9)],!is.na(FFD_corr))
Lat$date<-as.Date(Lat$FFD_corr)
Lat$year<-as.factor(Lat$year)
Lat<-Lat%>%
  group_by(year)%>%
  summarize(meandate=mean.Date(date))%>%
  left_join(data_sel_agg[c(1,146)])

# New Lat data
Lat<-subset(alldata[c(1,3,9)],!is.na(FFD_corr))
Lat$date<-as.Date(Lat$FFD_corr)
Lat$year<-as.factor(Lat$year)
Lat<-Lat%>%left_join(data_sel[c(1:3,20,44:45)])
Lat<-subset(Lat,!is.na(FFD))

head(LatClim)
head(Lat)

# New FFDwin (with all individual FFD data, not with mean FFD)

FFDwin <- slidingwin(xvar = list(Temp = LatClim$mean),
                     cdate = LatClim$date,
                     bdate = Lat$date,
                     baseline = lm(FFD ~ 1, data = Lat),
                     cinterval = "day",
                     range = c(365, 0), # One year before
                     type = "absolute", refday = c(01,07), # July 1st
                     stat = "mean",
                     func = "lin")

# Test the linear effect of mean temperature on mean FFD across all windows between 0 and 150 days before May 20th.
FFDWin <- slidingwin(xvar = list(Temp = LatClim$mean),
                      cdate = LatClim$date,
                      bdate = Lat$meandate,
                      baseline = lm(FFD_mean ~ 1, data = Lat),
                      cinterval = "day",
                      range = c(365, 0), # One year before
                      type = "absolute", refday = c(01,07), # July 1st
                      stat = "mean",
                      func = "lin")

head(FFDWin[[1]]$Dataset) 
FFDWin[[1]]$BestModel 
head(FFDWin[[1]]$BestModelData)
summary(FFDWin[[1]]$BestModel) # Summary of best model! Cool! R square = 0.83

FFDRand <- randwin(repeats = 5, 
                   xvar = list(Temp = LatClim$mean),
                   cdate = LatClim$date,
                   bdate = Lat$meandate,
                   baseline = lm(FFD_mean ~ 1, data = Lat),
                   cinterval = "day",
                   range = c(365, 0), # One year before
                   type = "absolute", refday = c(01, 07), # July 1st
                   stat = "mean",
                   func = "lin")

FFDRand[[1]]

pvalue(dataset = FFDWin[[1]]$Dataset, datasetrand = FFDRand[[1]], 
       metric = "C", 
       sample.size = 22) 

FFDOutput <- FFDWin[[1]]$Dataset
FFDRand <- FFDRand[[1]]

plothist(dataset = FFDOutput, datasetrand = FFDRand)

plotdelta(dataset = FFDOutput)
plotweights(dataset = FFDOutput)
plotbetas(dataset = FFDOutput)
plotwin(dataset = FFDOutput)

FFDSingle <- singlewin(xvar = list(Temp = LatClim$mean),
                       cdate = LatClim$date,
                       bdate = Lat$meandate,
                       baseline = lm(FFD_mean ~ 1, data = Lat),
                       cinterval = "day",
                        range = c(97, 31), # start and end time of the single window
                       type = "absolute", refday = c(01, 07), # July 1st
                       stat = "mean",
                       func = "lin")

plotbest(dataset = FFDOutput,
         bestmodel = FFDSingle$BestModel, 
         bestmodeldata = FFDSingle$BestModelData)

plotall(dataset = FFDOutput,
        datasetrand = FFDRand,
        bestmodel = FFDSingle$BestModel, 
        bestmodeldata = FFDSingle$BestModelData)

# Multiple parameter combinations

## Temperature, mean, linear and quadratic

FFDWin2 <- slidingwin(xvar = list(Temp = LatClim$mean),
                      cdate = LatClim$date,
                      bdate = Lat$meandate,
                      baseline = lm(FFD_mean ~ 1, data = Lat),
                      cinterval = "day",
                      range = c(365, 0), # One year before
                      type = "absolute", refday = c(01, 07), # July 1st
                      stat = c("mean"),
                      func = c("lin", "quad"))

FFDWin2$combos # Quadratic is better, lower AICc

FFDOutput2 <- FFDWin2[[2]]$Dataset

plotdelta(dataset = FFDOutput2)
plotweights(dataset = FFDOutput2)
plotbetas(dataset = FFDOutput2)
plotwin(dataset = FFDOutput2)

FFDSingle2 <- singlewin(xvar = list(Temp = LatClim$mean),
                        cdate = LatClim$date,
                        bdate = Lat$meandate,
                        baseline = lm(FFD_mean ~ 1, data = Lat),
                        cinterval = "day",
                        range = c(98,31), # start and end time of the single window
                        type = "absolute", refday = c(01, 07), # July 1st
                        stat = "mean",
                        func = "quad")

plotbest(dataset = FFDOutput2,
         bestmodel = FFDSingle2$BestModel, 
         bestmodeldata = FFDSingle2$BestModelData)

plotall(dataset = FFDOutput2,
        datasetrand = FFDRand2,
        bestmodel = FFDSingle2$BestModel, 
        bestmodeldata = FFDSingle2$BestModelData)

# Explore model averaging?

## Temperature mean, and precipitation, sum, linear

FFDWin3 <- slidingwin(xvar = list(Temp = LatClim$mean, Prec = LatClim$precipitation),
                      cdate = LatClim$date,
                      bdate = Lat$meandate,
                      baseline = lm(FFD_mean ~ 1, data = Lat),
                      cinterval = "day",
                      range = c(365, 0), # One year before
                      type = "absolute", refday = c(01, 07), # July 1st
                      stat = c("mean","sum"),
                      func = c("lin"))


# Test slope?

# FFD,fitness (with mean of minimum temperatures)
Lat$climate <- 1

FFDwin4 <- slidingwin(xvar = list(Temp = LatClim$min),
                     cdate = LatClim$date,
                     bdate = Lat$date,
                     baseline = lm(n_intact_seeds_rel ~ climate*FFD_std, data = Lat),
                     cinterval = "day",
                     range = c(365, 0), # One year before
                     type = "absolute", refday = c(01,07), # July 1st
                     stat = "mean",
                     func = "lin")

head(FFDwin4[[1]]$Dataset) 
FFDwin4[[1]]$BestModel 
head(FFDwin4[[1]]$BestModelData)
summary(FFDwin4[[1]]$BestModel) # Summary of best model! Cool! R square = 0.83

FFDOutput4 <- FFDwin4[[1]]$Dataset

plotdelta(dataset = FFDOutput4)
plotweights(dataset = FFDOutput4)
plotbetas(dataset = FFDOutput4)
plotwin(dataset = FFDOutput4)

FFDSingle4 <- singlewin(xvar = list(Temp = LatClim$mean),
                       cdate = LatClim$date,
                       bdate = Lat$date,
                       baseline = lm(n_intact_seeds_rel ~ climate*FFD_std, data = Lat),
                       cinterval = "day",
                       range = c(52, 49), # start and end time of the single window
                       type = "absolute", refday = c(01, 07), # July 1st
                       stat = "mean",
                       func = "lin")

plotbest(dataset = FFDOutput,
         bestmodel = FFDSingle$BestModel, 
         bestmodeldata = FFDSingle$BestModelData)

plotall(dataset = FFDOutput,
        datasetrand = FFDRand,
        bestmodel = FFDSingle$BestModel, 
        bestmodeldata = FFDSingle$BestModelData)
     
# FFD,fitness (with mean of precipitation)
FFDwin5 <- slidingwin(xvar = list(Prec = LatClim$precipitation),
                      cdate = LatClim$date,
                      bdate = Lat$date,
                      baseline = lm(n_intact_seeds_rel ~ climate*FFD_std, data = Lat),
                      cinterval = "day",
                      range = c(365, 0), # One year before
                      type = "absolute", refday = c(01,07), # July 1st
                      stat = "mean",
                      func = "lin")

head(FFDwin5[[1]]$Dataset) 
FFDwin5[[1]]$BestModel 
head(FFDwin5[[1]]$BestModelData)
summary(FFDwin5[[1]]$BestModel) # Summary of best model! Cool! R square = 0.83

FFDOutput5 <- FFDwin5[[1]]$Dataset

plotdelta(dataset = FFDOutput5)
plotweights(dataset = FFDOutput5)
plotbetas(dataset = FFDOutput5)
plotwin(dataset = FFDOutput5)

FFDSingle5 <- singlewin(xvar = list(Temp = LatClim$mean),
                        cdate = LatClim$date,
                        bdate = Lat$date,
                        baseline = lm(n_intact_seeds_rel ~ climate*FFD_std, data = Lat),
                        cinterval = "day",
                        range = c(103, 103), # start and end time of the single window
                        type = "absolute", refday = c(01, 07), # July 1st
                        stat = "sum",
                        func = "lin")

plotbest(dataset = FFDOutput,
         bestmodel = FFDSingle$BestModel, 
         bestmodeldata = FFDSingle$BestModelData)

plotall(dataset = FFDOutput,
        datasetrand = FFDRand,
        bestmodel = FFDSingle$BestModel, 
        bestmodeldata = FFDSingle$BestModelData)

# Does not work well, is the problem the interaction on the model? or the mean/sum of precipitation?
# Try precipitation for FFD as response only

# Relative windows for each individual?



