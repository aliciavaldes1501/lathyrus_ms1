# Packages that I used:
library(dplyr)
library(broom)
library(tidyr)

# I am sure you can do this in many other ways, using other packages!

# For each year, I perform a model regressing absolute fitness (n_intact_seeds) 
# on my trait of interest (FFD) and my condition trait (n_fl)
models_fit_FFD_nfl<-data.frame(data_sel %>% group_by(year) %>%
                                 do(model = lm(n_intact_seeds~FFD+n_fl, data = .)) %>% tidy(model))

# I extract the slopes from the previous models
coefs_abs_fit<-spread(subset(models_fit_FFD_nfl,!term=="(Intercept)")[1:3],term,estimate)

names(coefs_abs_fit)<-c("year","coef_FFD","coef_n_fl") 
# For each year, I have the slope of FFD (coef_FFD --> this is the estimate of the trait-fitness relationship)
# and the slope for n_fl (coef_n_fl, which we do not use)

# I merge these values with another dataset where I have the yearly selection gradients, 
# as well as yearly estimates of trait means, trait variances and fitness means
new_data<-merge(merge(data_sel_agg[c(1,146:147,156)], # This is the other dataset
                      as.data.frame(data_sel%>%
                                      group_by(year)%>%
                                      summarise(mean_abs_fitness=mean(n_intact_seeds)))),coefs_abs_fit)

# This is how the data look like:
# > head(new_data)
#   year FFD_mean   FFD_var  selgradFFD mean_abs_fitness   coef_FFD   coef_n_fl
# 1 1987 66.25589 16.699234 -0.07790572         7.725935 -0.1472896  0.46045083
# 2 1988 59.90789 20.244857 -0.08781589         4.183819 -0.0816561  0.17117359
# 3 1989 53.85571 18.807595 -0.14412079         8.683673 -0.2885782  0.52014644
# 4 1990 54.46244 26.093643 -0.27608505         3.517934 -0.1941245  0.25286275
# 5 1991 64.99514 36.445531 -0.32057018         7.302221 -0.3963606  0.36578204
# 6 1992 59.85048  9.975637 -0.46283867         1.190378 -0.1744393 -0.01199987

# And what the variable names mean:
# FFD_mean = yearly estimates of trait means
# FFD_var = yearly estimates of trait variances
# selgradFFD = selection estimates (yearly selection gradients) 
# mean_abs_fitness yearly estimates of fitness means
# coef_FFD = yearly estimates of trait-fitness relationships (calculated above)

### Fitting the models ###

model5<-lm(selgradFFD~FFD_mean,data=new_data)
model6<-lm(selgradFFD~FFD_var,data=new_data)
model7<-lm(selgradFFD~mean_abs_fitness,data=new_data)
model8<-lm(selgradFFD~coef_FFD,data=new_data)

summary(model5)
summary(model6)
summary(model7)
summary(model8)

# To assess how much of the among-year variation in selection is explained by each of the four components,
# we look at the Adjusted R-squared from the model summaries
