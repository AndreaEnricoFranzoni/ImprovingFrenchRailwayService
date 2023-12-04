library(readxl)
library(stats)
library(parallel)
library(pbapply)
library(ISLR2)
library(car)
library(np)
library(splines)
library(fda)
library(magrittr)
library(KernSmooth)
library(rgl)
library(mgcv)
rm(list=ls())
graphics.off()

data = read_excel('aggregated_trains_by_year.xlsx')
# We remove an evident outlier
data=data[-which(data$year==2017 & data$route=='PARIS LYON - GRENOBLE'),]
n = dim(data)[1]

data$year = as.numeric(data$year)
col_years = rep('chartreuse2', n)
col_years[which(data$year==2016)] = 'coral'
col_years[which(data$year==2017)] = 'dodgerblue'
col_years[which(data$year==2018)] = 'purple'

# per ora consideriamo questa
x11()
par(mfrow=c(1,2))
plot(data$avg_delay_all_departing, data$avg_delay_all_arriving, col=col_years, pch=16)
abline(a=0, b=1, col='cyan')
plot(data$journey_time_avg, data$avg_delay_all_arriving, col=col_years, pch=16)


model_param = lm(data$avg_delay_all_arriving ~ data$avg_delay_all_departing + data$journey_time_avg +
                   as.factor(data$year))
summary(model_param)

x11()
par(mfrow=c(2,2))
plot(model_param)

shapiro.test(model_param$residuals)

# si potrebbe usare come covariata anche il numero totale di viaggi

##### local regression ####
m_loc = npreg(avg_delay_all_arriving ~ avg_delay_all_departing + journey_time_avg ,
              ckertype = 'uniform',
              bws = c(0.5, 0.5), # bandwidth
              data = data)
summary(m_loc)

#### GAM with interaction #####
avg_delay_arr = data$avg_delay_all_arriving
avg_delay_dep = data$avg_delay_all_departing
avg_journey = data$journey_time_avg
year = as.factor(data$year)

# summary(model_gam)
# avg_delay_dep.grid=seq(range(avg_delay_dep)[1],range(avg_delay_dep)[2],length.out = 100)
# avg_journey.grid=seq(range(avg_journey)[1],range(avg_journey)[2],length.out = 100)
# grid=expand.grid(avg_delay_dep.grid,avg_journey.grid)
# names(grid)=c('Delay departure','Journey time')
# pred=predict(first_model,newdata=grid) 
# x11()
# persp3d(avg_delay_dep.grid,avg_journey.grid,pred,col='blue',border="black",lwd=0.3)
# points3d(avg_delay_dep,avg_journey,avg_delay_arr,col='black',size=5)

model_gam = gam(data$avg_delay_all_arriving ~ s(data$avg_delay_all_departing, bs = 'cr') + 
                        s(data$journey_time_avg, bs ='cr') +
                       as.factor(data$year))

model_gam = gam(avg_delay_arr ~ s(avg_delay_dep, bs = 'cr') + s(avg_journey, bs ='cr') + s(year, bs='fs') )

summary(model_gam)

x11()
par(mfrow=c(1, 3))
plot(model_gam)

x11()
plot(model_gam$residuals)


model_gam_inter = gam(avg_delay_arr ~ s(avg_delay_dep, avg_journey, bs = 'tp', m=2) + s(year, bs='fs')  )
model_gam_inter = gam(avg_delay_arr ~ s(avg_delay_dep, by=year, bs = 'cr') + s(avg_journey, by=year, bs ='cr') )
summary(model_gam_inter)
x11()
par(mfrow=c(4, 2))
plot(model_gam_inter)

x11()
plot(model_gam_inter$residuals)

# Comparing residuals
mu.res = mean(model_gam_inter$residuals)
s.res = sd(model_gam_inter$residuals)
x = as.matrix(model_gam_inter$residuals)
y = as.matrix( rnorm(length(model_gam_inter$residuals), mu.res, sqrt(s.res)) ) 
DepthProc::ddPlot(x, y,depth_params = list(method='Tukey'))

shapiro.test(model_gam_inter$residuals)

# Semi parametric model
model_gam_red = gam(avg_delay_arr ~ s(avg_delay_dep, bs = 'cr') + avg_journey + s(year, bs='fs') )

summary(model_gam_red)

anova(model_gam_red,model_gam, test = "F")

x11()
par(mfrow=c(1, 3))
plot(model_gam)

# ANOVA sulla differenza della media nei 4 anni

# IC per ritardo all'arrivo nei 4 anni
# IC per ritardo all'arrivo-ritardo alla partenza nei 4 anni



graphics.off()
