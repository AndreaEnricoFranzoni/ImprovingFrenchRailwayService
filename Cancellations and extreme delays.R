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
library(nlme)
library(corrplot)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)
library(robustbase)
rm(list=ls())
graphics.off()

data = read_excel('Data_by_year.xlsx')

n = dim(data)[1]

i_2015=which(data$year==2015)
i_2016=which(data$year==2016)
i_2017=which(data$year==2017)
i_2018=which(data$year==2018)
indexes=list(i_2015, i_2016, i_2017, i_2018)
i_national = which(data$service=='National')
i_international=which(data$service == 'International')

data$year = as.numeric(data$year)
col_years = rep('chartreuse2', n)
col_years[i_2016] = 'coral'
col_years[i_2017] = 'dodgerblue'
col_years[i_2018] = 'purple'

col_service = rep('chartreuse3', n)
col_service[i_international]='coral'

avg_delay_arr = data$avg_delay_all_arriving
avg_delay_dep = data$avg_delay_all_departing
avg_journey = data$journey_time_avg
num_trips=data$total_num_trips
year = as.factor(data$year)

#### Robust ####
avg_dep = data$avg_delay_all_departing
avg_arr = data$avg_delay_all_arriving
canc_perc = data$num_of_canceled_trains/data$total_num_trips
extreme_delay = data$num_greater_30_min_late/data$total_num_trips
# The latter 2 covariates have an impact on the economical status of the company
# Directly they are forced by law to refund, indirectly people are not keen to use trains again
# We assume (unrealistic) that there are no intermediate stations because we have no data on them
x_lim = c(0,0.13)
y_lim = c(0,0.15)

#2018
multiv_2018 = data.frame(cancelled_percentage=canc_perc[i_2018], extreme_delay=extreme_delay[i_2018], route=data$route[i_2018], time=data$journey_time_avg[i_2018])
N=dim(multiv_2018)[1]
fit_MCD_2018 = covMcd(x = multiv_2018[,1:2], alpha = .75, nsamp = 2000)
fit_MCD_2018$center

ind_best_subset_2018 = fit_MCD_2018$best
ind_best_subset_2018

ind_out_MCD_2018 = (1:N)[which(fit_MCD_2018$mcd.wt==0)]
ind_out_MCD_2018
multiv_2018$route[ind_out_MCD_2018]

x11()
plot(canc_perc[i_2018], extreme_delay[i_2018], col=ifelse(1:N%in%ind_out_MCD_2018,'red',"black"),pch=20,
     xlim=x_lim, ylim=y_lim, cex.lab=1.2, cex.axis=1.2, xlab='Proportion of cancellations', ylab='Proportion of trains with delay >30 min')
points(fit_MCD_2018$center[1], fit_MCD_2018$center[2],pch=4, col='coral', lwd=3, cex=1.5)
#text(multiv_2018$cancelled_percentage[ind_out_MCD_2018], multiv_2018$extreme_delay[ind_out_MCD_2018], label=multiv_2018$route[ind_out_MCD_2018])

# In particular, Paris Lyon - Perpignan (A/R) is the worst outlier (index 58, 62)


#2017
multiv_2017 = data.frame(cancelled_percentage=canc_perc[i_2017], extreme_delay=extreme_delay[i_2017], route=data$route[i_2017])
N=dim(multiv_2017)[1]
fit_MCD_2017 = covMcd(x = multiv_2017[,1:2], alpha = .75, nsamp = 2000)
fit_MCD_2017$center

ind_best_subset_2017 = fit_MCD_2017$best
ind_best_subset_2017

ind_out_MCD_2017 = (1:N)[which(fit_MCD_2017$mcd.wt==0)]
ind_out_MCD_2017
multiv_2017$route[ind_out_MCD_2017]


x11()
plot(canc_perc[i_2017], extreme_delay[i_2017], col=ifelse(1:n%in%ind_out_MCD_2017,'red',"black"),pch=20,
     xlim=x_lim, ylim=y_lim, cex.lab=1.2, cex.axis=1.2, xlab='Proportion of cancellations', ylab='Proportion of trains with delay >30 min')
points(fit_MCD_2017$center[1], fit_MCD_2017$center[2],pch=4, col='coral', lwd=3, cex=1.5)
#text(canc_perc[i_2017], extreme_delay[i_2017], label=1:N)

# In particular, Paris Lyon - Perpignan in this year is still an outlier, but with a low number of cancellations (index 38, 44)
# Lille Marseille (index 22, 29) no particular relation but they are also present as outlier in 2018, 2016, 2015
# Rennes Lyon (39, 89) almost same cancellations but different extreme events and they are also present as outlier in 2018, 2015 (only 1 in 2016)
# Montpellier Lyon (51, 88) 51 borderline but they are also present as outlier in 2018, 2016, 2015
# 33 Paris Lyon - Nice Ville (also the R as 94 has high delays) but they are also present as outlier in 2018, 2016, 2015
# 56 Paris Lyon - Toulon (also the R as 57 has high delays) but they are also present as outlier in 2018, 2016, 2015
# 91 Stuttgart - Paris Est
# Also strasbourg Paris Est and Francfort Paris Est has A/R (FF - PE also in 2016)
# MANY MANY ROUTES ARE A/R

#2016
multiv_2016 = data.frame(cancelled_percentage=canc_perc[i_2016], extreme_delay=extreme_delay[i_2016], route=data$route[i_2016])
N=dim(multiv_2016)[1]
fit_MCD_2016 = covMcd(x = multiv_2016[,1:2], alpha = .75, nsamp = 2000)
fit_MCD_2016$center

ind_best_subset_2016 = fit_MCD_2016$best
ind_best_subset_2016

ind_out_MCD_2016 = (1:N)[which(fit_MCD_2016$mcd.wt==0)]
ind_out_MCD_2016
multiv_2016$route[ind_out_MCD_2016]


x11()
plot(canc_perc[i_2016], extreme_delay[i_2016], col=ifelse(1:n%in%ind_out_MCD_2016,'red',"black"),pch=20,
     xlim=x_lim, ylim=y_lim, cex.lab=1.2, cex.axis=1.2, xlab='Proportion of cancellations', ylab='Proportion of trains with delay >30 min')
points(fit_MCD_2016$center[1], fit_MCD_2016$center[2],pch=4, col='coral', lwd=3, cex=1.5)
#text(canc_perc[i_2016], extreme_delay[i_2016], label=1:N)

# In that case also positive outliers are detected: in particular, no cancellation routes (da individuare)
# Paris Lyon - Perpignan (93, 105) has same levels of 2017
# 75 (montparnasse tolouse)
# 28 - 96(Paris Lyon Nice Ville A/R) 
# 40 - 66 (Lyon Montpellier A/R) 
# 32 (Toulon Paris Lyon)

#2015
multiv_2015 = data.frame(cancelled_percentage=canc_perc[i_2015], extreme_delay=extreme_delay[i_2015], route=data$route[i_2015])
N=dim(multiv_2015)[1]
fit_MCD_2015 = covMcd(x = multiv_2015[,1:2], alpha = .75, nsamp = 2000)
fit_MCD_2015$center

ind_best_subset_2015 = fit_MCD_2015$best
ind_best_subset_2015

ind_out_MCD_2015 = (1:N)[which(fit_MCD_2015$mcd.wt==0)]
ind_out_MCD_2015
multiv_2015$route[ind_out_MCD_2015]


x11()
plot(canc_perc[i_2015], extreme_delay[i_2015], col=ifelse(1:n%in%ind_out_MCD_2015,'red',"black"),pch=20,
     xlim=x_lim, ylim=y_lim, cex.lab=1.2, cex.axis=1.2, xlab='Proportion of cancellations', ylab='Proportion of trains with delay >30 min')
points(fit_MCD_2015$center[1], fit_MCD_2015$center[2],pch=4, col='coral', lwd=3, cex=1.5)
#text(canc_perc[i_2015], extreme_delay[i_2015], label=1:N)

# Paris Lyon - Perpignan (8, 104) has same levels of 2017, 2016 (not critical but outliers)
# 77 - 65 (Paris Lyon - Annecy) high cancellations for the 2015
# 102 (Nice Ville - Paris Lyon)


# For critical routes that also emerge from cause analysis, we can give a motivation on their problems

# Robust location estimate
# Cancellations   Extreme delays
# 0.0017          0.026
# 0.016           0.034
# 0.0077          0.04
# 0.08            0.048
# Increasing delays trend, but in particular very big cancellations in 2018 (scioperi contro la liberalizzazione di SNCF)
# va approfondita con anova o qualcosa del genere
data_outlier = matrix(NA, nrow=108, ncol=7)
data_outlier[,1]= data$route[1:108]
data_outlier[,6]= data$departure_station[1:108]
data_outlier[,7]= data$arrival_station[1:108]
for(i in 1:108){
  if(data_outlier[i,1] %in% multiv_2018$route[ind_out_MCD_2018])
    data_outlier[i,5]=1
  else data_outlier[i,5]=0
}  
for(i in 1:108){
  if(data_outlier[i,1] %in% multiv_2017$route[ind_out_MCD_2017])
    data_outlier[i,4]=1
  else data_outlier[i,4]=0
}  
for(i in 1:108){
  if(data_outlier[i,1] %in% multiv_2016$route[ind_out_MCD_2016])
    data_outlier[i,3]=1
  else data_outlier[i,3]=0
}  
for(i in 1:108){
  if(data_outlier[i,1] %in% multiv_2015$route[ind_out_MCD_2015])
    data_outlier[i,2]=1
  else data_outlier[i,2]=0
}
data_outlier=data.frame(data_outlier)
colnames(data_outlier)=c("Route", 2015, 2016,2017,2018, "Dep", "Arr")
route_outlier=data_outlier[-which(data_outlier$'2015'==0 & data_outlier$'2016'==0 & data_outlier$'2017'==0 & data_outlier$'2018'==0),]
#railway_outlier = route_outlier[which(route_outlier$Dep %in%route_outlier$Arr & route_outlier$Arr %in% route_outlier$Dep),]
