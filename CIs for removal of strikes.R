#### Libraries ####
library(dplyr)
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
rm(list=ls())
graphics.off()
set.seed(21100)

#### Data preprocessing ####
data = read_excel('trains_update_2610.xlsx')
data= data[-which(data$avg_delay_all_arriving<(-30)),]
data=data[,-c(9,13,17)]
n=dim(data)[1]
n
p=dim(data)[2]
p

names(data)
na_indeces=NULL
for(i in 1:n)
  if(any(is.na(data[i,])))
    na_indeces=c(na_indeces, i)                                               # we save indices for the rows having missing values in the dataset
na_routes=data[na_indeces,]
temp = count(na_routes, na_routes$route)
routes_missing_values = temp$`na_routes$route`[which(temp$n>=7)]                # we save name of routes with at least 7 missing values (our tolerance)

temp = count(data, data$route)
truncated_routes = temp$`data$route`[which(temp$n <40)]                         # we save name of routes with less than 40 occurences (meaning that they were unobserved in at least 7 dates)
truncated_routes

nas = unique(c(routes_missing_values,truncated_routes))                         # we merge together the route names
#data=na.omit(data)

for(i in 1:length(nas))
  data=data[-which(data$route ==nas[i]),]

n = dim(data)[1]

data$year = as.numeric(data$year)
i_2015=which(data$year==2015)
i_2016=which(data$year==2016)
i_2017=which(data$year==2017)
i_2018=which(data$year==2018)
indexes=list(i_2015, i_2016, i_2017, i_2018)

data_2018 = data[i_2018,]
data_2015 = data[i_2015,]
data_2016 = data[i_2016,]
data_2017 = data[i_2017,]

#### Confidence intervals for the median of the % of the cancelled trains ####
#### 2018 ####
CI_18 = matrix(NA, nrow=11, ncol=2)
for(month in 1:11){
i=1
sample1=rep(NA, 108)
for (route in unique(data_2018$route)){
  sample1[i] = data_2018$num_of_canceled_trains[which(data_2018$route==route & data_2018$month==month)]/
    data_2018$total_num_trips[which(data_2018$route==route & data_2018$month==month)]
  i=i+1
}

T.obs =median(sample1)

B=1000
set.seed(2024)
T.boot = numeric(B)
for(b in 1:B){
  T.boot[b]  = median(sample(sample1, replace = T))
}
alpha = 0.05
k=47
right.quantile = quantile(T.boot, 1-alpha/(2*k))
left.quantile = quantile(T.boot, alpha/(2*k))
CI_18[month,] = c(T.obs - (right.quantile- T.obs), T.obs - (left.quantile-T.obs)) 
}

CI_18

x11()
plot(1:11, CI_18[,1], ylim=c(0, max(CI_18)))
points(1:11, CI_18[,2])
segments(1:11,CI_18[,1], 1:11,CI_18[,2], col='red3')

x11()
plot(c(1,2,8,9,10,11), CI_18[c(1,2,8,9,10,11),1], ylim=range(CI_18[c(1,2,8,9,10,11),]))
points(c(1,2,8,9,10,11), CI_18[c(1,2,8,9,10,11),2])
segments(c(1,2,8,9,10,11),CI_18[c(1,2,8,9,10,11),1], c(1,2,8,9,10,11),CI_18[c(1,2,8,9,10,11),2], col='red3')


#### 2017 ####
CI_17 = matrix(NA, nrow=12, ncol=2)
missing_routes = NULL
for(route in unique(data_2017$route)){
  v = sum(data_2017$month[which(data_2017$route==route)])
  if(v!=78){
    missing_routes = c(missing_routes, route)
  }
}
for(month in 1:12){
  i=1
  sample1=rep(NA, 108-length(missing_routes))
  for (route in unique(data_2017$route)){
    if(((route %in% missing_routes)==F)){
      sample1[i] = data_2017$num_of_canceled_trains[which(data_2017$route==route & data_2017$month==month)]/
        data_2017$total_num_trips[which(data_2017$route==route & data_2017$month==month)]
      i=i+1
    }
  }
  
  T.obs =median(sample1)
  
  B=1000
  set.seed(2024)
  T.boot = numeric(B)
  for(b in 1:B){
    T.boot[b]  = median(sample(sample1, replace = T))
  }
  alpha = 0.05
  k=47
  right.quantile = quantile(T.boot, 1-alpha/(2*k))
  left.quantile = quantile(T.boot, alpha/(2*k))
  CI_17[month,] = c(T.obs - (right.quantile- T.obs), T.obs - (left.quantile-T.obs)) 
}

CI_17

x11()
plot(1:12, CI_17[,1], ylim=c(0, max(CI_17)))
points(1:12, CI_17[,2])
segments(1:12,CI_17[,1], 1:12,CI_17[,2], col='red3')


#### 2016 ####
CI_16 = matrix(NA, nrow=12, ncol=2)
missing_routes = NULL
for(route in unique(data_2016$route)){
 v = sum(data_2016$month[which(data_2016$route==route)])
 if(v!=78){
   missing_routes = c(missing_routes, route)
 }
}
for(month in 1:12){
  i=1
  sample1=rep(NA, 108-length(missing_routes))
  for (route in unique(data_2016$route)){
    if(((route %in% missing_routes)==F)){
    sample1[i] = data_2016$num_of_canceled_trains[which(data_2016$route==route & data_2016$month==month)]/
      data_2016$total_num_trips[which(data_2016$route==route & data_2016$month==month)]
    i=i+1
    }
  }
  
  T.obs =median(sample1)
  
  B=1000
  set.seed(2024)
  T.boot = numeric(B)
  for(b in 1:B){
    T.boot[b]  = median(sample(sample1, replace = T))
  }
  alpha = 0.05
  k=47
  right.quantile = quantile(T.boot, 1-alpha/(2*k))
  left.quantile = quantile(T.boot, alpha/(2*k))
  CI_16[month,] = c(T.obs - (right.quantile- T.obs), T.obs - (left.quantile-T.obs)) 
}

CI_16

x11()
plot(1:12, CI_16[,1], ylim=c(0, max(CI_16)))
points(1:12, CI_16[,2])
segments(1:12,CI_16[,1], 1:12,CI_16[,2], col='red3')


#### 2015 ####
CI_15 = matrix(NA, nrow=12, ncol=2)
for(month in 1:12){
  i=1
  sample1=rep(NA, 108)
  for (route in unique(data_2015$route)){
    sample1[i] = data_2015$num_of_canceled_trains[which(data_2015$route==route & data_2015$month==month)]/
      data_2015$total_num_trips[which(data_2015$route==route & data_2015$month==month)]
    i=i+1
  }
  
  T.obs =median(sample1)
  
  B=1000
  set.seed(2024)
  T.boot = numeric(B)
  for(b in 1:B){
    T.boot[b]  = median(sample(sample1, replace = T))
  }
  alpha = 0.05
  k=47
  right.quantile = quantile(T.boot, 1-alpha/(2*k))
  left.quantile = quantile(T.boot, alpha/(2*k))
  CI_15[month,] = c(T.obs - (right.quantile- T.obs), T.obs - (left.quantile-T.obs)) 
}

CI_15

x11()
plot(1:12, CI_15[,1], ylim=c(0, max(CI_15)))
points(1:12, CI_15[,2])
segments(1:12,CI_15[,1], 1:12,CI_15[,2], col='red3')


