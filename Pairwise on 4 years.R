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
set.seed(2024)

data = read_excel('trains_update_2610.xlsx')
# Data preprocessing
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
i_national = which(data$service=='National')
i_international=which(data$service == 'International')

variable = data$num_of_canceled_trains/data$total_num_trips
month = as.factor(data$month)
year = as.factor(data$year)

x11()
boxplot(variable ~ month + year)

variable = data$num_greater_30_min_late/data$total_num_trips
month = as.factor(data$month)
year = as.factor(data$year)

x11()
boxplot(variable ~ month + year)

data_to_merge = data.frame(month=month, year=year, trips=data$total_num_trips, canc=data$num_of_canceled_trains, route=data$route)


temp = data_to_merge[which((data_to_merge$year==2015)|(data_to_merge$year==2016 & data_to_merge$month%in%c(1,2,3,4,5))),]

tot_trips = numeric(length(unique(temp$route)))
tot_canc = numeric(length(unique(temp$route)))

for(i in 1:length(unique(temp$route))){
  tot_trips[i] = sum(temp$trips[which(temp$route==unique(temp$route)[i])]) 
  tot_canc[i] = sum(temp$canc[which(temp$route==unique(temp$route)[i])])
}

first_chunk = data.frame(route=unique(temp$route), trips = tot_trips, canc=tot_canc, perc_canc = tot_canc/tot_trips)

temp = data_to_merge[which(data_to_merge$year==2016 & data_to_merge$month==6),]

tot_trips = numeric(length(unique(temp$route)))
tot_canc = numeric(length(unique(temp$route)))

for(i in 1:length(unique(temp$route))){
  tot_trips[i] = sum(temp$trips[which(temp$route==unique(temp$route)[i])]) 
  tot_canc[i] = sum(temp$canc[which(temp$route==unique(temp$route)[i])])
}

first_peak = data.frame(route=unique(temp$route), trips = tot_trips, canc=tot_canc, perc_canc = tot_canc/tot_trips)

temp = data_to_merge[which((data_to_merge$year==2017)|(data_to_merge$year==2016 & data_to_merge$month%in%c(7,8,9,10,11,12))
                           |(data_to_merge$year==2018 & data_to_merge$month%in%c(1,2))),]

tot_trips = numeric(length(unique(temp$route)))
tot_canc = numeric(length(unique(temp$route)))

for(i in 1:length(unique(temp$route))){
  tot_trips[i] = sum(temp$trips[which(temp$route==unique(temp$route)[i])]) 
  tot_canc[i] = sum(temp$canc[which(temp$route==unique(temp$route)[i])])
}

second_chunk = data.frame(route=unique(temp$route), trips = tot_trips, canc=tot_canc, perc_canc = tot_canc/tot_trips)

temp = data_to_merge[which(data_to_merge$year==2018 & data_to_merge$month%in%c(3,4,5,6,7)),]

tot_trips = numeric(length(unique(temp$route)))
tot_canc = numeric(length(unique(temp$route)))

for(i in 1:length(unique(temp$route))){
  tot_trips[i] = sum(temp$trips[which(temp$route==unique(temp$route)[i])]) 
  tot_canc[i] = sum(temp$canc[which(temp$route==unique(temp$route)[i])])
}

second_peak = data.frame(route=unique(temp$route), trips = tot_trips, canc=tot_canc, perc_canc = tot_canc/tot_trips)

temp = data_to_merge[which(data_to_merge$year==2018 & data_to_merge$month%in%c(8,9,10,11)),]

tot_trips = numeric(length(unique(temp$route)))
tot_canc = numeric(length(unique(temp$route)))

for(i in 1:length(unique(temp$route))){
  tot_trips[i] = sum(temp$trips[which(temp$route==unique(temp$route)[i])]) 
  tot_canc[i] = sum(temp$canc[which(temp$route==unique(temp$route)[i])])
}

third_chunk = data.frame(route=unique(temp$route), trips = tot_trips, canc=tot_canc, perc_canc = tot_canc/tot_trips)



stat=function(sample1, mu0){
  t1=median(sample1)
  return ((t1-mu0)^2)
}

# Testiamo first vs second chunk
i=1
sample1=rep(NA, 108)
sample2=rep(NA, 108)
for (route in unique(first_chunk$route)){
  sample1[i] = first_chunk$perc_canc[which(first_chunk$route==route)]
  sample2[i] = second_chunk$perc_canc[which(third_chunk$route==route)]
  i=i+1
}
x11()
par(mfrow=c(1,2))
hist(sample1)
hist(sample2)
x11()
par(mfrow=c(1,2))
boxplot(sample1)
boxplot(sample2)
p = 1
n1 = length(sample1)
n2 = length(sample2)
n  = n1 + n2
mu0 = 0
diff = sample1-sample2
x11()
boxplot(diff)
hist(diff)
shapiro.test(diff)$p.value

B=1000
T.obs = median(diff)
T.obs
set.seed(2024)
cl=makeCluster(parallel::detectCores()/2)
clusterExport(cl=cl,list('diff'))
T.boot=pbreplicate(B, median(sample(diff, replace = T)),cl=cl)

# Bootstrap CI (reverse)
alpha = 0.05
right.quantile = quantile(T.boot, 1 - alpha/2)
left.quantile = quantile(T.boot, alpha/2)

CI.RP = c(T.obs - (right.quantile - T.obs),
          T.obs - (left.quantile - T.obs))
names(CI.RP) = c("lwr", "upr")
CI.RP

x11()
plot(ecdf(T.boot), main='Sample distribution')
abline(v = T.obs, lty=2)
abline(v = CI.RP)
