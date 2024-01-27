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

data_2018 = data[i_2018,]


stat=function(sample1, mu0){
    t1=median(sample1)
    return ((t1-mu0)^2)
}

# Testiamo marzo-aprile
pval_vec = rep(NA, 10)
shap_vec = rep(NA, 10)
for(month in 1:10){
i=1
sample1=rep(NA, 108)
sample2=rep(NA, 108)
for (route in unique(data_2018$route)){
  sample1[i] = data_2018$num_of_canceled_trains[which(data_2018$route==route & data_2018$month==month)]/
    data_2018$total_num_trips[which(data_2018$route==route & data_2018$month==month)]
  sample2[i] = data_2018$num_of_canceled_trains[which(data_2018$route==route & data_2018$month==(month+1))]/
    data_2018$total_num_trips[which(data_2018$route==route & data_2018$month==(month+1))]
  i=i+1
}
# x11()
# par(mfrow=c(1,2))
# hist(sample1)
# hist(sample2)
# x11()
# par(mfrow=c(1,2))
# boxplot(sample1)
# boxplot(sample2)
p = 1
n1 = length(sample1)
n2 = length(sample2)
n  = n1 + n2
mu0 = 0
diff = sample1-sample2
# x11()
# boxplot(diff)
# hist(diff)
shap_vec[month]=shapiro.test(diff)$p.value
T20 = stat(diff, mu0=mu0)
T20
B=1000
T2 = numeric(B)
for(perm in 1:B){
  signs.perm = rbinom(n1, 1, 0.5)*2 - 1
  diff_perm = diff* signs.perm
  T2[perm]  = stat(diff_perm, mu0=mu0)
}
# x11()
# par(mfrow=c(2,1))
# hist(T2,xlim=range(c(T2,T20)))
# abline(v=T20,col=3,lwd=4)
# plot(ecdf(T2),xlim=range(c(T2,T20)))
# abline(v=T20,col=3,lwd=4)

pval_vec[month] = sum(T2>=T20)/B
if(shap_vec[month]>0.05)
pval_vec[month]=t.test(diff, alternative = "two.sided", mu=0)$p.value
}
pval_vec
shap_vec

pval_vec[2]
