library(readxl)
library(dplyr)
library(roahd)
rm(list=ls())
graphics.off()
data=read_excel('trains_update_2610.xlsx')

#### Remove NAs ####
data=data[-which(data$avg_delay_all_arriving<(-30)),]

data = data[,-c(9, 13, 17)]   
n=dim(data)[1]
p=dim(data)[2]

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

data=data[-which(data$route%in%nas),]


#### Dataset for a variable ####
variable=data$avg_delay_all_arriving # variabile di interesse

years = unique(data$year)
months = unique(data$month)
num_tratte = length(unique(data$route))
cols = 47
data_to_fd = matrix(NA, nrow=num_tratte, ncol = cols)

r = (unique(data$route))
for (k in 1:num_tratte) {
  for (j in years) {
    for ( i in months){
      if(length(variable[which(data$route==r[k] & data$year==j & data$month==i)])!=0)
        data_to_fd[k,(j-2015)*12 + (i)]=variable[which(data$route==r[k] & data$year==j & data$month==i)]
    }
  }
}


data_to_fd = data.frame(data_to_fd)
rownames(data_to_fd) = r
colnames(data_to_fd) = 1:47

# qua salvo le tratte con degli NA, ma probabilmente non serve
na.indices = NULL
for (i in 1:dim(data_to_fd)[1]){
  for (j in 1:dim(data_to_fd)[2])
    if(is.na(data_to_fd[i,j]))
      na.indices = c(na.indices,i)
}
na.indices 

#### Conversion to a functional dataset ####
data_fd = fData(1:47, data_to_fd)
x11()
plot(data_fd)