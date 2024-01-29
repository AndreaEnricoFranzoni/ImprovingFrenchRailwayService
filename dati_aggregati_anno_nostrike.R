library(readxl)
library(dplyr)
library(writexl)

rm(list=ls())
graphics.off()
data=read_excel('trains_update_2610.xlsx')
data=data[-which(data$avg_delay_all_arriving<(-30)),]
n=dim(data)[1]
n
p=dim(data)[2]
p

data = data[,-c(9, 13, 17)]                                                # We clean the original data from useless columns (comments, month, year)
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

dataset=na.omit(data)                                                              # we remove all NAs from the dataset, i.e. we drop single observations

col_month = dataset$month
dataset = dataset[,-2]

# by_sum = c(5, 6,7,10, 19, 21, 22)
# by_mean = c(4,8,9,11, 20)
# by_cause = c(12,13,14,15,16,17,18)
# by_names = c(1,2,3,23)

by_sum = c(5, 6,7,10, 19, 21, 22)+1
by_mean_tot = c(4, 9, 12)+1
by_mean_arr = 11+1
by_mean_dep = 8 +1
by_mean_15 = 20+1
by_cause = c(13,14,15,16,17,18)+1
by_names = c(0,1,2,3,23)+1

#### DATA 2015 ####
data=dataset[which(dataset$year==2015),]
data_2015 =  matrix(NA, nrow=length(unique(data$route)), ncol=dim(data)[2])
data_agg = matrix(NA, nrow=length(unique(data$route)), ncol=dim(data)[2])
data_agg=as.data.frame(data_agg)

for (i in 1:length(unique(data$route))){
  for (j in by_sum){
    data_agg[i,j] = sum(data[which(data$route == unique(data$route)[i]),j])   
  }
  for (k in by_mean_tot){
    tot = data_agg[i, 6]
    data_agg[i,k] =     sum(data[which(data$route==unique(data$route)[i]),6]*
                              data[which(data$route==unique(data$route)[i]),k])/tot
  }
  # arrival
  tot = data_agg[i, 11]
  data_agg[i, by_mean_arr] =  sum(data[which(data$route==unique(data$route)[i]),11]*
                                    data[which(data$route==unique(data$route)[i]),by_mean_arr])/tot
  
  # departure
  tot = data_agg[i, 8]
  data_agg[i, by_mean_dep] =  sum(data[which(data$route==unique(data$route)[i]),8]*
                                    data[which(data$route==unique(data$route)[i]),by_mean_dep])/tot
  
  #15 min
  tot = data_agg[i, 20]
  data_agg[i, by_mean_15] =  sum(data[which(data$route==unique(data$route)[i]), 20]*
                                    data[which(data$route==unique(data$route)[i]),by_mean_15])/tot
  
  for (m in by_cause){
    tot = data_agg[i, 11]
    data_agg[i,m] =     sum(data[which(data$route==unique(data$route)[i]),11]*
                              data[which(data$route==unique(data$route)[i]),m])/tot
  }
  data_agg[i, by_names] = as.character(unique(data[which(data$route == unique(data$route)[i]),by_names]))
}

colnames(data_agg) = colnames(data)
data_2015 = data_agg
 
#### DATA 2016 ####
data=dataset[which(dataset$year==2016),]
data_2016 =  matrix(NA, nrow=length(unique(data$route)), ncol=dim(data)[2])
data_agg = matrix(NA, nrow=length(unique(data$route)), ncol=dim(data)[2])
data_agg=as.data.frame(data_agg)

for (i in 1:length(unique(data$route))){
  for (j in by_sum){
    data_agg[i,j] = sum(data[which(data$route == unique(data$route)[i]),j])   
  }
  for (k in by_mean_tot){
    tot = data_agg[i, 6]
    data_agg[i,k] =     sum(data[which(data$route==unique(data$route)[i]),6]*
                              data[which(data$route==unique(data$route)[i]),k])/tot
  }
  # arrival
  tot = data_agg[i, 11]
  data_agg[i, by_mean_arr] =  sum(data[which(data$route==unique(data$route)[i]),11]*
                                    data[which(data$route==unique(data$route)[i]),by_mean_arr])/tot
  
  # departure
  tot = data_agg[i, 8]
  data_agg[i, by_mean_dep] =  sum(data[which(data$route==unique(data$route)[i]),8]*
                                    data[which(data$route==unique(data$route)[i]),by_mean_dep])/tot
  
  #15 min
  tot = data_agg[i, 20]
  data_agg[i, by_mean_15] =  sum(data[which(data$route==unique(data$route)[i]), 20]*
                                   data[which(data$route==unique(data$route)[i]),by_mean_15])/tot
  
  for (m in by_cause){
    tot = data_agg[i, 11]
    data_agg[i,m] =     sum(data[which(data$route==unique(data$route)[i]),11]*
                              data[which(data$route==unique(data$route)[i]),m])/tot
  }
  data_agg[i, by_names] = as.character(unique(data[which(data$route == unique(data$route)[i]),by_names]))
}

colnames(data_agg) = colnames(data)
data_2016 = data_agg


#### DATA 2016 - NO STRIKE ####
month=col_month[which(dataset$year==2016)]
data=dataset[which(dataset$year==2016),]
data=data[-which(month==6),]
data_2016 =  matrix(NA, nrow=length(unique(data$route)), ncol=dim(data)[2])
data_agg = matrix(NA, nrow=length(unique(data$route)), ncol=dim(data)[2])
data_agg=as.data.frame(data_agg)

for (i in 1:length(unique(data$route))){
  for (j in by_sum){
    data_agg[i,j] = sum(data[which(data$route == unique(data$route)[i]),j])   
  }
  for (k in by_mean_tot){
    tot = data_agg[i, 6]
    data_agg[i,k] =     sum(data[which(data$route==unique(data$route)[i]),6]*
                              data[which(data$route==unique(data$route)[i]),k])/tot
  }
  # arrival
  tot = data_agg[i, 11]
  data_agg[i, by_mean_arr] =  sum(data[which(data$route==unique(data$route)[i]),11]*
                                    data[which(data$route==unique(data$route)[i]),by_mean_arr])/tot
  
  # departure
  tot = data_agg[i, 8]
  data_agg[i, by_mean_dep] =  sum(data[which(data$route==unique(data$route)[i]),8]*
                                    data[which(data$route==unique(data$route)[i]),by_mean_dep])/tot
  
  #15 min
  tot = data_agg[i, 20]
  data_agg[i, by_mean_15] =  sum(data[which(data$route==unique(data$route)[i]), 20]*
                                   data[which(data$route==unique(data$route)[i]),by_mean_15])/tot
  
  for (m in by_cause){
    tot = data_agg[i, 11]
    data_agg[i,m] =     sum(data[which(data$route==unique(data$route)[i]),11]*
                              data[which(data$route==unique(data$route)[i]),m])/tot
  }
  data_agg[i, by_names] = as.character(unique(data[which(data$route == unique(data$route)[i]),by_names]))
}

colnames(data_agg) = colnames(data)
data_2016 = data_agg


#### DATA 2017 ####
data=dataset[which(dataset$year==2017),]
data_2017 =  matrix(NA, nrow=length(unique(data$route)), ncol=dim(data)[2])
data_agg = matrix(NA, nrow=length(unique(data$route)), ncol=dim(data)[2])
data_agg=as.data.frame(data_agg)

for (i in 1:length(unique(data$route))){
  for (j in by_sum){
    data_agg[i,j] = sum(data[which(data$route == unique(data$route)[i]),j])   
  }
  for (k in by_mean_tot){
    tot = data_agg[i, 6]
    data_agg[i,k] =     sum(data[which(data$route==unique(data$route)[i]),6]*
                              data[which(data$route==unique(data$route)[i]),k])/tot
  }
  # arrival
  tot = data_agg[i, 11]
  data_agg[i, by_mean_arr] =  sum(data[which(data$route==unique(data$route)[i]),11]*
                                    data[which(data$route==unique(data$route)[i]),by_mean_arr])/tot
  
  # departure
  tot = data_agg[i, 8]
  data_agg[i, by_mean_dep] =  sum(data[which(data$route==unique(data$route)[i]),8]*
                                    data[which(data$route==unique(data$route)[i]),by_mean_dep])/tot
  
  #15 min
  tot = data_agg[i, 20]
  data_agg[i, by_mean_15] =  sum(data[which(data$route==unique(data$route)[i]), 20]*
                                   data[which(data$route==unique(data$route)[i]),by_mean_15])/tot
  
  for (m in by_cause){
    tot = data_agg[i, 11]
    data_agg[i,m] =     sum(data[which(data$route==unique(data$route)[i]),11]*
                              data[which(data$route==unique(data$route)[i]),m])/tot
  }
  data_agg[i, by_names] = as.character(unique(data[which(data$route == unique(data$route)[i]),by_names]))
}

colnames(data_agg) = colnames(data)
data_2017 = data_agg


#### DATA 2018 ####
data=dataset[which(dataset$year==2018),]
data_2018 =  matrix(NA, nrow=length(unique(data$route)), ncol=dim(data)[2])
data_agg = matrix(NA, nrow=length(unique(data$route)), ncol=dim(data)[2])
data_agg=as.data.frame(data_agg)

for (i in 1:length(unique(data$route))){
  for (j in by_sum){
    data_agg[i,j] = sum(data[which(data$route == unique(data$route)[i]),j])   
  }
  for (k in by_mean_tot){
    tot = data_agg[i, 6]
    data_agg[i,k] =     sum(data[which(data$route==unique(data$route)[i]),6]*
                              data[which(data$route==unique(data$route)[i]),k])/tot
  }
  # arrival
  tot = data_agg[i, 11]
  data_agg[i, by_mean_arr] =  sum(data[which(data$route==unique(data$route)[i]),11]*
                                    data[which(data$route==unique(data$route)[i]),by_mean_arr])/tot
  
  # departure
  tot = data_agg[i, 8]
  data_agg[i, by_mean_dep] =  sum(data[which(data$route==unique(data$route)[i]),8]*
                                    data[which(data$route==unique(data$route)[i]),by_mean_dep])/tot
  
  #15 min
  tot = data_agg[i, 20]
  data_agg[i, by_mean_15] =  sum(data[which(data$route==unique(data$route)[i]), 20]*
                                   data[which(data$route==unique(data$route)[i]),by_mean_15])/tot
  
  for (m in by_cause){
    tot = data_agg[i, 11]
    data_agg[i,m] =     sum(data[which(data$route==unique(data$route)[i]),11]*
                              data[which(data$route==unique(data$route)[i]),m])/tot
  }
  data_agg[i, by_names] = as.character(unique(data[which(data$route == unique(data$route)[i]),by_names]))
}

colnames(data_agg) = colnames(data)
data_2018 = data_agg



#### DATA 2018 - NO STRIKE ####
month=col_month[which(dataset$year==2018)]
data=dataset[which(dataset$year==2018),]
data=data[-which(month%in%c(4,5,6,7)),]
data_2018 =  matrix(NA, nrow=length(unique(data$route)), ncol=dim(data)[2])
data_agg = matrix(NA, nrow=length(unique(data$route)), ncol=dim(data)[2])
data_agg=as.data.frame(data_agg)

for (i in 1:length(unique(data$route))){
  for (j in by_sum){
    data_agg[i,j] = sum(data[which(data$route == unique(data$route)[i]),j])   
  }
  for (k in by_mean_tot){
    tot = data_agg[i, 6]
    data_agg[i,k] =     sum(data[which(data$route==unique(data$route)[i]),6]*
                              data[which(data$route==unique(data$route)[i]),k])/tot
  }
  # arrival
  tot = data_agg[i, 11]
  data_agg[i, by_mean_arr] =  sum(data[which(data$route==unique(data$route)[i]),11]*
                                    data[which(data$route==unique(data$route)[i]),by_mean_arr])/tot
  
  # departure
  tot = data_agg[i, 8]
  data_agg[i, by_mean_dep] =  sum(data[which(data$route==unique(data$route)[i]),8]*
                                    data[which(data$route==unique(data$route)[i]),by_mean_dep])/tot
  
  #15 min
  tot = data_agg[i, 20]
  data_agg[i, by_mean_15] =  sum(data[which(data$route==unique(data$route)[i]), 20]*
                                   data[which(data$route==unique(data$route)[i]),by_mean_15])/tot
  
  for (m in by_cause){
    tot = data_agg[i, 11]
    data_agg[i,m] =     sum(data[which(data$route==unique(data$route)[i]),11]*
                              data[which(data$route==unique(data$route)[i]),m])/tot
  }
  data_agg[i, by_names] = as.character(unique(data[which(data$route == unique(data$route)[i]),by_names]))
}

colnames(data_agg) = colnames(data)
data_2018 = data_agg


#### Merging ####
data_agg = rbind(data_2015,data_2016,data_2017, data_2018)

for(i in 1:length(nas))
  data_agg=data_agg[-which(data_agg$route ==nas[i]),]                             # we drop all routes that exceed tolerance from the final dataset
#write_xlsx(data_agg, 'aggregated_trains_by_year_nostrike.xlsx')

