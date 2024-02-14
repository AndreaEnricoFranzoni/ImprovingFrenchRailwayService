library(readxl)
library(dplyr)
library(roahd)
rm(list=ls())
graphics.off()
data=read_excel('Data_by_month.xlsx')

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

#### Dataset for avg delay late on arrival ####
variable = data$avg_delay_late_on_arrival

years = unique(data$year)
months = unique(data$month)
num_tratte = length(unique(data$route))
cols = 47
data_to_fd = matrix(NA, nrow=num_tratte, ncol = cols)

r = unique(data$route)
for (k in 1:num_tratte) {
  for (j in years) {
    for ( i in months){
      if(length(variable[which(data$route==r[k] & data$year==j & data$month==i)])!=0)
        data_to_fd[k,(j-2015)*12 + (i)]=variable[which(data$route==r[k] & data$year==j & data$month==i)]
    }
  }
}


data_to_fd = data.frame(data_to_fd)
#
# removing strikes
data_to_fd = data_to_fd[,-c(15,16,18,39,40,41,42,43)]


n_cols = dim(data_to_fd)[2]
rownames(data_to_fd) = r
colnames(data_to_fd) = 1:n_cols

# qua salvo le tratte con NA
na.indices = NULL
for (i in 1:dim(data_to_fd)[1]){
  for (j in 1:dim(data_to_fd)[2])
    if(is.na(data_to_fd[i,j]))
      na.indices = c(na.indices,i)
}
na.indices 
data_to_fd[na.indices,]


data_to_fd = data_to_fd[-na.indices,]
routes_no_na = r[-na.indices]

#### Conversion to a functional dataset ####
data_fd = fData(1:n_cols, data_to_fd)
x11()
plot(data_fd, cex.lab=1.2, cex.axis=1.2, xlab='Month', ylab='Average delay late on arrival')



x11()
invisible(roahd::fbplot(data_fd, main="", Fvalue = 1.5, xlab='Month', ylab='Average delay late on arrival',cex.lab=1.2, cex.axis=1.2))
# save the indices of the outliers
out.fbplot <- roahd::fbplot(data_fd, display = FALSE, Fvalue = 1.5)
out.fbplot$ID_outliers

mei = MEI(data_fd)
mbd = MBD(data_fd)

N = data_fd$N #number of observations
a0 = -2/(N*(N-1))
a1 = 2*(N+1)/(N-1)

d = a0 + a1*mei + a0*N^2*mei^2 - mbd
f=2
threshold = f*IQR(d) + quantile(d, 0.75)
# Q3 is the 75% quantile

outliers = which(d>threshold)
x11()
plot(mei, mbd, xlab='MEI', ylab='MBD', pch=16)
points(mei[outliers], mbd[outliers], col='red', pch=16)

outliers

#### Create labels ####
# departure station paris vs non paris
paris_dep_routes = data$route[which(data$departure_station%in%c('PARIS EST', 'PARIS LYON',
                                                                'PARIS MONTPARNASSE','PARIS NORD'))]
paris_dep_routes = unique(paris_dep_routes)


label_dep = rep(1, dim(data_to_fd)[1]) # 1 if not paris, 0 if paris
for (i in 1:length(routes_no_na)){
  if (routes_no_na[i]%in%paris_dep_routes)
    label_dep[i] = 0
}

# arrival station paris vs non paris
paris_arr_routes = data$route[which(data$arrival_station%in%c('PARIS EST', 'PARIS LYON',
                                                              'PARIS MONTPARNASSE','PARIS NORD'))]
paris_arr_routes = unique(paris_arr_routes)

label_arr = rep(1, dim(data_to_fd)[1]) # 1 if not paris, 0 if paris
for (i in 1:length(routes_no_na)){
  if (routes_no_na[i]%in%paris_arr_routes)
    label_arr[i] = 0
}


#### Test on departure #####
f_data_test = data_fd # functional data object

label = label_dep
i1 = which(label==0) #paris
i2 = which(label==1) # not paris

data1 = f_data_test[i1,]
data2 = f_data_test[i2,]

n1 = data1$N
n2 = data2$N
median1 = median_fData(data1,type='MBD')
median2 = median_fData(data2,type='MBD')

colori = rep('darkolivegreen2', n1+n2)
colori[i2] = 'purple'
x11()
plot(f_data_test, col=colori, xlab='Month', ylab='Average delay late on arrival',cex.lab=1.2, cex.axis=1.2)
points(1:median1$P, median1$values, col='forestgreen', type='l', lwd=3)
points(1:median2$P, median2$values, col='purple4', type='l', lwd=3)
legend(x='topright', legend=c('Departing from Paris', 'Not departing from Paris'), col = c('darkolivegreen2', 'purple'), lty=1)


median_diff <- median1$values-median2$values
T0=(sum(abs(median_diff))) # test statistic
N <- n1+n2
B <- 1000
T0_perm <- numeric(B)
set.seed(2024)
pb=progress::progress_bar$new(total=B, format = " Processing [:bar] :percent eta: :eta")
for(perm in 1:B){
  permutazione <- sample(N)
  df_perm=f_data_test[permutazione,]
  perm_1 = df_perm[1:n1,]
  perm_2 = df_perm[(n1+1):N,]
  median_diff_perm = median_fData(perm_1, type = 'MBD')$values -
    median_fData(perm_2, type ='MBD')$values
  T0_perm[perm]=sum(abs(median_diff_perm))
  pb$tick()
}
p.val = sum(T0_perm >= T0)/B
p.val

x11()
hist(T0_perm, xlim=range(c(T0, T0_perm)), main='', cex.lab=1.2, cex.axis=1.2)
abline(v=T0,col='green', lwd=3)

#### Local test for departure ####
#this was a global test: the idea now is to perform a local test to see if there are some istants in which 
#the difference exists

#idea: faccio un test per ogni istante temporale: è come se fosse un test univariato
#poi uso BH strategy: prendo i miei 39 test, prendo i pvalues, e li ordino. Prendo il più
#grande sotto il threshold: rifiuto tutti quelli prima di esso

n_cols = dim(data_to_fd)[2]
p_val_BH = numeric(n_cols) #for every time instant, I do a test, nonparametric, and I'll obtain a pvalue

paris_bh = data1$values
no_paris_bh = data2$values

set.seed(2024)
pb=progress::progress_bar$new(total=n_cols, format = " Processing [:bar] :percent eta: :eta")
for (i in 1:n_cols) {
  
  x = paris_bh[,i]
  y = no_paris_bh[,i]
  T0=abs(median(x)-median(y)) 
  
  B <- 1000
  T0_perm <- numeric(B)
  
  
  x_pooled = c(x,y)
  n_bh = length(x_pooled)
  n1_bh = length(x)
  
  for(perm in 1:B){
    # permutation:
    permutation <- sample(1:n_bh)
    x_perm <- x_pooled[permutation]
    x1_perm <- x_perm[1:n1_bh]
    x2_perm <- x_perm[(n1_bh+1):n_bh]
    # test statistic:
    T0_perm[perm] <- abs(median(x1_perm) - median(x2_perm))
  }
  # p-value
  p_val_BH[i] <- sum(T0_perm>=T0)/B
  pb$tick()
  
}

alpha = 0.05
#I have to sort them
time_istants_bh = 1:39
p_val_BH_sort = sort(p_val_BH)
istants_BH_sort = order(p_val_BH)
BH_correction = p.adjust(p_val_BH,method = 'fdr')
test_rifiutati = which(BH_correction<alpha)
indici_rifiutati = which(test_rifiutati %in% istants_BH_sort)

x11()
plot(1:39,p_val_BH_sort,#main='FDR-adjusted p-value function',
     xlab='Cumulated time', cex.lab=1.2, cex.axis=1.2,
     ylab='Sorted p-values',pch=16)
abline(a=0,b=alpha/39, col='red')
abline(h=alpha, col='blue')
#points(1:length(indici_rifiutati),pfor _val_BH_sort[indici_rifiutati],col='red',pch=16)

#### Test on arrival #####
f_data_test = data_fd # functional data object

label = label_arr
i1 = which(label==0) #paris
i2 = which(label==1) # not paris

data1 = f_data_test[i1,]
data2 = f_data_test[i2,]

n1 = data1$N
n2 = data2$N
median1 = median_fData(data1,type='MBD')
median2 = median_fData(data2,type='MBD')

colori = rep('darkolivegreen2', n1+n2)
colori[i2] = 'purple'
x11()
plot(f_data_test, col=colori, xlab='Month', ylab='Average delay late on arrival',cex.lab=1.2, cex.axis=1.2)
points(1:median1$P, median1$values, col='forestgreen', type='l', lwd=3)
points(1:median2$P, median2$values, col='purple4', type='l', lwd=3)
legend(x='topright', legend=c('Arriving in Paris', 'Not arriving in Paris'), col = c('darkolivegreen2', 'purple'), lty=1)

median_diff <- median1$values-median2$values
T0=(sum(abs(median_diff))) # test statistic
N <- n1+n2
B <- 1000
T0_perm <- numeric(B)
set.seed(2024)
pb=progress::progress_bar$new(total=B, format = " Processing [:bar] :percent eta: :eta")
for(perm in 1:B){
  permutazione <- sample(N)
  df_perm=f_data_test[permutazione,]
  perm_1 = df_perm[1:n1,]
  perm_2 = df_perm[(n1+1):N,]
  median_diff_perm = median_fData(perm_1, type = 'MBD')$values -
    median_fData(perm_2, type ='MBD')$values
  T0_perm[perm]=sum(abs(median_diff_perm))
  pb$tick()
}
p.val = sum(T0_perm >= T0)/B
p.val

x11()
hist(T0_perm, xlim=range(c(T0, T0_perm)), main='', cex.lab=1.2, cex.axis=1.2)
abline(v=T0,col='green', lwd=3)


#### Local test for arrival ####
#this was a global test: the idea now is to perform a local test to see if there are some istants in which 
#the difference exists

#idea: faccio un test per ogni istante temporale: è come se fosse un test univariato
#poi uso BH strategy: prendo i miei 39 test, prendo i pvalues, e li ordino. Prendo il più
#grande sotto il threshold: rifiuto tutti quelli prima di esso

n_cols = dim(data_to_fd)[2]
p_val_BH = numeric(n_cols) #for every time instant, I do a test, nonparametric, and I'll obtain a pvalue

paris_bh = data1$values
no_paris_bh = data2$values

set.seed(2024)
pb=progress::progress_bar$new(total=n_cols, format = " Processing [:bar] :percent eta: :eta")
for (i in 1:n_cols) {
  
  x = paris_bh[,i]
  y = no_paris_bh[,i]
  T0=abs(median(x)-median(y)) 
  
  B <- 1000
  T0_perm <- numeric(B)
  
  
  x_pooled = c(x,y)
  n_bh = length(x_pooled)
  n1_bh = length(x)
  
  for(perm in 1:B){
    # permutation:
    permutation <- sample(1:n_bh)
    x_perm <- x_pooled[permutation]
    x1_perm <- x_perm[1:n1_bh]
    x2_perm <- x_perm[(n1_bh+1):n_bh]
    # test statistic:
    T0_perm[perm] <- abs(median(x1_perm) - median(x2_perm))
  }
  # p-value
  p_val_BH[i] <- sum(T0_perm>=T0)/B
  pb$tick()
  
}

alpha = 0.05
#I have to sort them
time_istants_bh = 1:39
p_val_BH_sort = sort(p_val_BH)
istants_BH_sort = order(p_val_BH)
BH_correction = p.adjust(p_val_BH,method = 'fdr')
test_rifiutati = which(BH_correction<alpha)
indici_rifiutati = which(test_rifiutati %in% istants_BH_sort)

x11()
plot(1:39,p_val_BH_sort,#main='FDR-adjusted p-value function',
     xlab='Cumulated time', cex.lab=1.2, cex.axis=1.2,
     ylab='Sorted p-values',pch=16)
abline(a=0,b=alpha/39, col='red')
abline(h=alpha, col='blue')
#points(1:length(indici_rifiutati),pfor _val_BH_sort[indici_rifiutati],col='red',pch=16)

#### Section 2 ####
data=read_excel('Data_by_month.xlsx')
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

#### Dataset for extreme events ####
variable=data$num_greater_30_min_late/data$total_num_trips # variabile di interesse

years = unique(data$year)
months = unique(data$month)
num_tratte = length(unique(data$route))
cols = 47
data_to_fd = matrix(NA, nrow=num_tratte, ncol = cols)

r = unique(data$route)
for (k in 1:num_tratte) {
  for (j in years) {
    for ( i in months){
      if(length(variable[which(data$route==r[k] & data$year==j & data$month==i)])!=0)
        data_to_fd[k,(j-2015)*12 + (i)]=variable[which(data$route==r[k] & data$year==j & data$month==i)]
    }
  }
}


data_to_fd = data.frame(data_to_fd)
#
# removing strikes
data_to_fd = data_to_fd[,-c(15,16,18,39,40,41,42,43)]


n_cols = dim(data_to_fd)[2]
rownames(data_to_fd) = r
colnames(data_to_fd) = 1:n_cols

# qua salvo le tratte con NA
na.indices = NULL
for (i in 1:dim(data_to_fd)[1]){
  for (j in 1:dim(data_to_fd)[2])
    if(is.na(data_to_fd[i,j]))
      na.indices = c(na.indices,i)
}
na.indices 
data_to_fd[na.indices,]


data_to_fd = data_to_fd[-na.indices,]
routes_no_na = r[-na.indices]

#### Conversion to a functional dataset ####
data_fd = fData(1:n_cols, data_to_fd)
x11()
plot(data_fd, cex.lab=1.2, cex.axis=1.2, xlab='Month', ylab='Proportion of trains with delay >30 min')

x11()
invisible(roahd::fbplot(data_fd, main="", Fvalue = 1.5, xlab='Month', ylab='Proportion of trains with delay >30 min',cex.lab=1.2, cex.axis=1.2))
# save the indices of the outliers
out.fbplot <- roahd::fbplot(data_fd, display = FALSE, Fvalue = 1.5)
out.fbplot$ID_outliers

mei = MEI(data_fd)
mbd = MBD(data_fd)

N = data_fd$N #number of observations
a0 = -2/(N*(N-1))
a1 = 2*(N+1)/(N-1)

d = a0 + a1*mei + a0*N^2*mei^2 - mbd
f=2
threshold = f*IQR(d) + quantile(d, 0.75)
# Q3 is the 75% quantile

outliers = which(d>threshold)
x11()
plot(mei, mbd, xlab='MEI', ylab='MBD', pch=16)
points(mei[outliers], mbd[outliers], col='red', pch=16)

outliers


#### Section 3 ####
data=read_excel('Data_by_month.xlsx')
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

#### Dataset for canc rates ####
variable=data$num_of_canceled_trains/data$total_num_trips # variabile di interesse

years = unique(data$year)
months = unique(data$month)
num_tratte = length(unique(data$route))
cols = 47
data_to_fd = matrix(NA, nrow=num_tratte, ncol = cols)

r = unique(data$route)
for (k in 1:num_tratte) {
  for (j in years) {
    for ( i in months){
      if(length(variable[which(data$route==r[k] & data$year==j & data$month==i)])!=0)
        data_to_fd[k,(j-2015)*12 + (i)]=variable[which(data$route==r[k] & data$year==j & data$month==i)]
    }
  }
}


data_to_fd = data.frame(data_to_fd)
#
# removing strikes
data_to_fd = data_to_fd[,-c(15,16,18,39,40,41,42,43)]


n_cols = dim(data_to_fd)[2]
rownames(data_to_fd) = r
colnames(data_to_fd) = 1:n_cols

# qua salvo le tratte con NA
na.indices = NULL
for (i in 1:dim(data_to_fd)[1]){
  for (j in 1:dim(data_to_fd)[2])
    if(is.na(data_to_fd[i,j]))
      na.indices = c(na.indices,i)
}
na.indices 
data_to_fd[na.indices,]


data_to_fd = data_to_fd[-na.indices,]
routes_no_na = r[-na.indices]

#### Conversion to a functional dataset ####
data_fd = fData(1:n_cols, data_to_fd)
x11()
plot(data_fd, cex.lab=1.2, cex.axis=1.2, xlab='Month', ylab='Proportion of cancellations')

x11()
invisible(roahd::fbplot(data_fd, main="", Fvalue = 1.5, xlab='Month', ylab='Proportion of cancellations',cex.lab=1.2, cex.axis=1.2))
# save the indices of the outliers
out.fbplot <- roahd::fbplot(data_fd, display = FALSE, Fvalue = 1.5)
out.fbplot$ID_outliers

mei = MEI(data_fd)
mbd = MBD(data_fd)

N = data_fd$N #number of observations
a0 = -2/(N*(N-1))
a1 = 2*(N+1)/(N-1)

d = a0 + a1*mei + a0*N^2*mei^2 - mbd
f=2
threshold = f*IQR(d) + quantile(d, 0.75)
# Q3 is the 75% quantile

outliers = which(d>threshold)
x11()
plot(mei, mbd, xlab='MEI', ylab='MBD', pch=16)
points(mei[outliers], mbd[outliers], col='red', pch=16)

outliers
