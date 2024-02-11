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
# removing strikes
data_to_fd = data_to_fd[,-c(15,16,18,39,40,41,42,43)]

rownames(data_to_fd) = r
colnames(data_to_fd) = 1:39

# qua salvo le tratte con degli NA, ma probabilmente non serve
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
data_fd = fData(1:39, data_to_fd)
x11()
plot(data_fd)



x11()
invisible(roahd::fbplot(data_fd, main="Functional boxplot", Fvalue = 1.5))
# save the indices of the outliers
out.fbplot <- roahd::fbplot(data_fd, display = FALSE, Fvalue = 1.5)
out.fbplot$ID_outliers

# # outliergram (phase outliers)
# x11()
# outliergram(data_fd,  Fvalue = 1.5)
# 
# # save the indices of the outliers
# out.outliergram <- outliergram(data_fd, Fvalue = 1.5, display = FALSE)
# out.outliergram$ID_outliers

mei = MEI(data_fd)
mbd = MBD(data_fd)
x11()
plot(mei, mbd, xlab='MEI', ylab='MBD')

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
paris_dep_routes = data$route[which(data$departure_station%in%c('PARIS EST', 'PARIS LYON',
                                                                'PARIS MONTPARNASSE','PARIS NORD'))]
paris_dep_routes = unique(paris_dep_routes)


label = rep(1, dim(data_to_fd)[1]) # 1 if not paris, 0 if paris
for (i in 1:length(routes_no_na)){
  if (routes_no_na[i]%in%paris_dep_routes)
      label[i] = 0
}



#### PERMUTATIONAL FUNCTIONAL TEST FOR 2 POPULATIONS #####
f_data_test = data_fd # functional data object

# save indices of the groups
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
plot(f_data_test, col=colori)
legend(x='topright', legend=c('group1', 'group2'), col = c('darkolivegreen2', 'purple'), lty=1)


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
hist(T0_perm, xlim=range(c(T0, T0_perm)))
abline(v=T0,col='green', lwd=3)

x11()
plot(ecdf(T0_perm), xlim=range(c(T0, T0_perm)))
abline(v=T0,col='green', lwd=3)

