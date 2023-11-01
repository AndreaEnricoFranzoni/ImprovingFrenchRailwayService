library(roahd)
library(readxl)
library(MASS)
library(rgl)
library(DepthProc)
library(hexbin)
library(aplpack)
library(robustbase)
library(MDBED) 

graphics.off()
data=read_excel('trains_update_2610.xlsx')
n=dim(data)[1]
n
p=dim(data)[2]
p

considered_var=data$avg_delay_all_arriving
summary(considered_var)
years = unique(data$year)
months = unique(data$month)
num_tratte = length(unique(data$route))
cols = (length(unique(data$year)))*(length(unique(data$month)))
dati_nuovi = matrix(nrow=num_tratte, ncol = cols)

# Functional depth --------------------------------------------------------
r = (unique(data$route))
for (k in 1:num_tratte) {
  for (j in years) {
    for ( i in months){
      if(length(considered_var[which(data$route==r[k] & data$year==j & data$month==i)])!=0)
        dati_nuovi[k,(j-2015)*12 + (i)]=considered_var[which(data$route==r[k] & data$year==j & data$month==i)]
    }
  }
  dati_nuovi[k,48]=r[k]
}

dati_nuovi = data.frame(dati_nuovi)
dati_nuovi = na.omit(dati_nuovi)

fun = t(as.matrix(dati_nuovi[1:47]))
fun = as.data.frame(fun)
fun = as.data.frame(sapply(fun, as.numeric))
time = 1:47

f_data = fData(time, t(fun))

band_depth = BD(Data = f_data)
modified_band_depth = MBD(Data = f_data)
median_curve = median_fData(fData = f_data, type = "MBD") # still an fData object

x11()
plot(f_data) 
lines(time,median_curve$values, lwd=2)
#lines(time,modified_band_depth, lwd=10, col='black')

# EI(f_data) #per ogni curva, calcola l'Epigraph Index 
# MEI(f_data) #per ogni curva, calcola il Modified EI
# HI(f_data) #per ogni curva, calcola l'Hypograph Index 
# MHI(f_data) #per ogni curva, calcola il modified Hypograph Index 

#outlier detection
set.seed(22)
x11()
invisible(fbplot(f_data, main="Magnitude outliers",adjust = T))
x11()
invisible(outliergram(f_data,adjust = T))

out_mag=fbplot(f_data, main="Magnitude outliers", display=FALSE)
# bag is 50% more central observations, than we find fences
out_mag$ID_outliers
out_shape = outliergram(f_data, adjust=T, display = FALSE)
out_shape$ID_outliers


# Multivariate depth ------------------------------------------------------





#### TRATTA A-B vs TRATTA B-A ####
# We choose a route we want to analyse
departure = 'REIMS'           #A
arrival = 'PARIS EST'              #B
route_AB = paste(departure, '-', arrival)
route_BA = paste(arrival, '-', departure)

data_AB = data[which(data$route==route_AB),]
data_BA = data[which(data$route==route_BA),]

# this function returns the dataset of a given route sorted by date
sort_by_time = function(month, year, data){
  sorted_data = data
  count = 1
  for (i in sort(unique(year))){
    for(j in sort(unique(month))){
      if(dim(data[which(year==i & month==j),])[1]!=0){
        sorted_data[count, ] = data[which(year==i & month==j),]
        count = count +1
      }
    }
  }
  return (sorted_data)
}

data_AB_sorted = sort_by_time(data_AB$month, data_AB$year, data_AB)
data_BA_sorted = sort_by_time(data_BA$month, data_BA$year, data_BA)

# now I need to check that both datasets have an observations for a given date
# if they do not, I drop that observation (not tested)
n1 = dim(data_AB_sorted)[1]
n2 = dim(data_BA_sorted)[1]

final_AB = data_AB_sorted
final_BA = data_BA_sorted
k=1
for (i in 1:n1){
  count = 0
  for(j in 1:n2){
    if (data_AB_sorted$year[i] == data_BA_sorted$year[j] & data_AB_sorted$month[i] == data_BA_sorted$month[j] & count==0){
      count = 1
      final_AB[k,] = as.vector(data_AB_sorted[i,])
      final_BA[k,] = as.vector(data_BA_sorted[j,])
      k = k+1
    }
  }
}

nfinal = dim(final_AB)[1]
if(nfinal > k){
  final_AB= final_AB[-((k+1):n1), ]
  final_BA= final_BA[-((k+1):n2), ]
}


# These are the paired data
data_AB = data.frame(final_AB)
data_BA = data.frame(final_BA)

rm(final_AB)
rm(final_BA)

#### TRATTA A-B vs TRATTA B-A - PAIRED UNIVARIATE TEST - CENTRE OF SYMMETRY TEST ####
dataset1 <- data_AB$avg_delay_late_on_arrival
dataset2 <- data_BA$avg_delay_late_on_arrival
dataset1 = na.omit(dataset1)
dataset2 = na.omit(dataset2)

data_paired = data.frame(dataset1, dataset2) # se ci sono problemi, bisogna eliminare delle righe
names(data_paired) = c(route_AB, route_BA)

x11()
boxplot(data_paired)

x11()
plot(1:n1, dataset1, type='l', xlab='time', col='blue')
points(1:n2, dataset2, type='l', col='red')
legend(x='topright', legend=c(route_AB, route_BA), col=c('blue', 'red'), lty=1)


dataset1=data.frame(dataset1)
dataset2=data.frame(dataset2)
n1 <- dim(dataset1)[1]
n2 <- dim(dataset2)[1] 
n <- n1+n2

dataset1.mean <- colMeans(dataset1)
dataset2.mean <- colMeans(dataset2)
dataset1.cov  <-  cov(dataset1)
dataset2.cov  <-  cov(dataset2)
Sp      <- ((n1-1)*dataset1.cov + (n2-1)*dataset2.cov)/(n1+n2-2)  # pooled cov matrix
Spinv   <- solve(Sp)

delta.0 <- 0 

diff <- dataset1-dataset2
diff.mean <- colMeans(diff)
diff.cov <- cov(diff)
diff.invcov <- solve(diff.cov)

x11()
boxplot(diff)
# Let’s start with the squared euclidean distance between the difference in means and 
# the hypothesised value (then we will use the mahalanobis distance with and without covariance)

T20 <- as.numeric(t(diff.mean-delta.0)  %*% (diff.mean-delta.0))

B = 5000
T2 <- numeric(B)
set.seed(123)

for(perm in 1:B){ # Random permutation
  # obs: exchanging data within couples means changing the sign of the difference
  signs.perm <- rbinom(n1, 1, 0.5)*2 - 1
  diff_perm = NULL
  diff_perm <- diff * matrix(signs.perm,nrow=n1,ncol=1,byrow=FALSE)
  diff.mean_perm <- colMeans(diff_perm)
  diff.cov_perm <- cov(diff_perm)
  diff.invcov_perm <- solve(diff.cov_perm)
  T2[perm] <- as.numeric(t(diff.mean_perm-delta.0) %*% (diff.mean_perm-delta.0))
}

# plotting the permutational distribution under H0
x11()
hist(T2,xlim=range(c(T2,T20)),breaks=100)
abline(v=T20,col=3,lwd=4)

x11()
plot(ecdf(T2), xlim=range(-0.2, T20+0.2))
abline(v=T20,col=3,lwd=4)

# p-value
p_val <- sum(T2>=T20)/B
p_val

graphics.off()

#### TRATTA A-B vs TRATTA B-A - PAIRED MULTIVARIATE TEST ####
dataset1 <- cbind(data_AB$delay_cause_external_cause, data_AB$delay_cause_rail_infrastructure)
dataset2 <- cbind(data_BA$delay_cause_external_cause, data_BA$delay_cause_rail_infrastructure)
dataset1 = na.omit(dataset1)
dataset2 = na.omit(dataset2)

cov_names = c('ext_cause', 'rail_cause')

data_paired = data.frame(dataset1, dataset2)  # se ci sono problemi, bisogna eliminare delle righe
names(data_paired) = c(paste(route_AB, cov_names[1]), paste(route_AB, cov_names[2]), 
                       paste(route_BA, cov_names[1]), paste(route_BA, cov_names[2]))


x11()
boxplot(data_paired, col=c(2,3,2,3))

dataset1=data.frame(dataset1)
#names(dataset1) = c(paste(route_AB, cov_names[1]), paste(route_AB, cov_names[2]))
dataset2=data.frame(dataset2)

x11()
plot(1:n1, dataset1[,1], type='l', xlab='time', col='blue')
points(1:n2, dataset2[,1], type='l', col='red')
legend(x='topright', legend=c(route_AB, route_BA), col=c('blue', 'red'), lty=1)

x11()
plot(1:n1, dataset1[,2], type='l', col='blue')
points(1:n2, dataset2[,2], type='l', col='red')
legend(x='topright', legend=c(route_AB, route_BA), col=c('blue', 'red'), lty=1)


n1 <- dim(dataset1)[1]
n2 <- dim(dataset2)[1] 
p = dim(dataset1)[2]
n <- n1+n2

dataset1.mean <- colMeans(dataset1)
dataset2.mean <- colMeans(dataset2)
dataset1.cov  <-  cov(dataset1)
dataset2.cov  <-  cov(dataset2)
Sp      <- ((n1-1)*dataset1.cov + (n2-1)*dataset2.cov)/(n1+n2-2)  # pooled cov matrix
Spinv   <- solve(Sp)

delta.0 <- rep(0,p)

diff <- dataset1-dataset2
diff.mean <- colMeans(diff)
diff.cov <- cov(diff)
diff.invcov <- solve(diff.cov)

x11()
boxplot(diff)
# Let’s start with the squared euclidean distance between the difference in means and 
# the hypothesised value (then we will use the mahalanobis distance with and without covariance)

T20 <- as.numeric(t(diff.mean-delta.0)  %*% (diff.mean-delta.0))

B = 10000
T2 <- numeric(B)
set.seed(123)

for(perm in 1:B){ # Random permutation
  # obs: exchanging data within couples means changing the sign of the difference
  signs.perm <- rbinom(n1, 1, 0.5)*2 - 1
  diff_perm = NULL
  diff_perm <- diff * matrix(signs.perm,nrow=n1,ncol=p,byrow=FALSE)
  diff.mean_perm <- colMeans(diff_perm)
  diff.cov_perm <- cov(diff_perm)
  diff.invcov_perm <- solve(diff.cov_perm)
  T2[perm] <- as.numeric(t(diff.mean_perm-delta.0) %*% (diff.mean_perm-delta.0))
}

# plotting the permutational distribution under H0
x11()
hist(T2,xlim=range(c(T2,T20)),breaks=100)
abline(v=T20,col=3,lwd=4)

x11()
plot(ecdf(T2), xlim=range(-0.2, T20+0.2))
abline(v=T20,col=3,lwd=4)

# p-value
p_val <- sum(T2>=T20)/B
p_val

graphics.off()
