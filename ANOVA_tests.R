library(readxl)
library(stats)
library(parallel)
library(pbapply)
data = read_excel('aggregated_trains.xlsx')

n_cores = detectCores()
n_cores

#### ANOVA NATIONAL VS INTERNATIONAL ####
data.cat = factor(data$service)
data.num = data$avg_delay_all_arriving
x.name = 'Service'
y.name = 'Average delay all arriving'

g <- nlevels(data.cat)
n <- dim(data)[1]

x11()
plot(data.cat, data.num, xlab=x.name, ylab=y.name,col=rainbow(g),main='Original Data')

fit <- aov(data.num ~ data.cat)
summary(fit)
# high p-value

# we check the assumptions of parametric ANOVA
# normality within groups
shapiro.test(data.num[which(data.cat=='National')])
shapiro.test(data.num[which(data.cat=='International')])

# same variance
bartlett.test(data.num, data.cat)

T0 <- summary(fit)[[1]][1,4]  # extract the test statistic
T0

B = 1000
set.seed(123)

T_stat <- numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  data.num_perm <- data.num[permutation]
  fit_perm <- aov(data.num_perm ~ data.cat)
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

# Let’s see the distribution, and then the p-value of the permutational f-test
x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val



#### MANOVA NATIONAL VS INTERNATIONAL ####
data.num =data[, c(11,12,20)] # maybe add 10
data.cat = factor(data$service)


x11()
plot(data.num, col=data.cat)


i1 <- which(data.cat==levels(data.cat)[1])
i2 <- which(data.cat==levels(data.cat)[2])

n1 <- length(i1)
n2 <- length(i2)

g  <- length(levels(data.cat))
g
p  <- dim(data.num)[2]
p

# How to perform a MANOVA test? instead of using the F-test, we will develop a test based on 
# a “permutationalisation” of the Wilks1 test. Let’s compute the permutational test statistic

fit <- manova(as.matrix(data.num) ~ data.cat)
summary.manova(fit,test="Wilks") 


T0 <- -summary.manova(fit,test="Wilks")$stats[1,2]
T0

#And let’s now compute instead the permutational distribution of the test statistic.

T_stat <- numeric(B)

for(perm in 1:B){
  # choose random permutation
  permutation <- sample(1:n)
  data.cat.perm <- data.cat[permutation]
  fit.perm <- manova(as.matrix(data.num) ~ data.cat.perm)
  T_stat[perm] <- -summary.manova(fit.perm,test="Wilks")$stats[1,2]
}

#Let’s visualize again the distribution, and the p-value
x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

x11()
plot(ecdf(T_stat))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val




#### ANOVA DEPARTURE STATION ####
stations = c('LYON PART DIEU', 'PARIS NORD')
data_station=NULL
for(i in 1:length(stations))
  data_station = rbind(data_station, data[which(data$departure_station==stations[i]),])

n_routes_from_stat = NULL
for (i in 1:dim(data_station)[1]){
# numero routes che partono da stazione
  station = data_station$departure_station[i]
  num = length(data_station$departure_station[which(data_station$departure_station==station)])
  n_routes_from_stat = c(n_routes_from_stat, num) 
}
n_routes_from_stat

n_delays_from_stat = NULL
for (i in 1:dim(data_station)[1]){
  station = data_station$departure_station[i]
  num = sum(data_station$num_late_at_departure[
    which(data_station$departure_station==station)])
  n_delays_from_stat = c(n_delays_from_stat, num)
}
response = NULL
for (i in 1:dim(data_station)[1]){
  response = c(response, data_station$avg_delay_late_at_departure[i]*
                data_station$num_late_at_departure[i]*n_routes_from_stat[i]/   
                n_delays_from_stat[i])
}
data_station$response = response                                                # since we have aggregated data, we need to readjust them

data.cat = factor(data_station$departure_station)
data.num = data_station$response
x.name = 'Departure station'
y.name = 'Average delay all departing'



g <- nlevels(data.cat)
n <- dim(data_station)[1]

x11()
plot(data.cat, data.num, xlab=x.name, ylab=y.name,col=rainbow(g),main='Original Data')

fit <- aov(data.num ~ data.cat)
summary(fit)
# high p-value

# we check the assumptions of parametric ANOVA
# normality within groups
# shapiro.test(data.num[which(data.cat=='National')])
# shapiro.test(data.num[which(data.cat=='International')])

# same variance
bartlett.test(data.num, data.cat)

T0 <- summary(fit)[[1]][1,4]  # extract the test statistic
T0

B = 1000
#set.seed(123)

T_stat <- numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  data.num_perm <- data.num[permutation]
  fit_perm <- aov(data.num_perm ~ data.cat)
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

# Let’s see the distribution, and then the p-value of the permutational f-test
x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val



#### ANOVA ARRIVAL STATION ####
stations = c('PARIS MONTPARNASSE', 'PARIS LYON')
data_station=NULL
for(i in 1:length(stations))
  data_station = rbind(data_station, data[which(data$arrival_station==stations[i]),])

n_routes_from_stat = NULL
for (i in 1:dim(data_station)[1]){
  # numero routes che partono da stazione
  station = data_station$arrival_station[i]
  num = length(data_station$arrival_station[which(data_station$arrival_station==station)])
  n_routes_from_stat = c(n_routes_from_stat, num) 
}
n_routes_from_stat

n_delays_from_stat = NULL
for (i in 1:dim(data_station)[1]){
  station = data_station$arrival_station[i]
  num = sum(data_station$num_arriving_late[
    which(data_station$arrival_station==station)])
  n_delays_from_stat = c(n_delays_from_stat, num)
}
response = NULL
for (i in 1:dim(data_station)[1]){
  response = c(response, data_station$avg_delay_late_on_arrival[i]*
                 data_station$num_arriving_late[i]*n_routes_from_stat[i]/   
                 n_delays_from_stat[i])
}
data_station$response = response                                                # since we have aggregated data, we need to readjust them

data.cat = factor(data_station$arrival_station)
data.num = data_station$response
x.name = 'Arrival station'
y.name = 'Average delay arriving late'



g <- nlevels(data.cat)
n <- dim(data_station)[1]

x11()
plot(data.cat, data.num, xlab=x.name, ylab=y.name,col=rainbow(g),main='Original Data')

fit <- aov(data.num ~ data.cat)
summary(fit)
# high p-value

# we check the assumptions of parametric ANOVA
# normality within groups
# shapiro.test(data.num[which(data.cat=='National')])
# shapiro.test(data.num[which(data.cat=='International')])

# same variance
bartlett.test(data.num, data.cat)

T0 <- summary(fit)[[1]][1,4]  # extract the test statistic
T0

B = 1000
set.seed(123)

T_stat <- numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  data.num_perm <- data.num[permutation]
  fit_perm <- aov(data.num_perm ~ data.cat)
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

# Let’s see the distribution, and then the p-value of the permutational f-test
x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val




#### T-test DEPARTURE STATION + CI####
stations = c('PARIS LYON', 'PARIS MONTPARNASSE')
data_station=NULL
for(i in 1:length(stations))
  data_station = rbind(data_station, data[which(data$departure_station==stations[i]),])

n_routes_from_stat = NULL
for (i in 1:dim(data_station)[1]){
  # numero routes che partono da stazione
  station = data_station$departure_station[i]
  num = length(data_station$departure_station[which(data_station$departure_station==station)])
  n_routes_from_stat = c(n_routes_from_stat, num) 
}
n_routes_from_stat

n_delays_from_stat = NULL
for (i in 1:dim(data_station)[1]){
  station = data_station$departure_station[i]
  num = sum(data_station$num_late_at_departure[
    which(data_station$departure_station==station)])
  n_delays_from_stat = c(n_delays_from_stat, num)
}
response = NULL
for (i in 1:dim(data_station)[1]){
  response = c(response, data_station$avg_delay_late_at_departure[i]*
                 data_station$num_late_at_departure[i]*n_routes_from_stat[i]/   
                 n_delays_from_stat[i])
}
data_station$response = response                                                # since we have aggregated data, we need to readjust them

n=dim(data_station)[1]
group1 = data_station$response[which(data_station$departure_station==stations[1])]
group2 = data_station$response[which(data_station$departure_station==stations[2])]

data.cat = factor(data_station$departure_station)
data.num = data_station$response
x.name = 'Departure station'
y.name = 'Average delay all departing'

# same variance
fligner.test(data.num, data.cat)

T0=(t.test(group1, group2, alternative="two.sided", var.equal = T, mu=100)$statistic)^2

B = 1000
#set.seed(123)

T_stat = numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation = sample(1:n)
  data.num_perm = data.num[permutation]
  group1_perm = data.num_perm[which(data.cat==stations[1])]-50
  group2_perm = data.num_perm[which(data.cat==stations[2])]-50
  # Test statistic
  T_stat[perm] = (t.test(group1_perm, group2_perm, alternative="two.sided", var.equal = T, mu=0)$statistic)^2
}
x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

x11()
plot(ecdf(T_stat),xlim=range(c(T_stat,T0)))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val

mu.grid=seq(-10,10,length.out=100)

pval = function(data, stations, mu){
group1 = data$response[which(data$departure_station==stations[1])]-mu/2
group2 = data$response[which(data$departure_station==stations[2])]-mu/2
  
data.cat = factor(data$departure_station)
data.num = data$response
T0=(t.test(group1, group2, alternative="two.sided", var.equal = T, mu=0)$statistic)^2
n=length(group1)+length(group2)
B = 1000
T_stat = numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation = sample(1:n)
  data.num_perm = data.num[permutation]
  group1_perm = data.num_perm[which(data.cat==stations[1])]-mu/2
  group2_perm = data.num_perm[which(data.cat==stations[2])]-mu/2
  # Test statistic:
  T_stat[perm] = (t.test(group1_perm, group2_perm, alternative="two.sided", var.equal = T, mu=0)$statistic)^2
}

# p-value
p_val <- sum(T_stat>=T0)/B
return (p_val)
}

cl = makeCluster(n_cores)

clusterExport(cl,varlist=list("data_station","stations", "pval"))

perm_wrapper = function(grid_point) {
  pval(data_station, stations, grid_point)
}

pval_function = pbsapply(mu.grid, perm_wrapper, cl = cl)

alpha = 0.05  # set the significance level
plot(mu.grid, pval_function, type = "l")  # plot p-value function
values.within.CI = mu.grid[pval_function > alpha]
CI = range(values.within.CI)  # obtain the confidence interval
abline(v=CI[1], col="red")
abline(v=CI[2], col="red")

#### MEAN DIFFERENCE DEPARTURE STATION + BOOTSTRAP CI####
stations = c('PARIS LYON', 'PARIS MONTPARNASSE')
data_station=NULL
for(i in 1:length(stations))
  data_station = rbind(data_station, data[which(data$departure_station==stations[i]),])

n_routes_from_stat = NULL
for (i in 1:dim(data_station)[1]){
  # numero routes che partono da stazione
  station = data_station$departure_station[i]
  num = length(data_station$departure_station[which(data_station$departure_station==station)])
  n_routes_from_stat = c(n_routes_from_stat, num) 
}
n_routes_from_stat

n_delays_from_stat = NULL
for (i in 1:dim(data_station)[1]){
  station = data_station$departure_station[i]
  num = sum(data_station$num_late_at_departure[
    which(data_station$departure_station==station)])
  n_delays_from_stat = c(n_delays_from_stat, num)
}
response = NULL
for (i in 1:dim(data_station)[1]){
  response = c(response, data_station$avg_delay_late_at_departure[i]*
                 data_station$num_late_at_departure[i]*n_routes_from_stat[i]/   
                 n_delays_from_stat[i])
}
data_station$response = response                                                # since we have aggregated data, we need to readjust them

n=dim(data_station)[1]
group1 = data_station$response[which(data_station$departure_station==stations[1])]
group2 = data_station$response[which(data_station$departure_station==stations[2])]

T0 = mean(group1)-mean(group2)

data.cat = factor(data_station$departure_station)
data.num = data_station$response

B = 1000
#set.seed(123)

T_boot = numeric(B) 

for(iter in 1:B){
  # Permutation:
  index = sample(1:n, replace = T)
  bootstrap_sample = data.num[index]
  group1_boot = bootstrap_sample[which(data.cat==stations[1])]
  group2_boot = bootstrap_sample[which(data.cat==stations[2])]
  # Test statistic
  T_boot[iter] = mean(group1_boot)-mean(group2_boot)
}

alpha=0.05
right.quantile = quantile(T_boot, 1-alpha/2)
left.quantile = quantile(T_boot, alpha/2)

CI.RP = c(T0 - (right.quantile - T0), T0,  T0 - (left.quantile - T0))
names(CI.RP) = c('lower', 'center', 'upper')
CI.RP