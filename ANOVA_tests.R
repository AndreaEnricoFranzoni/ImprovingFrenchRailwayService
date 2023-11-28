library(readxl)

data = read_excel('aggregated_trains.xlsx')

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


