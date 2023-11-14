# ANOVA SULLE TRATTE DI UNA STAZIONE
library(readxl)

data=read_excel('trains_update_2610.xlsx')
n=dim(data)[1]
n
p=dim(data)[2]
p

# First, I look at the stations

# L'idea sarebbe: 
# - dal dataset con tutto seleziono le stazioni da cui partono più di 2-3 tratte
# - vedo i ritardi dei treni per ognuna di queste departure stations
# - seleziono le departure stations con ritardi più alti (facendo un ANOVA ?)
# - faccio la successiva analisi su queste stazioni

x11()
boxplot(data$avg_delay_late_at_departure ~ data$departure_station)

# I isolate data of trains departing from a station
station = 'LYON PART DIEU' # ANOVA returns a very low p-value
#station = 'PARIS EST' #ANOVA returns a very high p-value
data_station = data[which(data$departure_station==station),]
unique(data_station$route)
data_station$route= factor(data_station$route)
data_station$arrival_station = factor(data_station$arrival_station)

#### BOXPLOTS ####
# plot of the average delay of each route 
boxplot(data_station$avg_delay_late_at_departure ~ data_station$arrival_station, 
        main=paste('STATION', station), xlab = 'arrival station', ylab='average delay at departure' )

# plot of the number of trains that have a late departure for each route
boxplot(data_station$num_late_at_departure~ data_station$arrival_station, 
        main=paste('STATION', station), xlab='arrival station', ylab = 'number of late trains at departure' )
# standardised with respect to the total number of trips
boxplot(data_station$num_late_at_departure/data_station$total_num_trips~ data_station$arrival_station, 
        main=paste('STATION', station), xlab = 'arrival station', ylab='ration of late trains at departure' )


#### PARAMETRIC ANOVA ####
nlevels = length(levels(data_station$arrival_station))
nlevels

# first of all, I fit a parametric ANOVA
anova.param = aov(data_station$avg_delay_late_at_departure  ~ data_station$route)
summary(anova.param)

# do the assumptions of ANOVA hold?
# are data within each group normally distributed?
p.shapiro = rep(0, nlevels)
for (i in 1:nlevels){
  p.shapiro[i] = shapiro.test(data_station$avg_delay_late_at_departure[which(data_station$route == levels(data_station$route)[i])])$p.value
}
p.shapiro

# do they have the same variance?
bartlett.test(data_station$avg_delay_late_at_departure, data_station$route)


#### PERMUTATIONAL ANOVA ####
# if the assumptions fail, we can proceed with a non-parametric ANOVA
T0 <- summary(anova.param)[[1]][1,4]  # extract the test statistic
T0

B = 1000
set.seed(123)

T_stat <- numeric(B) 
n_data_station <- dim(data_station)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n_data_station)
  data_perm <- data_station$avg_delay_late_at_departure[permutation]
  fit_perm <- aov(data_perm ~ data_station$route)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)


plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val

# If ANOVA returns a low p-value, it means that the delay at departure is worse for some routes (there may be
# some problematic routes or it has to do with the fact that high speed trains may have priority over the 
# other trains)

# If ANOVA returns a high p-value, it means that the delay at departure is equal for all routes => the station
# can be analysed as a whole


#### CONFIDENCE INTERVALS ####
# Now I would like to build CIs for the mean delay at departure for each route, to see
# which routes are the most


