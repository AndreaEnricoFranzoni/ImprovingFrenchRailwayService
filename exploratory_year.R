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
rm(list=ls())
graphics.off()

data = read_excel('aggregated_trains_by_year.xlsx')
# We remove an evident outlier
data=data[-which(data$year==2017 & data$route=='PARIS LYON - GRENOBLE'),]
n = dim(data)[1]

i_2015=which(data$year==2015)
i_2016=which(data$year==2016)
i_2017=which(data$year==2017)
i_2018=which(data$year==2018)

data$year = as.numeric(data$year)
col_years = rep('chartreuse2', n)
col_years[i_2016] = 'coral'
col_years[i_2017] = 'dodgerblue'
col_years[i_2018] = 'purple'


# per ora consideriamo questa
x11()
par(mfrow=c(1,2))
plot(data$avg_delay_all_departing, data$avg_delay_all_arriving, col=col_years, pch=16)
legend("topleft", legend=levels(as.factor(data$year)), fill=unique(col_years), cex=.7)
abline(a=0, b=1, col='cyan')
plot(data$journey_time_avg, data$avg_delay_all_arriving, col=col_years, pch=16)
legend("topleft", legend=levels(as.factor(data$year)), fill=unique(col_years), cex=.7)


model_param = lm(data$avg_delay_all_arriving ~ data$avg_delay_all_departing + data$journey_time_avg +
                   as.factor(data$year))
summary(model_param)

x11()
par(mfrow=c(2,2))
plot(model_param)

shapiro.test(model_param$residuals)

# si potrebbe usare come covariata anche il numero totale di viaggi

##### local regression ####
m_loc = npreg(avg_delay_all_arriving ~ avg_delay_all_departing + journey_time_avg ,
              ckertype = 'uniform',
              bws = c(0.5, 0.5), # bandwidth
              data = data)
summary(m_loc)

#### GAM with interaction #####
avg_delay_arr = data$avg_delay_all_arriving
avg_delay_dep = data$avg_delay_all_departing
avg_journey = data$journey_time_avg
year = as.factor(data$year)

# summary(model_gam)
# avg_delay_dep.grid=seq(range(avg_delay_dep)[1],range(avg_delay_dep)[2],length.out = 100)
# avg_journey.grid=seq(range(avg_journey)[1],range(avg_journey)[2],length.out = 100)
# grid=expand.grid(avg_delay_dep.grid,avg_journey.grid)
# names(grid)=c('Delay departure','Journey time')
# pred=predict(first_model,newdata=grid) 
# x11()
# persp3d(avg_delay_dep.grid,avg_journey.grid,pred,col='blue',border="black",lwd=0.3)
# points3d(avg_delay_dep,avg_journey,avg_delay_arr,col='black',size=5)

model_gam = gam(data$avg_delay_all_arriving ~ s(data$avg_delay_all_departing, bs = 'cr') + 
                        s(data$journey_time_avg, bs ='cr') +
                       as.factor(data$year))

model_gam = gam(avg_delay_arr ~ s(avg_delay_dep, bs = 'cr') + s(avg_journey, bs ='cr') + s(year, bs='fs') )

summary(model_gam)

x11()
par(mfrow=c(1, 3))
plot(model_gam)

x11()
plot(model_gam$residuals, col=col_years)
legend("topleft", legend=levels(as.factor(data$year)), fill=unique(col_years), cex=.7)



model_gam_inter = gam(avg_delay_arr ~ s(avg_delay_dep, avg_journey, bs = 'tp', m=2) + s(year, bs='fs')  )
model_gam_inter = gam(avg_delay_arr ~ s(avg_delay_dep, by=year, bs = 'cr') + s(avg_journey, by=year, bs ='cr') )
summary(model_gam_inter)
x11()
par(mfrow=c(4, 2))
plot(model_gam_inter)

x11()
plot(model_gam_inter$residuals)

# Comparing residuals
mu.res = mean(model_gam_inter$residuals)
s.res = sd(model_gam_inter$residuals)
x = as.matrix(model_gam_inter$residuals)
y = as.matrix( rnorm(length(model_gam_inter$residuals), mu.res, sqrt(s.res)) ) 
DepthProc::ddPlot(x, y,depth_params = list(method='Tukey'))

shapiro.test(model_gam_inter$residuals)

# Semi parametric model
model_gam_red = gam(avg_delay_arr ~ s(avg_delay_dep, bs = 'cr') + avg_journey + s(year, bs='fs') )

summary(model_gam_red)

anova(model_gam_red,model_gam, test = "F")

x11()
par(mfrow=c(1, 3))
plot(model_gam)

# ANOVA sulla differenza della media nei 4 anni


#CI: T bootstrap, done on a univariate variable, within the 4 years (Bonferroni correction with k=4)
k = 4 #correzione perchè ci sono 4 anni differenti
B <- 1e3
SEED = 14062000
ALPHA = 0.05

#IC per avg delay at dep (devo modificare il titolo e il nome degli assi nel grafico)
variabile=data$avg_delay_all_departing


mu.hat_2015 <- mean(variabile[i_2015])
mu.hat_2016 <- mean(variabile[i_2016])
mu.hat_2017 <- mean(variabile[i_2017])
mu.hat_2018 <- mean(variabile[i_2018])

sigma.hat_2015 <- sd(variabile[i_2015])
sigma.hat_2016 <- sd(variabile[i_2016])
sigma.hat_2017 <- sd(variabile[i_2017])
sigma.hat_2018 <- sd(variabile[i_2018])


set.seed(SEED)
t.boot_2015 <- as.numeric(B)
t.boot_2016 <- as.numeric(B)
t.boot_2017 <- as.numeric(B)
t.boot_2018 <- as.numeric(B)

for (b in 1:B){
  x.boot_2015 <- sample(variabile[i_2015], replace=T)
  x.boot_2016 <- sample(variabile[i_2016], replace=T)
  x.boot_2017 <- sample(variabile[i_2017], replace=T)
  x.boot_2018 <- sample(variabile[i_2018], replace=T)
  
  mu.boot_2015 <- mean(x.boot_2015)
  mu.boot_2016 <- mean(x.boot_2016)
  mu.boot_2017 <- mean(x.boot_2017)
  mu.boot_2018 <- mean(x.boot_2018)
  
  sigma.boot_2015 <- sd(x.boot_2015)
  sigma.boot_2016 <- sd(x.boot_2016)
  sigma.boot_2017 <- sd(x.boot_2017)
  sigma.boot_2018 <- sd(x.boot_2018)
  
  
  t.boot_2015[b] <- (mu.boot_2015 - mu.hat_2015) / sigma.boot_2015
  t.boot_2016[b] <- (mu.boot_2016 - mu.hat_2016) / sigma.boot_2016
  t.boot_2017[b] <- (mu.boot_2017 - mu.hat_2017) / sigma.boot_2017
  t.boot_2018[b] <- (mu.boot_2018 - mu.hat_2018) / sigma.boot_2018
  
}



q.low_2015 <- quantile(t.boot_2015, ALPHA/(2*k))
q.low_2016 <- quantile(t.boot_2016, ALPHA/(2*k))
q.low_2017 <- quantile(t.boot_2017, ALPHA/(2*k))
q.low_2018 <- quantile(t.boot_2018, ALPHA/(2*k))

q.up_2015 <- quantile(t.boot_2015, 1-ALPHA/(2*k))
q.up_2016 <- quantile(t.boot_2016, 1-ALPHA/(2*k))
q.up_2017 <- quantile(t.boot_2017, 1-ALPHA/(2*k))
q.up_2018 <- quantile(t.boot_2018, 1-ALPHA/(2*k))

CI_2015 <- c(lower=mu.hat_2015 - q.up_2015 * sigma.hat_2015,
                           point=mu.hat_2015,
                           upper=mu.hat_2015 - q.low_2015 * sigma.hat_2015)
CI_2016 <- c(lower=mu.hat_2016 - q.up_2016 * sigma.hat_2016,
                       point=mu.hat_2016,
                       upper=mu.hat_2016 - q.low_2016 * sigma.hat_2016)
CI_2017 <- c(lower=mu.hat_2017 - q.up_2017 * sigma.hat_2017,
                       point=mu.hat_2017,
                       upper=mu.hat_2017 - q.low_2017 * sigma.hat_2017)
CI_2018 <- c(lower=mu.hat_2018 - q.up_2018 * sigma.hat_2018,
                       point=mu.hat_2018,
                       upper=mu.hat_2018 - q.low_2018 * sigma.hat_2018)
CI_2015
CI_2016
CI_2017
CI_2018
#poi, se voglio salvarli:
#CI_var_2015=CI_2015
#CI_var_2016=CI_2016
#CI_var_2017=CI_2017
#CI_var_2018=CI_2018



x11()
plot(rep(1,length(variabile[i_2015])), variabile[i_2015], 
     xlim=c(0,5), ylim=c(min(variabile),max(variabile)),
     xlab="Year", ylab="Average delay at the dep", 
     main="Bootstrap 95% CI for the mean", col=unique(col_years)[1])
points(rep(2,length(variabile[i_2016])), variabile[i_2016], col=unique(col_years)[2])
points(rep(3,length(variabile[i_2017])), variabile[i_2017], col=unique(col_years)[3])
points(rep(4,length(variabile[i_2018])), variabile[i_2018], col=unique(col_years)[4])

points(1,CI_2015[1], col='black', lwd=10)
points(1,CI_2015[3], col='black', lwd=10)
segments(1,CI_2015[1], 1, CI_2015[3], col='black', lwd=5)
points(2,CI_2016[1], col='black', lwd=10)
points(2,CI_2016[3], col='black', lwd=10)
segments(2,CI_2016[1], 2, CI_2016[3], col='black', lwd=5)
points(3,CI_2017[1], col='black', lwd=10)
points(3,CI_2017[3], col='black', lwd=10)
segments(3,CI_2017[1], 3, CI_2017[3], col='black', lwd=5)
points(4,CI_2018[1], col='black', lwd=10)
points(4,CI_2018[3], col='black', lwd=10)
segments(4,CI_2018[1], 4, CI_2018[3], col='black', lwd=5)


# IC per ritardo all'arrivo nei 4 anni
k = 4 #correzione perchè ci sono 4 anni differenti
B <- 1e3
SEED = 14062000
ALPHA = 0.05
mu.hat_delay_arr_2015 <- mean(avg_delay_arr[i_2015])
mu.hat_delay_arr_2016 <- mean(avg_delay_arr[i_2016])
mu.hat_delay_arr_2017 <- mean(avg_delay_arr[i_2017])
mu.hat_delay_arr_2018 <- mean(avg_delay_arr[i_2018])

sigma.hat_delay_arr_2015 <- sd(avg_delay_arr[i_2015])
sigma.hat_delay_arr_2016 <- sd(avg_delay_arr[i_2016])
sigma.hat_delay_arr_2017 <- sd(avg_delay_arr[i_2017])
sigma.hat_delay_arr_2018 <- sd(avg_delay_arr[i_2018])


set.seed(SEED)
t.boot_delay_arr_2015 <- as.numeric(B)
t.boot_delay_arr_2016 <- as.numeric(B)
t.boot_delay_arr_2017 <- as.numeric(B)
t.boot_delay_arr_2018 <- as.numeric(B)

for (b in 1:B){
  x.boot_2015 <- sample(avg_delay_arr[i_2015], replace=T)
  x.boot_2016 <- sample(avg_delay_arr[i_2016], replace=T)
  x.boot_2017 <- sample(avg_delay_arr[i_2017], replace=T)
  x.boot_2018 <- sample(avg_delay_arr[i_2018], replace=T)
  
  mu.boot_2015 <- mean(x.boot_2015)
  mu.boot_2016 <- mean(x.boot_2016)
  mu.boot_2017 <- mean(x.boot_2017)
  mu.boot_2018 <- mean(x.boot_2018)
  
  sigma.boot_2015 <- sd(x.boot_2015)
  sigma.boot_2016 <- sd(x.boot_2016)
  sigma.boot_2017 <- sd(x.boot_2017)
  sigma.boot_2018 <- sd(x.boot_2018)
  
  
  t.boot_delay_arr_2015[b] <- (mu.boot_2015 - mu.hat_delay_arr_2015) / sigma.boot_2015
  t.boot_delay_arr_2016[b] <- (mu.boot_2016 - mu.hat_delay_arr_2016) / sigma.boot_2016
  t.boot_delay_arr_2017[b] <- (mu.boot_2017 - mu.hat_delay_arr_2017) / sigma.boot_2017
  t.boot_delay_arr_2018[b] <- (mu.boot_2018 - mu.hat_delay_arr_2018) / sigma.boot_2018
  
}



q.low_delay_arr_2015 <- quantile(t.boot_delay_arr_2015, ALPHA/(2*k))
q.low_delay_arr_2016 <- quantile(t.boot_delay_arr_2016, ALPHA/(2*k))
q.low_delay_arr_2017 <- quantile(t.boot_delay_arr_2017, ALPHA/(2*k))
q.low_delay_arr_2018 <- quantile(t.boot_delay_arr_2018, ALPHA/(2*k))

q.up_delay_arr_2015 <- quantile(t.boot_delay_arr_2015, 1-ALPHA/(2*k))
q.up_delay_arr_2016 <- quantile(t.boot_delay_arr_2016, 1-ALPHA/(2*k))
q.up_delay_arr_2017 <- quantile(t.boot_delay_arr_2017, 1-ALPHA/(2*k))
q.up_delay_arr_2018 <- quantile(t.boot_delay_arr_2018, 1-ALPHA/(2*k))

CI_delay_arrival_2015 <- c(lower=mu.hat_delay_arr_2015 - q.up_delay_arr_2015 * sigma.hat_delay_arr_2015,
        point=mu.hat_delay_arr_2015,
        upper=mu.hat_delay_arr_2015 - q.low_delay_arr_2015 * sigma.hat_delay_arr_2015)
CI_delay_arrival_2016 <- c(lower=mu.hat_delay_arr_2016 - q.up_delay_arr_2016 * sigma.hat_delay_arr_2016,
                           point=mu.hat_delay_arr_2016,
                           upper=mu.hat_delay_arr_2016 - q.low_delay_arr_2016 * sigma.hat_delay_arr_2016)
CI_delay_arrival_2017 <- c(lower=mu.hat_delay_arr_2017 - q.up_delay_arr_2017 * sigma.hat_delay_arr_2017,
                           point=mu.hat_delay_arr_2017,
                           upper=mu.hat_delay_arr_2017 - q.low_delay_arr_2017 * sigma.hat_delay_arr_2017)

CI_delay_arrival_2018 <- c(lower=mu.hat_delay_arr_2018 - q.up_delay_arr_2018 * sigma.hat_delay_arr_2018,
                           point=mu.hat_delay_arr_2018,
                           upper=mu.hat_delay_arr_2018 - q.low_delay_arr_2018 * sigma.hat_delay_arr_2018)
CI_delay_arrival_2015
CI_delay_arrival_2016
CI_delay_arrival_2017
CI_delay_arrival_2018

x11()
plot(rep(1,length(avg_delay_arr[i_2015])), avg_delay_arr[i_2015], 
     xlim=c(0,5), ylim=c(min(avg_delay_arr),max(avg_delay_arr)),
     xlab="Year", ylab="Average delay at the arrival", 
     main="Bootstrap 95% CI for the mean", col=unique(col_years)[1])
points(rep(2,length(avg_delay_arr[i_2016])), avg_delay_arr[i_2016], col=unique(col_years)[2])
points(rep(3,length(avg_delay_arr[i_2017])), avg_delay_arr[i_2017], col=unique(col_years)[3])
points(rep(4,length(avg_delay_arr[i_2018])), avg_delay_arr[i_2018], col=unique(col_years)[4])

points(1,CI_delay_arrival_2015[1], col='black', lwd=10)
points(1,CI_delay_arrival_2015[3], col='black', lwd=10)
segments(1,CI_delay_arrival_2015[1], 1, CI_delay_arrival_2015[3], col='black', lwd=5)
points(2,CI_delay_arrival_2016[1], col='black', lwd=10)
points(2,CI_delay_arrival_2016[3], col='black', lwd=10)
segments(2,CI_delay_arrival_2016[1], 2, CI_delay_arrival_2016[3], col='black', lwd=5)
points(3,CI_delay_arrival_2017[1], col='black', lwd=10)
points(3,CI_delay_arrival_2017[3], col='black', lwd=10)
segments(3,CI_delay_arrival_2017[1], 3, CI_delay_arrival_2017[3], col='black', lwd=5)
points(4,CI_delay_arrival_2018[1], col='black', lwd=10)
points(4,CI_delay_arrival_2018[3], col='black', lwd=10)
segments(4,CI_delay_arrival_2018[1], 4, CI_delay_arrival_2018[3], col='black', lwd=5)

# IC per ritardo all'arrivo-ritardo alla partenza nei 4 anni



graphics.off()
