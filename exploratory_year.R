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

data = read_excel('aggregated_trains_by_year_2701.xlsx')
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
plot(data$avg_delay_all_departing, data$avg_delay_all_arriving, col=col_years, pch=16,
     xlab = 'Average delay at departure', ylab = 'Average delay at arrival', cex.axis = 1.2,
     cex.lab=1.2)
legend("topleft", legend=levels(as.factor(data$year)), fill=unique(col_years), cex=1.0)
abline(a=0, b=1, col='cyan')
x11()
plot(data$journey_time_avg, data$avg_delay_all_arriving, col=col_years, pch=16,
     xlab = 'Average journey time', ylab = 'Average delay at arrival', cex.axis = 1.2,
     cex.lab=1.2)
legend("topleft", legend=levels(as.factor(data$year)), fill=unique(col_years), cex=1.0)

x11()
plot(data$total_num_trips, data$avg_delay_all_arriving, col=col_years, pch=16,
     xlab = 'Total number of trips', ylab = 'Average delay at arrival', cex.axis = 1.2,
     cex.lab=1.2)
legend("topleft", legend=levels(as.factor(data$year)), fill=unique(col_years), cex=1.0)

avg_delay_arr = data$avg_delay_all_arriving
avg_delay_dep = data$avg_delay_all_departing
avg_journey = data$journey_time_avg
num_trips=data$total_num_trips
year = as.factor(data$year)

model_param = lm(data$avg_delay_all_arriving ~ data$avg_delay_all_departing + data$journey_time_avg + data$total_num_trips+
                   as.factor(data$year))
summary(model_param)

x11()
par(mfrow=c(2,2))
plot(model_param)

shapiro.test(model_param$residuals)

model_param_int = lm(avg_delay_arr ~ avg_delay_dep:year + avg_journey:year + year)
summary(model_param_int)

x11()
par(mfrow=c(2,2))
plot(model_param_int)

shapiro.test(model_param$residuals)

##### local regression ####
m_loc = npreg(avg_delay_all_arriving ~ avg_delay_all_departing + journey_time_avg ,
              ckertype = 'uniform',
              bws = c(0.5, 0.5), # bandwidth
              data = data)
summary(m_loc)

#### GAM with interaction #####

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

model_gam_inter = gam(avg_delay_arr ~ s(avg_delay_dep, by=year, bs = 'cr') 
                      + s(avg_journey, num_trips, by=year, bs ='tp', m=2) 
                      + year)

test_hyp = lm(avg_delay_arr ~ year)
summary(test_hyp)

x11()
plot(test_hyp$residuals, col=col_years)
shapiro.test(test_hyp$residuals)
model_gam_inter = gam(avg_delay_arr ~ s(avg_delay_dep, by=year, bs = 'cr') 
                      + s(avg_journey, by=year, bs ='cr') + s(num_trips, bs='cr')
                      + year)
summary(model_gam_inter)
x11()
par(mfcol=c(2, 2))
plot(model_gam_inter)

x11()
plot(model_gam_inter$residuals, col=col_years)

# Comparing residuals
mu.res = mean(model_gam_inter$residuals)
s.res = sd(model_gam_inter$residuals)
x = as.matrix(model_gam_inter$residuals)
y = as.matrix(rnorm(length(model_gam_inter$residuals), mu.res, sqrt(s.res))) 
x11()
DepthProc::ddPlot(x, y,depth_params = list(method='Tukey'))

shapiro.test(model_gam_inter$residuals)
# Assolutamente non gaussiani

# Comparing models
model_gam_red = gam(avg_delay_arr ~ s(avg_delay_dep, by=year, bs = 'cr') 
                      + s(avg_journey, by=year, bs ='cr')
                      + year)


n1=length(model_gam_inter$residuals)
res_tot=c(model_gam_red$residuals, model_gam_inter$residuals)
SS1=sum(res_tot[1:n1]^2)
SS2=sum(res_tot[(n1+1):(2*n1)]^2)
df_1= n-sum(summary(model_gam_red)$s.table[,2])-4
df_2 = n-sum(summary(model_gam_inter)$s.table[,2])-4
df_difference = df_1-df_2
anova(model_gam_red, model_gam_inter, test='F')
T0=abs(((SS1-SS2)/df_difference)/(SS2/df_2))

B=1000
T_perm=numeric(B)
for(perm in 1:B){
  permutation=sample(2*n1)
  res_perm=res_tot[permutation]
  SS1=sum(res_perm[1:n1]^2)
  SS2=sum(res_perm[(n1+1):(2*n1)]^2)
  T_perm[perm] = abs(((SS1-SS2)/df_difference)/(SS2/df_2))
}
hist(T_perm)
abline(v=T0)
p.val = sum(T_perm>= T0)/B

# Permutational test for intercept coefficients of the model 

# Test for 2016
i_H1 = c(i_2015, i_2016)
var_2016 = NULL
for(i in 1:n){
  if(i %in% i_H1)
    var_2016 = c(var_2016, 2015)
  else var_2016 = c(var_2016, data$year[i])
}
year_2016 = factor(var_2016)
fit = gam(avg_delay_arr ~ s(avg_delay_dep, by=year, bs = 'cr') 
          + s(avg_journey, by=year, bs ='cr') + s(num_trips, bs='cr')
          + year)
T0 = abs(summary(fit)$p.t[2])
reduced_model= gam(avg_delay_arr ~ s(avg_delay_dep, by=year, bs = 'cr') 
                   + s(avg_journey, by=year, bs ='cr') + s(num_trips, bs='cr')
                   + year_2016)
res = reduced_model$residuals
B= 10
T_H1 = numeric(B)
for(perm in 1:B){
  permutation=sample(i_H1)
  permutation=c(permutation, c(i_2017, i_2018))
  res_perm=res[permutation]
  Y_perm= reduced_model$fitted.values + res_perm
  T_H1[perm] = abs(summary(gam(Y_perm ~ s(avg_delay_dep, by=year, bs = 'cr') 
                       + s(avg_journey, by=year, bs ='cr') + s(num_trips, bs='cr')
                       + year_2016))$p.t[1])
}
p.val = sum(T_H1>= T0)/B

# CI for beta of the intercept
fitted.obs = model_gam_inter$fitted.values
res.obs = model_gam_inter$residuals

b.obs_2016=summary(model_gam_inter)$p.coeff[2]
b.obs_2017=summary(model_gam_inter)$p.coeff[3]
b.obs_2018=summary(model_gam_inter)$p.coeff[4]
B=100
T.boot.b_2016=numeric(B)
T.boot.b_2017=numeric(B)
T.boot.b_2018=numeric(B)

for(b in 1:B){
  delay_arr.boot = fitted.obs[i_2015] + sample(res.obs[i_2015], replace = T)
  delay_arr.boot = c(delay_arr.boot, fitted.obs[i_2016] + sample(res.obs[i_2016], replace = T))
  delay_arr.boot = c(delay_arr.boot, fitted.obs[i_2017] + sample(res.obs[i_2017], replace = T))
  delay_arr.boot = c(delay_arr.boot, fitted.obs[i_2018] + sample(res.obs[i_2018], replace = T))
  boot.model=gam(delay_arr.boot ~ s(avg_delay_dep, by=year, bs = 'cr') 
      + s(avg_journey, by=year, bs ='cr') + s(num_trips, bs='cr')
      + year)
  T.boot.b_2016[b] = summary(boot.model)$p.coeff[2]
  T.boot.b_2017[b] = summary(boot.model)$p.coeff[3]
  T.boot.b_2018[b] = summary(boot.model)$p.coeff[4]
  
}
alpha=0.05
right.quantile_2016 = quantile(T.boot.b_2016, 1-alpha/2)
left.quantile_2016 = quantile(T.boot.b_2016, alpha/2)
CI.RP.b_2016 = c(b.obs_2016 - (right.quantile_2016 - b.obs_2016), b.obs_2016 - (left.quantile_2016 - b.obs_2016))
CI.RP.b_2016
right.quantile_2017 = quantile(T.boot.b_2017, 1-alpha/2)
left.quantile_2017 = quantile(T.boot.b_2017, alpha/2)
CI.RP.b_2017 = c(b.obs_2017 - (right.quantile_2017 - b.obs_2017), b.obs_2017 - (left.quantile_2017 - b.obs_2017))
CI.RP.b_2017
right.quantile_2018 = quantile(T.boot.b_2018, 1-alpha/2)
left.quantile_2018 = quantile(T.boot.b_2018, alpha/2)
CI.RP.b_2018 = c(b.obs_2018 - (right.quantile_2018 - b.obs_2018), b.obs_2018 - (left.quantile_2018 - b.obs_2018))
CI.RP.b_2018


# We remove the effect of 2016 from the intercept, getting a new model
i_H1 = c(i_2015, i_2016)
var_2016 = NULL
for(i in 1:n){
  if(i %in% i_H1)
    var_2016 = c(var_2016, 2015)
  else var_2016 = c(var_2016, data$year[i])
}
year_2016 = factor(var_2016)

model_gam_no2016= gam(avg_delay_arr ~ s(avg_delay_dep, by=year, bs = 'cr') 
                   + s(avg_journey, by=year, bs ='cr') + s(num_trips, bs='cr')
                   + year_2016)
summary(model_gam_no2016)

x11()
plot(model_gam_no2016$residuals)

x11()
par(mfcol=c(2,2))
plot(model_gam_no2016)

# CI for beta of the intercept
fitted.obs = model_gam_no2016$fitted.values
res.obs = model_gam_no2016$residuals

b.obs_2017=summary(model_gam_no2016)$p.coeff[2]
b.obs_2018=summary(model_gam_no2016)$p.coeff[3]
B=100
T.boot.b_2017=numeric(B)
T.boot.b_2018=numeric(B)

for(b in 1:B){
  delay_arr.boot = fitted.obs[i_H1] + sample(res.obs[i_H1], replace = T)
  delay_arr.boot = c(delay_arr.boot, fitted.obs[i_2017] + sample(res.obs[i_2017], replace = T))
  delay_arr.boot = c(delay_arr.boot, fitted.obs[i_2018] + sample(res.obs[i_2018], replace = T))
  boot.model=gam(delay_arr.boot ~ s(avg_delay_dep, by=year, bs = 'cr') 
                 + s(avg_journey, by=year, bs ='cr') + s(num_trips, bs='cr')
                 + year_2016)
  T.boot.b_2017[b] = summary(boot.model)$p.coeff[2]
  T.boot.b_2018[b] = summary(boot.model)$p.coeff[3]
  
}
alpha=0.05
right.quantile_2017 = quantile(T.boot.b_2017, 1-alpha/2)
left.quantile_2017 = quantile(T.boot.b_2017, alpha/2)
CI.RP.b_2017 = c(b.obs_2017 - (right.quantile_2017 - b.obs_2017), b.obs_2017 - (left.quantile_2017 - b.obs_2017))
CI.RP.b_2017
right.quantile_2018 = quantile(T.boot.b_2018, 1-alpha/2)
left.quantile_2018 = quantile(T.boot.b_2018, alpha/2)
CI.RP.b_2018 = c(b.obs_2018 - (right.quantile_2018 - b.obs_2018), b.obs_2018 - (left.quantile_2018 - b.obs_2018))
CI.RP.b_2018


# Can be compared to a model without dependence on 2016 also for the interaction with a proper test (later)
model_gam_no2016_never= gam(avg_delay_arr ~ s(avg_delay_dep, by=year_2016, bs = 'cr') 
                      + s(avg_journey, by=year_2016, bs ='cr') + s(num_trips, bs='cr')
                      + year_2016)
summary(model_gam_no2016_never)
x11()
par(mfcol=c(2, 2))
plot(model_gam_no2016_never)


# ANOVA sulla differenza della media nei 4 anni


#CI: T bootstrap, done on a univariate variable, within the 4 years (Bonferroni correction with k=4)
k = 4 #correzione perchè ci sono 4 anni differenti
B = 1e3
SEED = 14062000
ALPHA = 0.05

#IC per avg delay at dep (devo modificare il titolo e il nome degli assi nel grafico)
variabile=data$avg_delay_all_departing


mu.hat_2015 = mean(variabile[i_2015])
mu.hat_2016 = mean(variabile[i_2016])
mu.hat_2017 = mean(variabile[i_2017])
mu.hat_2018 = mean(variabile[i_2018])

sigma.hat_2015 = sd(variabile[i_2015])
sigma.hat_2016 = sd(variabile[i_2016])
sigma.hat_2017 = sd(variabile[i_2017])
sigma.hat_2018 = sd(variabile[i_2018])


set.seed(SEED)
t.boot_2015 = as.numeric(B)
t.boot_2016 = as.numeric(B)
t.boot_2017 = as.numeric(B)
t.boot_2018 = as.numeric(B)

for (b in 1:B){
  x.boot_2015 = sample(variabile[i_2015], replace=T)
  x.boot_2016 = sample(variabile[i_2016], replace=T)
  x.boot_2017 = sample(variabile[i_2017], replace=T)
  x.boot_2018 = sample(variabile[i_2018], replace=T)
  
  mu.boot_2015 = mean(x.boot_2015)
  mu.boot_2016 = mean(x.boot_2016)
  mu.boot_2017 = mean(x.boot_2017)
  mu.boot_2018 = mean(x.boot_2018)
  
  sigma.boot_2015 = sd(x.boot_2015)
  sigma.boot_2016 = sd(x.boot_2016)
  sigma.boot_2017 = sd(x.boot_2017)
  sigma.boot_2018 = sd(x.boot_2018)
  
  
  t.boot_2015[b] = (mu.boot_2015 - mu.hat_2015) / sigma.boot_2015
  t.boot_2016[b] = (mu.boot_2016 - mu.hat_2016) / sigma.boot_2016
  t.boot_2017[b] = (mu.boot_2017 - mu.hat_2017) / sigma.boot_2017
  t.boot_2018[b] = (mu.boot_2018 - mu.hat_2018) / sigma.boot_2018
  
}



q.low_2015 = quantile(t.boot_2015, ALPHA/(2*k))
q.low_2016 = quantile(t.boot_2016, ALPHA/(2*k))
q.low_2017 = quantile(t.boot_2017, ALPHA/(2*k))
q.low_2018 = quantile(t.boot_2018, ALPHA/(2*k))

q.up_2015 = quantile(t.boot_2015, 1-ALPHA/(2*k))
q.up_2016 = quantile(t.boot_2016, 1-ALPHA/(2*k))
q.up_2017 = quantile(t.boot_2017, 1-ALPHA/(2*k))
q.up_2018 = quantile(t.boot_2018, 1-ALPHA/(2*k))

CI_2015 = c(lower=mu.hat_2015 - q.up_2015 * sigma.hat_2015,
             point=mu.hat_2015,
             upper=mu.hat_2015 - q.low_2015 * sigma.hat_2015)
CI_2016 = c(lower=mu.hat_2016 - q.up_2016 * sigma.hat_2016,
             point=mu.hat_2016,
             upper=mu.hat_2016 - q.low_2016 * sigma.hat_2016)
CI_2017 = c(lower=mu.hat_2017 - q.up_2017 * sigma.hat_2017,
             point=mu.hat_2017,
             upper=mu.hat_2017 - q.low_2017 * sigma.hat_2017)
CI_2018 = c(lower=mu.hat_2018 - q.up_2018 * sigma.hat_2018,
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
B = 1e3
SEED = 14062000
ALPHA = 0.05
mu.hat_delay_arr_2015 = mean(avg_delay_arr[i_2015])
mu.hat_delay_arr_2016 = mean(avg_delay_arr[i_2016])
mu.hat_delay_arr_2017 = mean(avg_delay_arr[i_2017])
mu.hat_delay_arr_2018 = mean(avg_delay_arr[i_2018])

sigma.hat_delay_arr_2015 = sd(avg_delay_arr[i_2015])
sigma.hat_delay_arr_2016 = sd(avg_delay_arr[i_2016])
sigma.hat_delay_arr_2017 = sd(avg_delay_arr[i_2017])
sigma.hat_delay_arr_2018 = sd(avg_delay_arr[i_2018])


set.seed(SEED)
t.boot_delay_arr_2015 = as.numeric(B)
t.boot_delay_arr_2016 = as.numeric(B)
t.boot_delay_arr_2017 = as.numeric(B)
t.boot_delay_arr_2018 = as.numeric(B)

for (b in 1:B){
  x.boot_2015 = sample(avg_delay_arr[i_2015], replace=T)
  x.boot_2016 = sample(avg_delay_arr[i_2016], replace=T)
  x.boot_2017 = sample(avg_delay_arr[i_2017], replace=T)
  x.boot_2018 = sample(avg_delay_arr[i_2018], replace=T)
  
  mu.boot_2015 = mean(x.boot_2015)
  mu.boot_2016 = mean(x.boot_2016)
  mu.boot_2017 = mean(x.boot_2017)
  mu.boot_2018 = mean(x.boot_2018)
  
  sigma.boot_2015 = sd(x.boot_2015)
  sigma.boot_2016 = sd(x.boot_2016)
  sigma.boot_2017 = sd(x.boot_2017)
  sigma.boot_2018 = sd(x.boot_2018)
  
  
  t.boot_delay_arr_2015[b] = (mu.boot_2015 - mu.hat_delay_arr_2015) / sigma.boot_2015
  t.boot_delay_arr_2016[b] = (mu.boot_2016 - mu.hat_delay_arr_2016) / sigma.boot_2016
  t.boot_delay_arr_2017[b] = (mu.boot_2017 - mu.hat_delay_arr_2017) / sigma.boot_2017
  t.boot_delay_arr_2018[b] = (mu.boot_2018 - mu.hat_delay_arr_2018) / sigma.boot_2018
  
}

q.low_delay_arr_2015 = quantile(t.boot_delay_arr_2015, ALPHA/(2*k))
q.low_delay_arr_2016 = quantile(t.boot_delay_arr_2016, ALPHA/(2*k))
q.low_delay_arr_2017 = quantile(t.boot_delay_arr_2017, ALPHA/(2*k))
q.low_delay_arr_2018 = quantile(t.boot_delay_arr_2018, ALPHA/(2*k))

q.up_delay_arr_2015 = quantile(t.boot_delay_arr_2015, 1-ALPHA/(2*k))
q.up_delay_arr_2016 = quantile(t.boot_delay_arr_2016, 1-ALPHA/(2*k))
q.up_delay_arr_2017 = quantile(t.boot_delay_arr_2017, 1-ALPHA/(2*k))
q.up_delay_arr_2018 = quantile(t.boot_delay_arr_2018, 1-ALPHA/(2*k))

CI_delay_arrival_2015 = c(lower=mu.hat_delay_arr_2015 - q.up_delay_arr_2015 * sigma.hat_delay_arr_2015,
                           point=mu.hat_delay_arr_2015,
                           upper=mu.hat_delay_arr_2015 - q.low_delay_arr_2015 * sigma.hat_delay_arr_2015)
CI_delay_arrival_2016 = c(lower=mu.hat_delay_arr_2016 - q.up_delay_arr_2016 * sigma.hat_delay_arr_2016,
                           point=mu.hat_delay_arr_2016,
                           upper=mu.hat_delay_arr_2016 - q.low_delay_arr_2016 * sigma.hat_delay_arr_2016)
CI_delay_arrival_2017 = c(lower=mu.hat_delay_arr_2017 - q.up_delay_arr_2017 * sigma.hat_delay_arr_2017,
                           point=mu.hat_delay_arr_2017,
                           upper=mu.hat_delay_arr_2017 - q.low_delay_arr_2017 * sigma.hat_delay_arr_2017)
CI_delay_arrival_2018 = c(lower=mu.hat_delay_arr_2018 - q.up_delay_arr_2018 * sigma.hat_delay_arr_2018,
                           point=mu.hat_delay_arr_2018,
                           upper=mu.hat_delay_arr_2018 - q.low_delay_arr_2018 * sigma.hat_delay_arr_2018)
CI_delay_arrival_2015
CI_delay_arrival_2016
CI_delay_arrival_2017
CI_delay_arrival_2018

x11()
plot(rep(1,length(avg_delay_arr[i_2015])), avg_delay_arr[i_2015], 
     xlim=c(0,5), ylim=c(0,10),
     xlab="Year", ylab="Average delay at the arrival", 
     main="Bootstrap 95% CI for the mean", col=unique(col_years)[1])
points(rep(2,length(avg_delay_arr[i_2016])), avg_delay_arr[i_2016], col=unique(col_years)[2])
points(rep(3,length(avg_delay_arr[i_2017])), avg_delay_arr[i_2017], col=unique(col_years)[3])
points(rep(4,length(avg_delay_arr[i_2018])), avg_delay_arr[i_2018], col=unique(col_years)[4])

points(1,CI_delay_arrival_2015[1], col='black', lwd=5)
points(1,CI_delay_arrival_2015[3], col='black', lwd=5)
segments(1,CI_delay_arrival_2015[1], 1, CI_delay_arrival_2015[3], col='black', lwd=5)
abline(h=CI_delay_arrival_2015[1], lty=2, col='grey')
abline(h=CI_delay_arrival_2015[3], lty=2, col='grey')
points(2,CI_delay_arrival_2016[1], col='black', lwd=5)
points(2,CI_delay_arrival_2016[3], col='black', lwd=5)
segments(2,CI_delay_arrival_2016[1], 2, CI_delay_arrival_2016[3], col='black', lwd=5)
abline(h=CI_delay_arrival_2016[1], lty=2, col='grey')
abline(h=CI_delay_arrival_2016[3], lty=2, col='grey')
points(3,CI_delay_arrival_2017[1], col='black', lwd=5)
points(3,CI_delay_arrival_2017[3], col='black', lwd=5)
segments(3,CI_delay_arrival_2017[1], 3, CI_delay_arrival_2017[3], col='black', lwd=5)
abline(h=CI_delay_arrival_2017[1], lty=2, col='grey')
abline(h=CI_delay_arrival_2017[3], lty=2, col='grey')
points(4,CI_delay_arrival_2018[1], col='black', lwd=5)
points(4,CI_delay_arrival_2018[3], col='black', lwd=5)
segments(4,CI_delay_arrival_2018[1], 4, CI_delay_arrival_2018[3], col='black', lwd=5)
abline(h=CI_delay_arrival_2018[1], lty=2, col='grey')
abline(h=CI_delay_arrival_2018[3], lty=2, col='grey')

# IC per ritardo all'arrivo-ritardo alla partenza nei 4 anni



graphics.off()
