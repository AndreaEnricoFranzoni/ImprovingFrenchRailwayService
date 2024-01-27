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
library(nlme)
library(corrplot)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)
rm(list=ls())
graphics.off()


data = read_excel('aggregated_trains_by_year_2701.xlsx')

n = dim(data)[1]

i_2015=which(data$year==2015)
i_2016=which(data$year==2016)
i_2017=which(data$year==2017)
i_2018=which(data$year==2018)
indexes=list(i_2015, i_2016, i_2017, i_2018)
i_national = which(data$service=='National')
i_international=which(data$service == 'International')

data$year = as.numeric(data$year)
col_years = rep('chartreuse2', n)
col_years[i_2016] = 'coral'
col_years[i_2017] = 'dodgerblue'
col_years[i_2018] = 'purple'

col_service = rep('chartreuse3', n)
col_service[i_international]='coral'

avg_delay_arr = data$avg_delay_all_arriving
avg_delay_dep = data$avg_delay_all_departing
avg_journey = data$journey_time_avg
num_trips=data$total_num_trips
year = as.factor(data$year)
perc_canc = data$num_of_canceled_trains/data$total_num_trips
#### Exploratory plots ####
x11()
plot(data$avg_delay_all_departing, data$avg_delay_all_arriving, col=col_years, pch=16)
legend("topleft", legend=levels(as.factor(data$year)), fill=unique(col_years), cex=.7)
x11()
plot(data$journey_time_avg, data$avg_delay_all_arriving, col=col_years, pch=16)
legend("topleft", legend=levels(as.factor(data$year)), fill=unique(col_years), cex=.7)
x11()
plot(data$total_num_trips, data$avg_delay_all_arriving, col=col_years, pch=16)
legend("topleft", legend=levels(year), fill=unique(col_years), cex=.7)
x11()
plot(perc_canc[i_2015], data$avg_delay_all_arriving[i_2015], col=col_years, pch=16)
legend("topleft", legend=levels(year), fill=unique(col_years), cex=.7)

x11()
plot(year, perc_canc)

# We observe some trends and some dependence across different years
# Num trips seems not relevant to explain the average delay at arrival

#### ANOVA ####
g= nlevels(year)
x11()
plot(year, avg_delay_arr, col=unique(col_years), main = 'Average delay at arrival',
     xlab='', ylab='Average delay at arrival (min)')
# We evidence a possibly increasing trend along the years

fit=aov(avg_delay_arr~year)
summary(fit)

# Test of assumption of normality
shapiro.test(avg_delay_arr[i_2015])
shapiro.test(avg_delay_arr[i_2016])
shapiro.test(avg_delay_arr[i_2017])
shapiro.test(avg_delay_arr[i_2018])
# We don't check equality of variances since the hypoteses are already not satisfied

# Since assumptions are not met, we turn to a nonparametric approach (permutational)
T0 = summary(fit)[[1]][1,4]  # extract the test statistic
T0

B = 1000
set.seed(1234)
T_stat = numeric(B) 
for(perm in 1:B){
  # Permutation:
  permutation = sample(n)
  avg_delay_arr_perm = avg_delay_arr[permutation]
  fit_perm = aov(avg_delay_arr_perm ~ year)
  # Test statistic:
  T_stat[perm] = summary(fit_perm)[[1]][1,4]
}

# Plotting the test statistics
x11()
par(mfrow=c(2,1))
hist(T_stat,xlim=range(c(T_stat,T0)))
abline(v=T0,col=3,lwd=4)
plot(ecdf(T_stat),xlim=range(c(T_stat,T0)))
abline(v=T0,col=3,lwd=4)
p_val = sum(T_stat>=T0)/B
p_val                                                                           
# low p-value, we reject the hypotesis of no difference across the years
# We would like to assess in which years there is a significant difference

#### Confidence intervals with Bonferroni correction ####
B = 1e3
alpha = 0.05

# Bootstrap T-intervals for avg delay at arrival
variable=avg_delay_arr-avg_delay_dep

BootstrapCI = function(variable, alpha, B, indexes, var_name){
k=4 # Bonferroni correction for 4 years
i_2015=indexes[[1]]
i_2016=indexes[[2]]
i_2017=indexes[[3]]
i_2018=indexes[[4]]
# Observed parameter
mu.hat_2015 = mean(variable[i_2015])
mu.hat_2016 = mean(variable[i_2016])
mu.hat_2017 = mean(variable[i_2017])
mu.hat_2018 = mean(variable[i_2018])

# Observed standard deviation
sigma.hat_2015 = sd(variable[i_2015])
sigma.hat_2016 = sd(variable[i_2016])
sigma.hat_2017 = sd(variable[i_2017])
sigma.hat_2018 = sd(variable[i_2018])

# Bootstrap distributions
t.boot_2015 = as.numeric(B)
t.boot_2016 = as.numeric(B)
t.boot_2017 = as.numeric(B)
t.boot_2018 = as.numeric(B)

for (b in 1:B){
  x.boot_2015 = sample(variable[i_2015], replace=T)
  x.boot_2016 = sample(variable[i_2016], replace=T)
  x.boot_2017 = sample(variable[i_2017], replace=T)
  x.boot_2018 = sample(variable[i_2018], replace=T)
  
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

# Computation of bootstrap quantiles with Bonferroni correction
q.low_2015 = quantile(t.boot_2015, alpha/(2*k))
q.low_2016 = quantile(t.boot_2016, alpha/(2*k))
q.low_2017 = quantile(t.boot_2017, alpha/(2*k))
q.low_2018 = quantile(t.boot_2018, alpha/(2*k))

q.up_2015 = quantile(t.boot_2015, 1-alpha/(2*k))
q.up_2016 = quantile(t.boot_2016, 1-alpha/(2*k))
q.up_2017 = quantile(t.boot_2017, 1-alpha/(2*k))
q.up_2018 = quantile(t.boot_2018, 1-alpha/(2*k))

# Computation of the bootstrap T-intervals
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
lower = c(CI_2015[1], CI_2016[1], CI_2017[1], CI_2018[1])
center = c(CI_2015[2], CI_2016[2], CI_2017[2], CI_2018[2])
upper = c(CI_2015[3], CI_2016[3], CI_2017[3], CI_2018[3])
CI_avg_delay_arr = data.frame(lower, center, upper)
rownames(CI_avg_delay_arr) = c(2015, 2016, 2017, 2018)
# Last interval doesn't intersect the other 3 so it is significantly higher
# The other 3 intervals show some partial overlapping

x11()
plot(rep(1,length(variable[i_2015])), variable[i_2015], 
     xlim=c(0.5,4.5), ylim=c(min(variable),max(variable)),
     xlab="Year", ylab=var_name, xaxt='n',
     main="Bootstrap 95% CI for the mean", col=unique(col_years)[1])
points(rep(2,length(variable[i_2016])), variable[i_2016], col=unique(col_years)[2])
points(rep(3,length(variable[i_2017])), variable[i_2017], col=unique(col_years)[3])
points(rep(4,length(variable[i_2018])), variable[i_2018], col=unique(col_years)[4])
axis(1, at=1:4, labels=2015:2018)

points(1,CI_2015[1], col='black', lwd=5)
points(1,CI_2015[3], col='black', lwd=5)
segments(1,CI_2015[1], 1, CI_2015[3], col='black', lwd=3)
segments(1,CI_2015[1], 2, CI_2015[1], col='grey', lty=2)
segments(1,CI_2015[3], 2, CI_2015[3], col='grey', lty=2)
points(2,CI_2016[1], col='black', lwd=5)
points(2,CI_2016[3], col='black', lwd=5)
segments(2,CI_2016[1], 2, CI_2016[3], col='black', lwd=3)
segments(2,CI_2016[1], 3, CI_2016[1], col='grey', lty=2)
segments(2,CI_2016[3], 3, CI_2016[3], col='grey', lty=2)
points(3,CI_2017[1], col='black', lwd=5)
points(3,CI_2017[3], col='black', lwd=5)
segments(3,CI_2017[1], 3, CI_2017[3], col='black', lwd=3)
segments(3,CI_2017[1], 4, CI_2017[1], col='grey', lty=2)
segments(3,CI_2017[3], 4, CI_2017[3], col='grey', lty=2)
points(4,CI_2018[1], col='black', lwd=5)
points(4,CI_2018[3], col='black', lwd=5)
segments(4,CI_2018[1], 4, CI_2018[3], col='black', lwd=3)
segments(4,CI_2018[1], 5, CI_2018[1], col='grey', lty=2)
segments(4,CI_2018[3], 5, CI_2018[3], col='grey', lty=2)

return(CI_avg_delay_arr)
}
# From these intervals, we can understand that the factor 'year' may be relevant
# in building a regression model for the average delay at arrival
CI = BootstrapCI(variable, alpha, B, indexes, 'Cumulated delay')
CI
#### Parametric model ####
# We observe that the observations are not iid, there is a temporal dependence for observations of the same route

initial_model = lm(avg_delay_arr ~ avg_delay_dep:year + avg_journey:year + year)
summary(initial_model)

x11()
par(mfrow=c(2,2))
plot(initial_model)

vif(initial_model)
#### LMM ####
col_route = rainbow(length(unique(data$route)))
x11()
boxplot(initial_model$residuals ~ as.factor(data$route), col=col_route)

# We observe that the residuals are really different for each route
x11()
boxplot(initial_model$residuals ~ year, col=unique(col_years))

# Homoschedastic residuals, but different correlation for the delay obtained from the same route
route=as.factor(data$route)
year_num=as.numeric(year)
mixed_model = gls(avg_delay_arr ~ avg_delay_dep + avg_journey + year, correlation = corSymm(form= ~year_num|route))
summary(mixed_model)
intervals(mixed_model, which='var-cov')

var_cov = getVarCov(mixed_model, individual = 'METZ - PARIS EST')
var_cov =as.matrix(var_cov)

mixed_model_AR = gls(avg_delay_arr ~ avg_delay_dep + avg_journey + year, correlation = corAR1(form= ~year_num|route))
summary(mixed_model_AR)
intervals(mixed_model_AR, which='var-cov')

anova(mixed_model_AR, mixed_model)

x11()
plot(mixed_model)

x11()
plot(mixed_model, resid(., type='p')~fitted(.)| year)

# Since residuals don't look good, we try to add also a random intercept in the routes
mixed_model = lmer(avg_delay_arr ~ avg_delay_dep + avg_journey + year + (1|route))
summary(mixed_model)

confint(mixed_model)

x11()
dotplot(ranef(mixed_model, condVar=T))

x11()
plot(mixed_model, resid(., type='pearson')~fitted(.)| year)

#### GAM ####
model_gam_inter = gam(avg_delay_arr ~ s(avg_delay_dep, by=year, bs = 'cr') 
                      + avg_journey:year + year)
summary(model_gam_inter)
x11()
par(mfcol=c(2, 2))
plot(model_gam_inter)



#### Robust ####
avg_dep = data$avg_delay_all_departing
avg_arr = data$avg_delay_all_arriving
canc_perc = data$num_of_canceled_trains/data$total_num_trips
extreme_delay = data$num_greater_15_min_late/data$total_num_trips

# 2015
multiv_2015 = data.frame(avg_arr=avg_arr[i_2015], avg_dep=avg_dep[i_2015], cancelled_percentage=canc_perc[i_2015],
                         extreme_delay=extreme_delay[i_2015], route=data$route[i_2015])
N=dim(multiv_2015)[1]
fit_MCD = covMcd(x = multiv_2015[,1:4], alpha = .75, nsamp = "best")
fit_MCD

ind_best_subset_2015 = fit_MCD$best
ind_best_subset_2015

ind_out_MCD_2015 = setdiff(1:N,fit_MCD$best)
ind_out_MCD_2015
multiv_2015$route[ind_out_MCD_2015]

#2016
multiv_2016 = data.frame(avg_arr=avg_arr[i_2016], avg_dep=avg_dep[i_2016], cancelled_percentage=canc_perc[i_2016],
                         extreme_delay=extreme_delay[i_2016], route=data$route[i_2016])
N=dim(multiv_2016)[1]
fit_MCD = covMcd(x = multiv_2016[,1:4], alpha = .75, nsamp = "best")
fit_MCD

ind_best_subset_2016 = fit_MCD$best
ind_best_subset_2016

ind_out_MCD_2016 = setdiff(1:N,fit_MCD$best)
ind_out_MCD_2016
multiv_2016$route[ind_out_MCD_2016]

#2017
multiv_2017 = data.frame(avg_arr=avg_arr[i_2017], avg_dep=avg_dep[i_2017], cancelled_percentage=canc_perc[i_2017],
                         extreme_delay=extreme_delay[i_2017], route=data$route[i_2017])
N=dim(multiv_2017)[1]
fit_MCD = covMcd(x = multiv_2017[,1:4], alpha = .75, nsamp = "best")
fit_MCD

ind_best_subset_2017 = fit_MCD$best
ind_best_subset_2017

ind_out_MCD_2017 = setdiff(1:N,fit_MCD$best)
ind_out_MCD_2017
multiv_2017$route[ind_out_MCD_2017]


#2018
multiv_2018 = data.frame(avg_arr=avg_arr[i_2018], avg_dep=avg_dep[i_2018], cancelled_percentage=canc_perc[i_2018],
                         extreme_delay=extreme_delay[i_2018], route=data$route[i_2018])
N=dim(multiv_2018)[1]
fit_MCD = covMcd(x = multiv_2018[,1:4], alpha = .75, nsamp = "best")
fit_MCD

ind_best_subset_2018 = fit_MCD$best
ind_best_subset_2018

ind_out_MCD_2018 = setdiff(1:N,fit_MCD$best)
ind_out_MCD_2018
multiv_2018$route[ind_out_MCD_2018]


x11()
par(mfrow=c(3,2))
plot(avg_delay_dep[i_2018], avg_delay_arr[i_2018], col=ifelse(1:n%in%ind_best_subset_2018,"black","red"),pch=16)
plot(avg_delay_dep[i_2018], canc_perc[i_2018], col=ifelse(1:n%in%ind_best_subset_2018,"black","red"),pch=16)
plot(avg_delay_dep[i_2018], extreme_delay[i_2018], col=ifelse(1:n%in%ind_best_subset_2018,"black","red"),pch=16)
plot(avg_delay_arr[i_2018], canc_perc[i_2018], col=ifelse(1:n%in%ind_best_subset_2018,"black","red"),pch=16)
plot(avg_delay_arr[i_2018], extreme_delay[i_2018], col=ifelse(1:n%in%ind_best_subset_2018,"black","red"),pch=16)
plot(canc_perc[i_2018], extreme_delay[i_2018], col=ifelse(1:n%in%ind_best_subset_2018,"black","red"),pch=16)


