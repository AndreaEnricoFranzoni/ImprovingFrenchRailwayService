library(readxl)
data=read_excel('trains_update_2610.xlsx')
N=dim(data)[1]
set.seed(1)

finestra_grafica=function(t){
  if(t==0) quartz()
  if(t==1) x11()
} #per usare il giusto comando per l'apertura della finestra grafica
t = 0 #0 per quartz, 1 per x11(): unica volta in cui viene usata la variabile t


library(tidyverse) #libreria che serve per lavorare con le stringhe
#io considero le unità definite unicamente da tratta_mese_anno: quindi ho n unità: le posso considerare tra loro indipendenti

tratte = vector(mode = "character", length = N)

for (i in 1:N) {
  tratte[i]=paste(data$route[i],paste(data$year[i], data$month[i], sep = " "), sep = " / ")
}

data = data.frame(data, row.names = tratte)
#IDEA: DIVIDO IL DATASET NEI QUATTRO ANNI E NEI DUE TIPI DI VIAGGIO (NAZIONALE/INTERNAZIONALE)
# PRENDO IN CONSIDERAZIONE LA VARIABILE ALEATORIA Rit_causa_x: E' UNA BERNOULLI DI PARAMETRO 
# THETA_causa_x, dove THETA_causa_x indica la probabilità di che vi sia un treno in ritardo dovuto
# alla causa_x: FACCIO GLI IC DI QUESTO PARAMETRO CON IL BOOTASTRAP

servizio = factor(data$service)
livelli_servizio = levels(servizio)
g=length(livelli_servizio)
g1 = length(which(servizio==livelli_servizio[2])) #cardinalità nazionali
g2 = length(which(servizio==livelli_servizio[1])) #cardinalità internazionali

anno = factor(data$year)
livelli_anno = levels(anno)
b=length(livelli_anno)
b_2015 = length(which(anno==livelli_anno[1])) #cardinalità 2015
b_2016 = length(which(anno==livelli_anno[2])) #cardinalità 2016
b_2017 = length(which(anno==livelli_anno[3])) #cardinalità 2017
b_2018 = length(which(anno==livelli_anno[4])) #cardinalità 2018

treni_naz_2015 = data[which(data$year==livelli_anno[1] & data$service==livelli_servizio[2]),]
treni_naz_2016 = data[which(data$year==livelli_anno[2] & data$service==livelli_servizio[2]),]
treni_naz_2017 = data[which(data$year==livelli_anno[3] & data$service==livelli_servizio[2]),]
treni_naz_2018 = data[which(data$year==livelli_anno[4] & data$service==livelli_servizio[2]),]
treni_internaz_2015 = data[which(data$year==livelli_anno[1] & data$service==livelli_servizio[1]),]
treni_internaz_2016 = data[which(data$year==livelli_anno[2] & data$service==livelli_servizio[1]),]
treni_internaz_2017 = data[which(data$year==livelli_anno[3] & data$service==livelli_servizio[1]),]
treni_internaz_2018 = data[which(data$year==livelli_anno[4] & data$service==livelli_servizio[1]),]
#fino a qui va fatto runnare, poi ogni punto può essere fatto runnare a parte

B <- 1e3      #numero di ripetzioni del bootstrap
ALPHA=0.05   #intervalli di confidenza bootstrap t al 95%


####
#PROBABILITA' RITARDO per RAIL INFRASTRUCTURE: BOOTSTRAP T-INTERVALS 95%
####

#2015

#nazionali
x.univar=treni_naz_2015$delay_cause_rail_infrastructure
length(x.univar) #1200
x.univar=x.univar[which(x.univar!="NA")]
length(x.univar) #1200
#ok: ho tutti i dati
mu.hat <- mean(x.univar)
sigma.hat <- sd(x.univar)

t.boot <- as.numeric(B)
for (b in 1:B){
  x.boot <- sample(x.univar, replace=T)
  mu.boot <- mean(x.boot)
  sigma.boot <- sd(x.boot)
  
  t.boot[b] <- (mu.boot - mu.hat) / sigma.boot
}

finestra_grafica(t)
hist(t.boot)
abline(v=0, col="pink", cex=2)

q.low <- quantile(t.boot, ALPHA/2)
q.up <- quantile(t.boot, 1-ALPHA/2)
CI_naz_2015_RI <- c(lower=mu.hat - q.up * sigma.hat,
        point=mu.hat,
        upper=mu.hat - q.low * sigma.hat)
CI_naz_2015_RI

#internazionali
x.univar=treni_internaz_2015$delay_cause_rail_infrastructure
length(x.univar) #144
x.univar=x.univar[which(x.univar!="NA")]
length(x.univar) #96
#ho perso 48/144 % = 1/3 % dei dati
mu.hat <- mean(x.univar)
sigma.hat <- sd(x.univar)

t.boot <- as.numeric(B)
for (b in 1:B){
  x.boot <- sample(x.univar, replace=T)
  mu.boot <- mean(x.boot)
  sigma.boot <- sd(x.boot)
  
  t.boot[b] <- (mu.boot - mu.hat) / sigma.boot
}

finestra_grafica(t)
hist(t.boot)
abline(v=0, col="pink", cex=2)

q.low <- quantile(t.boot, ALPHA/2)
q.up <- quantile(t.boot, 1-ALPHA/2)
CI_internaz_2015_RI <- c(lower=mu.hat - q.up * sigma.hat,
                    point=mu.hat,
                    upper=mu.hat - q.low * sigma.hat)
CI_internaz_2015_RI


#2016

#nazionali
x.univar=treni_naz_2016$delay_cause_rail_infrastructure
length(x.univar) #1200
x.univar=x.univar[which(x.univar!="NA")]
length(x.univar) #1200
#ok: ho tutti i dati
mu.hat <- mean(x.univar)
sigma.hat <- sd(x.univar)

t.boot <- as.numeric(B)
for (b in 1:B){
  x.boot <- sample(x.univar, replace=T)
  mu.boot <- mean(x.boot)
  sigma.boot <- sd(x.boot)
  
  t.boot[b] <- (mu.boot - mu.hat) / sigma.boot
}

finestra_grafica(t)
hist(t.boot)
abline(v=0, col="pink", cex=2)

q.low <- quantile(t.boot, ALPHA/2)
q.up <- quantile(t.boot, 1-ALPHA/2)
CI_naz_2016_RI <- c(lower=mu.hat - q.up * sigma.hat,
                    point=mu.hat,
                    upper=mu.hat - q.low * sigma.hat)
CI_naz_2016_RI

#internazionali
x.univar=treni_internaz_2016$delay_cause_rail_infrastructure
length(x.univar) #144
x.univar=x.univar[which(x.univar!="NA")]
length(x.univar) #97
#ho perso 47/144 % = 0.3263889 % dei dati
mu.hat <- mean(x.univar)
sigma.hat <- sd(x.univar)

t.boot <- as.numeric(B)
for (b in 1:B){
  x.boot <- sample(x.univar, replace=T)
  mu.boot <- mean(x.boot)
  sigma.boot <- sd(x.boot)
  
  t.boot[b] <- (mu.boot - mu.hat) / sigma.boot
}

finestra_grafica(t)
hist(t.boot)
abline(v=0, col="pink", cex=2)

q.low <- quantile(t.boot, ALPHA/2)
q.up <- quantile(t.boot, 1-ALPHA/2)
CI_internaz_2016_RI <- c(lower=mu.hat - q.up * sigma.hat,
                         point=mu.hat,
                         upper=mu.hat - q.low * sigma.hat)
CI_internaz_2016_RI


#2017

#nazionali
x.univar=treni_naz_2017$delay_cause_rail_infrastructure
length(x.univar) #1200
x.univar=x.univar[which(x.univar!="NA")]
length(x.univar) #1199
#ho perso 1/1200=0.0008333333 % dei dati
mu.hat <- mean(x.univar)
sigma.hat <- sd(x.univar)

t.boot <- as.numeric(B)
for (b in 1:B){
  x.boot <- sample(x.univar, replace=T)
  mu.boot <- mean(x.boot)
  sigma.boot <- sd(x.boot)
  
  t.boot[b] <- (mu.boot - mu.hat) / sigma.boot
}

finestra_grafica(t)
hist(t.boot)
abline(v=0, col="pink", cex=2)

q.low <- quantile(t.boot, ALPHA/2)
q.up <- quantile(t.boot, 1-ALPHA/2)
CI_naz_2017_RI <- c(lower=mu.hat - q.up * sigma.hat,
                    point=mu.hat,
                    upper=mu.hat - q.low * sigma.hat)
CI_naz_2017_RI

#internazionali
x.univar=treni_internaz_2017$delay_cause_rail_infrastructure
length(x.univar) #144
x.univar=x.univar[which(x.univar!="NA")]
length(x.univar) #143
#ho perso 1/144 % = 0.006993007 % dei dati
mu.hat <- mean(x.univar)
sigma.hat <- sd(x.univar)

t.boot <- as.numeric(B)
for (b in 1:B){
  x.boot <- sample(x.univar, replace=T)
  mu.boot <- mean(x.boot)
  sigma.boot <- sd(x.boot)
  
  t.boot[b] <- (mu.boot - mu.hat) / sigma.boot
}

finestra_grafica(t)
hist(t.boot)
abline(v=0, col="pink", cex=2)

q.low <- quantile(t.boot, ALPHA/2)
q.up <- quantile(t.boot, 1-ALPHA/2)
CI_internaz_2017_RI <- c(lower=mu.hat - q.up * sigma.hat,
                         point=mu.hat,
                         upper=mu.hat - q.low * sigma.hat)
CI_internaz_2017_RI


#2018

#nazionali
x.univar=treni_naz_2018$delay_cause_rail_infrastructure
length(x.univar) #1254
x.univar=x.univar[which(x.univar!="NA")]
length(x.univar) #1247
#ho perso 7/1254=0.005582137 % dei dati
mu.hat <- mean(x.univar)
sigma.hat <- sd(x.univar)

t.boot <- as.numeric(B)
for (b in 1:B){
  x.boot <- sample(x.univar, replace=T)
  mu.boot <- mean(x.boot)
  sigma.boot <- sd(x.boot)
  
  t.boot[b] <- (mu.boot - mu.hat) / sigma.boot
}

finestra_grafica(t)
hist(t.boot)
abline(v=0, col="pink", cex=2)

q.low <- quantile(t.boot, ALPHA/2)
q.up <- quantile(t.boot, 1-ALPHA/2)
CI_naz_2018_RI <- c(lower=mu.hat - q.up * sigma.hat,
                    point=mu.hat,
                    upper=mu.hat - q.low * sigma.hat)
CI_naz_2018_RI

#internazionali
x.univar=treni_internaz_2018$delay_cause_rail_infrastructure
length(x.univar) #176
x.univar=x.univar[which(x.univar!="NA")]
length(x.univar) #110
#ho perso 1/144 % = 0.375 % dei dati
mu.hat <- mean(x.univar)
sigma.hat <- sd(x.univar)

t.boot <- as.numeric(B)
for (b in 1:B){
  x.boot <- sample(x.univar, replace=T)
  mu.boot <- mean(x.boot)
  sigma.boot <- sd(x.boot)
  
  t.boot[b] <- (mu.boot - mu.hat) / sigma.boot
}

finestra_grafica(t)
hist(t.boot)
abline(v=0, col="pink", cex=2)

q.low <- quantile(t.boot, ALPHA/2)
q.up <- quantile(t.boot, 1-ALPHA/2)
CI_internaz_2018_RI <- c(lower=mu.hat - q.up * sigma.hat,
                         point=mu.hat,
                         upper=mu.hat - q.low * sigma.hat)
CI_internaz_2018_RI


#riunisco questi intervalli in una tabella
CI_RAIL_INFRASTRUCTURE_NAZ = data.frame(cbind("2015"=CI_naz_2015_RI, "2016"=CI_naz_2016_RI,
                                   "2017"=CI_naz_2017_RI, "2018"=CI_naz_2018_RI))
CI_RAIL_INFRASTRUCTURE_NAZ
CI_RAIL_INFRASTRUCTURE_INTERNAZ = data.frame(cbind("2015"=CI_internaz_2015_RI, "2016"=CI_internaz_2016_RI,
                                        "2017"=CI_internaz_2017_RI, "2018"=CI_internaz_2018_RI))
CI_RAIL_INFRASTRUCTURE_INTERNAZ

#plotto gli intervalli
finestra_grafica(t)
matplot(t(CI_RAIL_INFRASTRUCTURE_NAZ), type="l", 
        main="Bootstrap T 95% CI for the probability of 
        being late due to rail infrastructure, national routes",
        xlab="Year", col='red', lty='solid', lwd=3)
points(1,CI_RAIL_INFRASTRUCTURE_NAZ[1,1], col='black', lwd=10)
points(1,CI_RAIL_INFRASTRUCTURE_NAZ[3,1], col='black', lwd=10)
segments(1,CI_RAIL_INFRASTRUCTURE_NAZ[1,1], 1, CI_RAIL_INFRASTRUCTURE_NAZ[3,1], col='black', lwd=5)
points(2,CI_RAIL_INFRASTRUCTURE_NAZ[1,2], col='black', lwd=10)
points(2,CI_RAIL_INFRASTRUCTURE_NAZ[3,2], col='black', lwd=10)
segments(2,CI_RAIL_INFRASTRUCTURE_NAZ[1,2], 2, CI_RAIL_INFRASTRUCTURE_NAZ[3,2], col='black', lwd=5)
points(3,CI_RAIL_INFRASTRUCTURE_NAZ[1,3], col='black', lwd=10)
points(3,CI_RAIL_INFRASTRUCTURE_NAZ[3,3], col='black', lwd=10)
segments(3,CI_RAIL_INFRASTRUCTURE_NAZ[1,3], 3, CI_RAIL_INFRASTRUCTURE_NAZ[3,3], col='black', lwd=5)
points(4,CI_RAIL_INFRASTRUCTURE_NAZ[1,4], col='black', lwd=10)
points(4,CI_RAIL_INFRASTRUCTURE_NAZ[3,4], col='black', lwd=10)
segments(4,CI_RAIL_INFRASTRUCTURE_NAZ[1,4], 4, CI_RAIL_INFRASTRUCTURE_NAZ[3,4], col='black', lwd=5)

finestra_grafica(t)
matplot(t(CI_RAIL_INFRASTRUCTURE_INTERNAZ), type="l", 
        main="Bootstrap T 95% CI for the probability of 
        being late due to rail infrastructure, international routes",
        xlab="Year", col='red', lty='solid', lwd=3)
points(1,CI_RAIL_INFRASTRUCTURE_INTERNAZ[1,1], col='black', lwd=10)
points(1,CI_RAIL_INFRASTRUCTURE_INTERNAZ[3,1], col='black', lwd=10)
segments(1,CI_RAIL_INFRASTRUCTURE_INTERNAZ[1,1], 1, CI_RAIL_INFRASTRUCTURE_INTERNAZ[3,1], col='black', lwd=5)
points(2,CI_RAIL_INFRASTRUCTURE_INTERNAZ[1,2], col='black', lwd=10)
points(2,CI_RAIL_INFRASTRUCTURE_INTERNAZ[3,2], col='black', lwd=10)
segments(2,CI_RAIL_INFRASTRUCTURE_INTERNAZ[1,2], 2, CI_RAIL_INFRASTRUCTURE_INTERNAZ[3,2], col='black', lwd=5)
points(3,CI_RAIL_INFRASTRUCTURE_INTERNAZ[1,3], col='black', lwd=10)
points(3,CI_RAIL_INFRASTRUCTURE_INTERNAZ[3,3], col='black', lwd=10)
segments(3,CI_RAIL_INFRASTRUCTURE_INTERNAZ[1,3], 3, CI_RAIL_INFRASTRUCTURE_INTERNAZ[3,3], col='black', lwd=5)
points(4,CI_RAIL_INFRASTRUCTURE_INTERNAZ[1,4], col='black', lwd=10)
points(4,CI_RAIL_INFRASTRUCTURE_INTERNAZ[3,4], col='black', lwd=10)
segments(4,CI_RAIL_INFRASTRUCTURE_INTERNAZ[1,4], 4, CI_RAIL_INFRASTRUCTURE_INTERNAZ[3,4], col='black', lwd=5)


finestra_grafica(t)
par(mfrow=c(1,2))
matplot(t(CI_RAIL_INFRASTRUCTURE_NAZ), type="l", 
        main="Bootstrap T 95% CI for the probability of 
        being late due to rail infrastructure, national routes",
        xlab="Year", col='red', ylim=c(0.1,0.3), lty='solid', lwd=3)
points(1,CI_RAIL_INFRASTRUCTURE_NAZ[1,1], col='black', lwd=10)
points(1,CI_RAIL_INFRASTRUCTURE_NAZ[3,1], col='black', lwd=10)
segments(1,CI_RAIL_INFRASTRUCTURE_NAZ[1,1], 1, CI_RAIL_INFRASTRUCTURE_NAZ[3,1], col='black', lwd=5)
points(2,CI_RAIL_INFRASTRUCTURE_NAZ[1,2], col='black', lwd=10)
points(2,CI_RAIL_INFRASTRUCTURE_NAZ[3,2], col='black', lwd=10)
segments(2,CI_RAIL_INFRASTRUCTURE_NAZ[1,2], 2, CI_RAIL_INFRASTRUCTURE_NAZ[3,2], col='black', lwd=5)
points(3,CI_RAIL_INFRASTRUCTURE_NAZ[1,3], col='black', lwd=10)
points(3,CI_RAIL_INFRASTRUCTURE_NAZ[3,3], col='black', lwd=10)
segments(3,CI_RAIL_INFRASTRUCTURE_NAZ[1,3], 3, CI_RAIL_INFRASTRUCTURE_NAZ[3,3], col='black', lwd=5)
points(4,CI_RAIL_INFRASTRUCTURE_NAZ[1,4], col='black', lwd=10)
points(4,CI_RAIL_INFRASTRUCTURE_NAZ[3,4], col='black', lwd=10)
segments(4,CI_RAIL_INFRASTRUCTURE_NAZ[1,4], 4, CI_RAIL_INFRASTRUCTURE_NAZ[3,4], col='black', lwd=5)
matplot(t(CI_RAIL_INFRASTRUCTURE_INTERNAZ), type="l", 
        main="Bootstrap T 95% CI for the probability of 
        being late due to rail infrastructure, international routes",
        xlab="Year", col='red', , ylim=c(0.1,0.3), lty='solid', lwd=3)
points(1,CI_RAIL_INFRASTRUCTURE_INTERNAZ[1,1], col='black', lwd=10)
points(1,CI_RAIL_INFRASTRUCTURE_INTERNAZ[3,1], col='black', lwd=10)
segments(1,CI_RAIL_INFRASTRUCTURE_INTERNAZ[1,1], 1, CI_RAIL_INFRASTRUCTURE_INTERNAZ[3,1], col='black', lwd=5)
points(2,CI_RAIL_INFRASTRUCTURE_INTERNAZ[1,2], col='black', lwd=10)
points(2,CI_RAIL_INFRASTRUCTURE_INTERNAZ[3,2], col='black', lwd=10)
segments(2,CI_RAIL_INFRASTRUCTURE_INTERNAZ[1,2], 2, CI_RAIL_INFRASTRUCTURE_INTERNAZ[3,2], col='black', lwd=5)
points(3,CI_RAIL_INFRASTRUCTURE_INTERNAZ[1,3], col='black', lwd=10)
points(3,CI_RAIL_INFRASTRUCTURE_INTERNAZ[3,3], col='black', lwd=10)
segments(3,CI_RAIL_INFRASTRUCTURE_INTERNAZ[1,3], 3, CI_RAIL_INFRASTRUCTURE_INTERNAZ[3,3], col='black', lwd=5)
points(4,CI_RAIL_INFRASTRUCTURE_INTERNAZ[1,4], col='black', lwd=10)
points(4,CI_RAIL_INFRASTRUCTURE_INTERNAZ[3,4], col='black', lwd=10)
segments(4,CI_RAIL_INFRASTRUCTURE_INTERNAZ[1,4], 4, CI_RAIL_INFRASTRUCTURE_INTERNAZ[3,4], col='black', lwd=5)

#cosa si evince: che la qualità delle rail infrastructure sia migliorata nel corso degli anni,
#per le tratte nazionali. Quelle internazionali hanno una maggior varianza
# Bisogna prendere il matplot dei ritardi per poter trarre delle migliori conclusioni

#E' GIUSTO FARLO COSI' o BISOGNA APPLICARE delle correzioni dovute agli anni?
# e se vi aggiungo delle altre covariate tipo altri tipi di cause di ritardo? Devo applicare
# una correzione?

x.bivar <- data.m1[, c(1,2)]
y.bivar <- data.m1[, c(4,5)]

depths.x <- DepthProc::depth(u=x.bivar, method="Tukey")
depths.y <- DepthProc::depth(u=y.bivar, method="Tukey")
cbind(depths.x, depths.y)

T0 <- cor(depths.x, depths.y)**2  #Pearson correlation coefficient on the depths
T0 #and then I iterate the process permuting it

Tvec <- NULL
set.seed(SEED)
for (k in 1:B){
  depths.x.perm <- sample(depths.x, replace=F)
  depths.y.perm <- sample(depths.y, replace=F)
  
  r.sq.perm <- cor(depths.x.perm, depths.y.perm)**2
  
  Tvec <- c(Tvec, r.sq.perm)
}

hist(Tvec)
abline(v=T0, col="pink")
sum(Tvec>=T0)/B < ALPHA