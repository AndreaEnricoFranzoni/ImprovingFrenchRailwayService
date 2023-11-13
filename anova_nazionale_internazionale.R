library(readxl)
data=read_excel('trains_update_2610.xlsx')
N=dim(data)[1]
set.seed(1)

#cose dA VEDERE:
#punto 2_1
#punto 3_2: come trattare quegli outliers nazionali?

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

#l'idea è valutare la differenza di diverse risposte al fatto che siano tratte nazionali o internazionali:
#ogni unità è una tratta, in un determinato mese in un determinato anno
servizio = factor(data$service)
livelli_servizio = levels(servizio)
g=length(livelli_servizio)
g1 = length(which(servizio==livelli_servizio[2])) #cardinalità nazionali
g2 = length(which(servizio==livelli_servizio[1])) #cardinalità internazionali

#fino a qui va fatto runnare, poi ogni punto può essere fatto runnare a parte
#tutta la prima parte di analisi valuta risposte univariate

#PUNTO 1
###primo caso: valuto il numero di treni cancellati (che è una variabile intera) vs categoria
colnames(data)
#s = 8
#nome_colonna = tolower(colnames(data)[s])
dati_1 = data.frame( num_of_canceled_trains = data$num_of_canceled_trains,service = servizio, row.names = tratte)
nazionali_1 = dati_1[which(dati_1$service==livelli_servizio[2]),1]
internazionali_1 = dati_1[which(dati_1$service==livelli_servizio[1]),1]

#siamo in 1D, quindi la Tukey depth sarà calcolata utilizzando il boxplot
tukey_depth_1=depth(u=dati_1[,1],method='Tukey')
tukey_median_1=dati_1[which.max(tukey_depth_1),] 
tukey_median_1
tukey_depth_1_naz=depth(u=nazionali_1,method='Tukey')
tukey_median_1_naz=dati_1[which.max(tukey_depth_1_naz),] 
tukey_median_1_naz
tukey_depth_1_int=depth(u=internazionali_1,method='Tukey')
tukey_median_1_int=dati_1[which.max(tukey_depth_1_int),] 
tukey_median_1_int

finestra_grafica(t)
#sembra esserci una grande differenza
d1=boxplot(num_of_canceled_trains ~ service, data = dati_1)
d1$stats

d1_naz=boxplot(nazionali_1)
d1_naz$stats
length(d1_naz$out)  #615/4854 outliers
summary(d1_naz$out) #media di 54.71 treni cancellati negli outlier


d1_int=boxplot(internazionali_1)
d1_int$stats
length(d1_int$out)  #80/608 outliers
summary(d1_int$out) #media di 17.32 treni cancellati negli outlier

#le due distribuzioni sembrano essere differenti: sebbene la proporzione di dati che sono outliers sia la stessa
#nei due gruppi: per essere un outlier dei viaggi nazionali, devi avere almeno 10 cancellazioni, mentre
#per i viaggi internazionali, almeno 2: da questa prima analisi molto terra-terra, si evince come
#un treno nazionale sia più facilmente soggetto a cancellazioni: sarà probabilmente dovuto al fatto che ne partono di più

#voglio vedere se vi sia una differenza: come prima cosa controllo se le due distribuzioni possano essere
#considerate circa simili, usando come statistica del permutation test la differenza delle mediane
# H0: (#canceled_national) =<d> (#canceled_international) vs H1: different

#funzione per fare il test(permutational)
perm_median_test=function(x,y,iter=1e3){
  
  
  T0=abs(median(x)-median(y))  # test statistic: here we use the medians instead of the means
  T_stat=numeric(iter)
  x_pooled=c(x,y)
  n=length(x_pooled)
  n1=length(x)
  for(perm in 1:iter){
    # permutation:
    permutation <- sample(1:n)
    x_perm <- x_pooled[permutation]
    x1_perm <- x_perm[1:n1]
    x2_perm <- x_perm[(n1+1):n]
    # test statistic:
    T_stat[perm] <- abs(median(x1_perm) - median(x2_perm))
  }
  # p-value
  p_val <- sum(T_stat>=T0)/iter
  return(p_val)
  
}


x=nazionali_1
y=internazionali_1
iter=1e3
perm_median_test(x,y)
#questo è il pvalue: the two distributions are different since they have two different medians
T0=abs(median(x)-median(y))  # test statistic: here we use the medians instead of the means
T_stat=numeric(iter)
x_pooled=c(x,y)
n=length(x_pooled)
n1=length(x)
for(perm in 1:iter){
  # permutation:
  permutation <- sample(1:n)
  x_perm <- x_pooled[permutation]
  x1_perm <- x_perm[1:n1]
  x2_perm <- x_perm[(n1+1):n]
  # test statistic:
  T_stat[perm] <- abs(median(x1_perm) - median(x2_perm))
}

#questi plot sono assai brutti
finestra_grafica(t)
hist(T_stat,xlim=range(c(T_stat,T0)))
abline(v=T0,col=3,lwd=4)

finestra_grafica(t)
plot(ecdf(T_stat))
abline(v=T0,col=3,lwd=4)

#per ultima cosa, fitto un'ANOVA non parametrica
B=1e4
fit_1 <- aov(num_of_canceled_trains ~ service, data = dati_1)
summary(fit_1)

T0_1 <- summary(fit_1)[[1]][1,4]  # extract the test statistic
T0_1
#To compute the permutational distribution, I assign at random the treatments (that, under H0, should all be equal, and equal to 0)
T_stat_1 <- numeric(B) 
n <- dim(dati_1)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  num_canc_perm <- dati_1$num_of_canceled_trains[permutation]    #sto cambiando esclusivamente la parte categorica dei dati
  fit_perm <- aov(num_canc_perm ~ dati_1$service )
  
  # Test statistic:
  T_stat_1[perm] <- summary(fit_perm)[[1]][1,4]
}

#plot of the permutational distribution
finestra_grafica(t)
hist(T_stat_1,xlim=range(c(T_stat_1,T0_1)),breaks=30, main='Permutational distribution of statistic in the ANOVA_1')
abline(v=T0_1,col=3,lwd=2)

finestra_grafica(t)
plot(ecdf(T_stat_1),xlim=c(-1,20), main='ECDF of the statistic in the ANOVA_1')
abline(v=T0_1,col=3,lwd=4)

# p-value
p_val_1 <- sum(T_stat>=T0_1)/B
p_val_1
#il pvalue è circa 0: il numero di treni cancellati nelle due tipologie di tratte è diverso


#PUNTO 1_1
#il fatto che la tratta sia nazionale/internazionale influenza pesantemente il numero di treni cancellati: però potrebbe essere
#un qualcosa dovuto semplicemente al fatto che vi siano più treni nazionali piuttosto che internazionali: ripeto la stessa identica
#analisi prendendo come variabile di risposta il numero di treni_cancellati/numero di treni totali per ogni tratta
dati_1_1 = data.frame( prop_of_canceled_trains = data$num_of_canceled_trains/data$total_num_trips,service = servizio, row.names = tratte)
nazionali_1_1 = dati_1_1[which(dati_1$service==livelli_servizio[2]),1]
internazionali_1_1 = dati_1_1[which(dati_1$service==livelli_servizio[1]),1]

finestra_grafica(t)
d1_1=boxplot(prop_of_canceled_trains ~ service, data = dati_1_1)
d1_1$stats
#anche in questo caso sembra esservici un differenza nelle due distribuzioni

d1_1_naz=boxplot(nazionali_1_1)
d1_1_naz$stats
length(d1_1_naz$out)  #573/4854 outliers
summary(d1_1_naz$out) #media di 0.18848% dei treni cancellati negli outlier


d1_1_int=boxplot(internazionali_1_1)
d1_1_int$stats
length(d1_1_int$out)  #92/608 outliers
summary(d1_1_int$out) #media di 0.14756% treni cancellati negli outlier
#negli outliers, le distribuzioni sembrano essersi "avvicinate"


#ANOVA nonparametrica per vedere se vi siano differenze nelle proporzioni di treni cancellati lungo i 4 anni
B=1e4
fit_1_1 <- aov(prop_of_canceled_trains ~ service, data = dati_1_1)
summary(fit_1_1)

T0_1_1 <- summary(fit_1_1)[[1]][1,4]  # extract the test statistic
T0_1_1
#To compute the permutational distribution, I assign at random the treatments (that, under H0, should all be equal, and equal to 0)
T_stat_1_1 <- numeric(B) 
n <- dim(dati_1_1)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  prop_canc_perm <- dati_1_1$prop_of_canceled_trains[permutation]    #sto cambiando esclusivamente la parte categorica dei dati
  fit_perm <- aov(prop_canc_perm ~ dati_1_1$service) 
  
  # Test statistic:
  T_stat_1_1[perm] <- summary(fit_perm)[[1]][1,4]
}

#plot of the permutational distribution
finestra_grafica(t)
hist(T_stat_1_1,xlim=range(c(T_stat_1_1,T0_1_1)),breaks=30, main='Permutational distribution of statistic in the ANOVA_1_1')
abline(v=T0_1_1,col=3,lwd=2)

finestra_grafica(t)
plot(ecdf(T_stat_1_1),xlim=c(-1,20), main='ECDF of the statistic in the ANOVA_1_1')
abline(v=T0_1_1,col=3,lwd=4)

# p-value
p_val_1_1 <- sum(T_stat_1_1>=T0_1_1)/B
p_val_1_1

#il p-value è abbastanza alto in questo caso, circq 17%: non ho evidenza per rifiutare l'ipotesi secondo cui
#la proporzione di treni cancellati differisca tra tratte nazionali ed internazionali


#PUNTO 2_1
#Adesso faccio esattamente lo stesso ragionamento per il numero di treni in ritardo:
#valuto essenzialmente la proporzione di essi: per prima cosa quelli che partono in ritardo
dati_2_1 = data.frame( prop_of_late_at_departure_trains = data$num_late_at_departure/data$total_num_trips,service = servizio, row.names = tratte)
nazionali_2_1 = dati_2_1[which(dati_2_1$service==livelli_servizio[2]),1]
internazionali_2_1 = dati_2_1[which(dati_2_1$service==livelli_servizio[1]),1]

finestra_grafica(t)
d2_1=boxplot(prop_of_late_at_departure_trains ~ service, data = dati_2_1)
d2_1$stats
#sembra che in media siano circa uguali, sebbene il terzo percentile degli internazionali sia circa lo 0.237
#mentre quello dei nazionali 0.176

finestra_grafica(t)
d2_1_naz=boxplot(nazionali_2_1)
d2_1_naz$stats
length(d2_1_naz$out)  #402/4854=0.08281829 sono outliers
summary(d2_1_naz$out) #media di 0.5262 dei treni in ritardo negli outlier per i nazionali

finestra_grafica(t)
d2_1_int=boxplot(internazionali_2_1)
d2_1_int$stats
length(d2_1_int$out)  #61/608=0.1003289 sono outliers
summary(d2_1_int$out) #media di 0.7479 treni in ritardo negli outlier
#negli outliers, che in proporzione sono circa lo stesso numero, la proporzione è molto molto 
#diversa rispetto ai valori nei box: ci sono delle tratte specifiche in cui
#i treni tendono a partire maggiormente in ritardo, in maniera quasi dominante


#ANOVA nonparametrica per vedere se vi siano differenze nelle proporzioni di treni in ritardo alla partenza lungo i 4 anni
B=1e4
fit_2_1 <- aov(prop_of_late_at_departure_trains ~ service, data = dati_2_1)
summary(fit_2_1)

T0_2_1 <- summary(fit_2_1)[[1]][1,4]  # extract the test statistic
T0_2_1
#To compute the permutational distribution, I assign at random the treatments (that, under H0, should all be equal, and equal to 0)
T_stat_2_1 <- numeric(B) 
n <- dim(dati_2_1)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  prop_of_late_at_departure_perm <- dati_2_1$prop_of_late_at_departure_trains[permutation]    #sto cambiando esclusivamente la parte categorica dei dati
  fit_perm <- aov(prop_of_late_at_departure_perm ~ dati_2_1$service) 
  
  # Test statistic:
  T_stat_2_1[perm] <- summary(fit_perm)[[1]][1,4]
}

#plot of the permutational distribution
finestra_grafica(t)
hist(T_stat_2_1,xlim=range(c(T_stat_2_1,T0_2_1)),breaks=30, main='Permutational distribution of statistic in the ANOVA_2_1')
abline(v=T0_2_1,col=3,lwd=2)

finestra_grafica(t)
plot(ecdf(T_stat_2_1),xlim=c(-1,20), main='ECDF of the statistic in the ANOVA_2_1')
abline(v=T0_2_1,col=3,lwd=4)

# p-value
p_val_2_1 <- sum(T_stat_2_1>=T0_2_1)/B
p_val_2_1
#pvalue circa 0: la proporzione di treni in ritardo alla partenza è maggiore nei viaggi internazionali
#piuttosto che quella di quelli nazionali

#sarebbe interessante vedere bene questi outliers



#PUNTO 2_2
#valuto essenzialmente la proporzione di treni che arrivano in ritardo
dati_2_2 = data.frame( prop_of_late_at_arriving_trains = data$num_arriving_late/data$total_num_trips,service = servizio, row.names = tratte)
nazionali_2_2 = dati_2_2[which(dati_2_2$service==livelli_servizio[2]),1]
internazionali_2_2 = dati_2_2[which(dati_2_2$service==livelli_servizio[1]),1]

finestra_grafica(t)
d2_2=boxplot(prop_of_late_at_arriving_trains ~ service, data = dati_2_2)
d2_2$stats
#le due distribuzioni sembrano essere circa simili, sebbene vi sono molti più outliers, in proporzione
#per i viaggi internazionali (sensata come cosa, dal momento che essendo un viaggio più lungo potrebbero
#occorrere più imprevisti)

finestra_grafica(t)
d2_2_naz=boxplot(nazionali_2_2)
d2_2_naz$stats
length(d2_2_naz$out)  #104/4854 outliers
summary(d2_2_naz$out) #media di 0.3743 dei treni in ritardo negli outlier per i nazionali

finestra_grafica(t)
d2_2_int=boxplot(internazionali_2_2)
d2_2_int$stats
length(d2_2_int$out)  #21/608 outliers
summary(d2_2_int$out) #media di 0.4381 treni in ritardo negli outlier

#ANOVA nonparametrica per vedere se vi siano differenze nelle proporzioni di treni in ritardo all'arrivo lungo i 4 anni
B=1e4
fit_2_2 <- aov(prop_of_late_at_arriving_trains ~ service, data = dati_2_2)
summary(fit_2_2)

T0_2_2 <- summary(fit_2_2)[[1]][1,4]  # extract the test statistic
T0_2_2
#To compute the permutational distribution, I assign at random the treatments (that, under H0, should all be equal, and equal to 0)
T_stat_2_2 <- numeric(B) 
n <- dim(dati_2_2)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  prop_of_late_at_arriving_perm <- dati_2_2$prop_of_late_at_arriving_trains[permutation]    #sto cambiando esclusivamente la parte categorica dei dati
  fit_perm <- aov(prop_of_late_at_arriving_perm ~ dati_2_2$service) 
  
  # Test statistic:
  T_stat_2_2[perm] <- summary(fit_perm)[[1]][1,4]
}

#plot of the permutational distribution
finestra_grafica(t)
hist(T_stat_2_2,xlim=range(c(T_stat_2_2,T0_2_2)),breaks=30, main='Permutational distribution of statistic in the ANOVA_2_2')
abline(v=T0_2_2,col=3,lwd=2)

finestra_grafica(t)
plot(ecdf(T_stat_2_2),xlim=c(-1,20), main='ECDF of the statistic in the ANOVA_2_2')
abline(v=T0_2_2,col=3,lwd=4)

# p-value
p_val_2_2 <- sum(T_stat_2_2>=T0_2_2)/B
p_val_2_2
#il pvalue è circa 0.0066: puoi considerare le distribuzioni delle due proporzioni circa uguali

#cosa si è ottenuto da questi due punti: che la proporzione di treni nazionali che partono in ritardo
#è maggiore di quella degli internazionali, con alcune tratte in cui il ritardo è quasi una costante
#di contro, nei due tipi di servizi, la proporzione di treni che arrivano in ritardo è circa
#uguale, sebbene la percentuale di outliers internazionali sia maggiore (una tratta più lunga è
#verosimilmente maggiormante soggetta ad imprevisti)
#IMPORTANTE: in questo caso, è bene notare come outliers=maggior numero di treni in ritardo/cancellati

#LIMITE DI TUTTO CIO': NULLA CI DICE CHE SE NUMERO_PARTENZE_TARDI=NUMERO_ARRIVI_TARDI, SIGNIFICA
#CHE TUTTI I TRENI PARTITI TARDI SIANO ANCHE ARRIVATI TARDI, ECT
#UN TRENO CHE PARTE TARDI PUO' ARRIVARE IN ORARIO O MENO, COSI' COME UNO CHE PARTE GIUSTO PUO'
#ARRIVARE TARDI O MENO: L'ANALISI VA TENUTA SEPARATA


#PUNTO 3_1
#In questo caso voglio valutare il ritardo medio alla partenza tra tutti i treni in ritardo,
#a seconda del tipo di servizio erogato
dati_3_1 = data.frame( avg_delay_dep = data$avg_delay_late_at_departure,service = servizio, row.names = tratte)
nazionali_3_1 = dati_3_1[which(dati_3_1$service==livelli_servizio[2]),1]
internazionali_3_1 = dati_3_1[which(dati_3_1$service==livelli_servizio[1]),1]

finestra_grafica(t)
d3_1=boxplot(avg_delay_dep ~ service, data = dati_3_1, main='Boxplot ritardi alla partenza')
d3_1$stats
#i box sembrano essere circa simili, sebbene il ritardo medio sia leggermente più alto per
#i nazionali tra i treni che partono in ritardo

finestra_grafica(t)
d3_1_naz=boxplot(nazionali_3_1)
d3_1_naz$stats
length(d3_1_naz$out)  #180/4854=0.03708282 sono outliers
summary(d3_1_naz$out) #media di 42.77 minuti di ritardo dei treni in ritardo negli outlier per i nazionali:
#la media del ritardo si alza di 26.77 minuti

finestra_grafica(t)
d3_1_int=boxplot(internazionali_3_1)
d3_1_int$stats
length(d3_1_int$out)  #26/608=0.04276316 outliers
summary(d3_1_int$out) #media di 54.76 di minuti di ritardo per treni in ritardo negli outlier:
#la media del ritardo si alza di 40.2297 minuti
dati_3_1[which(dati_3_1$avg_delay_dep==(max(dati_3_1$avg_delay_dep))),]
dati_3_1[which(dati_3_1$avg_delay_dep==(max(dati_3_1[-which(dati_3_1$avg_delay_dep==(max(dati_3_1$avg_delay_dep))),]$avg_delay_dep))),]
#è importante notare come vi siano questi due valori che sono palesemnete due casi eccezionali:
#adesso provo a valutare il tutto con e senza queste due unità

#ANOVA nonparametrica per vedere se vi siano differenze nei ritardi medi di treni in ritardo alla partenza lungo i 4 anni
#tengo tutti i dati
B=1e4
fit_3_1_1 <- aov(avg_delay_dep ~ service, data = dati_3_1)
summary(fit_3_1_1)

T0_3_1_1 <- summary(fit_3_1_1)[[1]][1,4]  # extract the test statistic
T0_3_1_1
#To compute the permutational distribution, I assign at random the treatments (that, under H0, should all be equal, and equal to 0)
T_stat_3_1_1 <- numeric(B) 
n <- dim(dati_3_1)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  avg_delay_dep_perm <- dati_3_1$avg_delay_dep[permutation]    #sto cambiando esclusivamente la parte categorica dei dati
  fit_perm <- aov(avg_delay_dep_perm ~ dati_3_1$service) 
  
  # Test statistic:
  T_stat_3_1_1[perm] <- summary(fit_perm)[[1]][1,4]
}

#plot of the permutational distribution
finestra_grafica(t)
hist(T_stat_3_1_1,xlim=range(c(T_stat_3_1_1,T0_3_1_1)),breaks=30, main='Permutational distribution of statistic in the ANOVA_3_1')
abline(v=T0_3_1_1,col=3,lwd=2)

finestra_grafica(t)
plot(ecdf(T_stat_3_1_1),xlim=c(-1,20), main='ECDF of the statistic in the ANOVA_3_1')
abline(v=T0_3_1_1,col=3,lwd=4)

# p-value
p_val_3_1_1 <- sum(T_stat_3_1_1>=T0_3_1_1)/B
p_val_3_1_1
#il pvalue=0.5929 è molto alto: posso considerare le due distribuzioni come uguali.
#Ora però provo a togliere quelle due osservazioni

#PUNTO 3_1_2
#tolgo quelle due osservazioni
i1=which(dati_3_1$avg_delay_dep==(max(dati_3_1$avg_delay_dep)))
i2=which(dati_3_1$avg_delay_dep==(max(dati_3_1[-which(dati_3_1$avg_delay_dep==(max(dati_3_1$avg_delay_dep))),]$avg_delay_dep)))
dati_3_1_2 = dati_3_1[-c(i1,i2),]
nazionali_3_1_2 = dati_3_1_2[which(dati_3_1_2$service==livelli_servizio[2]),1]
internazionali_3_1_2 = dati_3_1_2[which(dati_3_1_2$service==livelli_servizio[1]),1]

finestra_grafica(t)
d3_1_2=boxplot(avg_delay_dep ~ service, data = dati_3_1_2)
d3_1_2$stats

#ANOVA
B=1e4
fit_3_1_2 <- aov(avg_delay_dep ~ service, data = dati_3_1_2)
summary(fit_3_1_2)

T0_3_1_2 <- summary(fit_3_1_2)[[1]][1,4]  # extract the test statistic
T0_3_1_2
#To compute the permutational distribution, I assign at random the treatments (that, under H0, should all be equal, and equal to 0)
T_stat_3_1_2 <- numeric(B) 
n <- dim(dati_3_1_2)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  avg_delay_dep_perm <- dati_3_1_2$avg_delay_dep[permutation]    #sto cambiando esclusivamente la parte categorica dei dati
  fit_perm <- aov(avg_delay_dep_perm ~ dati_3_1_2$service) 
  
  # Test statistic:
  T_stat_3_1_2[perm] <- summary(fit_perm)[[1]][1,4]
}

#plot of the permutational distribution
finestra_grafica(t)
hist(T_stat_3_1_2,xlim=range(c(T_stat_3_1_2,T0_3_1_2)),breaks=30, main='Permutational distribution of statistic in the ANOVA_3_1_2')
abline(v=T0_3_1_2,col=3,lwd=2)

finestra_grafica(t)
plot(ecdf(T_stat_3_1_2),xlim=c(-1,20), main='ECDF of the statistic in the ANOVA_3_1_2')
abline(v=T0_3_1_2,col=3,lwd=4)

# p-value
p_val_3_1_2 <- sum(T_stat_3_1_2>=T0_3_1_2)/B
p_val_3_1_2
#pvalue=0.0792: nonostante siano stati tolti quei due outliers, il ritardo medio dei treni in ritardo alla partenza 
#è lo stesso


#PUNTO 3_2
#Ora valuto il ritardo sui treni all'arrivo, tra i treni in ritardo
dati_3_2 = data.frame( avg_delay_arr = data$avg_delay_late_on_arrival,service = servizio, row.names = tratte)
nazionali_3_2 = dati_3_2[which(dati_3_2$service==livelli_servizio[2]),1]
internazionali_3_2 = dati_3_2[which(dati_3_2$service==livelli_servizio[1]),1]

finestra_grafica(t)
d3_2=boxplot(avg_delay_arr ~ service, data = dati_3_2, main="Boxplot ritardo all'arrivo")
d3_2$stats
#i box sembrano essere circa simili, sebbene il ritardo medio sia leggermente più alto per
#gli internazionali tra i treni che arrivano in ritardo

finestra_grafica(t)
d3_2_naz=boxplot(nazionali_3_2)
d3_2_naz$stats
length(d3_2_naz$out)  #136/4854=0.02801813 sono outliers
summary(d3_2_naz$out) #media di 73.63 minuti di ritardo dei treni in ritardo negli outlier per i nazionali
#la media del ritardo all'arrivo si alza di 43.04134 minuti

finestra_grafica(t)
d3_2_int=boxplot(internazionali_3_2)
d3_2_int$stats
length(d3_2_int$out)  #23/608=0.03782895 outliers
summary(d3_2_int$out) #media di 81.79 di minuti di ritardo per treni in ritardo negli outlier:
#la media del ritardo all'arrivo si alza di 49.94159

#prima ho detto che l'analisi di ritardo in partenza e in arrivo vada fatta separatamente: 
#è verosimile che un treno che parte con 2/3 minuti di ritardo arrivi in orario, recuperando. O viceversa,
#anche se parte in orario, che arrivi con un piccolo ritardo. Pertanto non è possibile stabilire una corrispondenza
#fra la proporzione di treni in ritardo all'andata e all'arrivo.
#Ovviamente è sensato però che treni che abbiano un grande ritardo fin dalla partenza possano si recuperare qualcosa,
#ma è più verosimile che possano accumularne ancora.
#Queso boxplot mostra proprio questo: di come il ritardo medio all'arrivo sia maggiore di quello alla partenza,
#di come entrambi i baffi del boxplot si alzino

finestra_grafica(t)
par(mfrow=c(1,2))
boxplot(avg_delay_dep ~ service, data = dati_3_1, main='Average delay at the departure', ylim=c(0,300), col=c('blue','red'), xlab='Type of route', ylab='Minutes')
legend('top', legend=livelli_servizio, fill=c('blue','red'), cex=.7)
boxplot(avg_delay_arr ~ service, data = dati_3_2, main="Average delay at the arrival", ylim=c(0,300), col=c('blue','red'),  xlab='Type of route', ylab='Minutes')
legend('top', legend=livelli_servizio, fill=c('blue','red'), cex=.7)

#inoltre, penso sia importante notare come, 'ad occhio', i treni internazionali sembrano semplicemente
#accumulare del ritardo, mentre vi sono delle treni nazionali, in un determinato mese, che hanno
#un aumento gigante del ritardo (la proporzione di outliers è circa la stessa): bisogna concentrarsi su questi



#ANOVA nonparametrica per vedere se vi siano differenze nei ritardi medi di treni in ritardo all'arrivo lungo i 4 anni
#tengo tutti i dati
B=1e4
fit_3_2 <- aov(avg_delay_arr ~ service, data = dati_3_2)
summary(fit_3_2)

T0_3_2 <- summary(fit_3_2)[[1]][1,4]  # extract the test statistic
T0_3_2
#To compute the permutational distribution, I assign at random the treatments (that, under H0, should all be equal, and equal to 0)
T_stat_3_2 <- numeric(B) 
n <- dim(dati_3_2)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  avg_delay_arr_perm <- dati_3_2$avg_delay_arr[permutation]    #sto cambiando esclusivamente la parte categorica dei dati
  fit_perm <- aov(avg_delay_arr_perm ~ dati_3_2$service) 
  
  # Test statistic:
  T_stat_3_2[perm] <- summary(fit_perm)[[1]][1,4]
}

#plot of the permutational distribution
finestra_grafica(t)
hist(T_stat_3_2,xlim=range(c(T_stat_3_2,T0_3_2)),breaks=30, main='Permutational distribution of statistic in the ANOVA_3_2')
abline(v=T0_3_2,col=3,lwd=2)

finestra_grafica(t)
plot(ecdf(T_stat_3_2),xlim=c(-1,20), main='ECDF of the statistic in the ANOVA_3_2')
abline(v=T0_3_2,col=3,lwd=4)

# p-value
p_val_3_2 <- sum(T_stat_3_2>=T0_3_2)/B
p_val_3_2
#pvalue circa 0: c'è una differenza tra i ritardi, in distribuzione, a seconda che siano 
#tratte nazionali o internazionali

#Quindi ricapitolando:
#prendendo come unità una tratta specifica in un mese specifico tra il 2015 e il 2018:
#     -la proporzione di treni cancellati non differisce per il tipo di tratta
#     -la proporzione di treni in ritardo alla partenza è maggiore nei viaggi internazionali che in quelli nazionali
#     -il ritardo medio alla partenza è lo stesso, mentre non lo è all'arrivo: la coda dei nazionali è molto più
#       pesante, con alcuni ritardi che sono incrementati tantissimo


#PUNTO 4
#Ora provo a valutare una risposta non scalare, ovvero le cause dei ritardi (non differenzio tra andata e ritorno)
dati_4 = data.frame(external_causes= data$delay_cause_external_cause, rail_infrastructure=data$delay_cause_rail_infrastructure,
                   traffic_management=data$delay_cause_traffic_management, rolling_stock = data$delay_cause_rolling_stock,
                   station_management=data$delay_cause_station_management, travelers=data$delay_cause_travelers, service = servizio, row.names = tratte)
nazionali_4 = dati_4[which(dati_4$service==livelli_servizio[2]),1]
internazionali_4 = dati_4[which(dati_4$service==livelli_servizio[1]),1]


finestra_grafica(t)
par(mfrow=c(3,2))
boxplot(external_causes ~ service, data = dati_4, main="Delay due to external causes", xlab='Route', ylab="Proportion", col=c('blue','red'))
legend('top', legend=livelli_servizio, fill=c('blue','red'), cex=.7)
boxplot(rail_infrastructure ~ service, data = dati_4, main='Delay due to rail infrastructure', xlab='Route', ylab="Proportion", col=c('blue','red'))
legend('top', legend=livelli_servizio, fill=c('blue','red'), cex=.7)
boxplot(traffic_management ~ service, data = dati_4, main='Delay due to traffic management',  xlab='Route', ylab="Proportion", col=c('blue','red'))
legend('top', legend=livelli_servizio, fill=c('blue','red'), cex=.7)
boxplot(rolling_stock ~ service, data = dati_4, main='Delay due to rolling stock', xlab='Route', ylab="Proportion", col=c('blue','red'))
legend('top', legend=livelli_servizio, fill=c('blue','red'), cex=.7)
boxplot(station_management ~ service, data = dati_4, main='Delay due to station management', xlab='Route', ylab="Proportion", col=c('blue','red'))
legend('top', legend=livelli_servizio, fill=c('blue','red'), cex=.7)
boxplot(travelers ~ service, data = dati_4, main='Delay due to travelers', xlab='Route', ylab="Proportion", col=c('blue','red'))
legend('top', legend=livelli_servizio, fill=c('blue','red'), cex=.7)

#I see differences in almost every response
#external causes is not so indicative, since it does not indicate anything about what happened
#rail infrastructure: affects more the national routes
#traffic management: affects more the international routes
#rolling stock: it appears to have some sort of equal distribution
#station management: it is some sort of similar
#station management: it makes sense that affects more national routes since you have usually more stops
#travelers: also, here, I suppose it is a sort of I have to wait for some passengers that are late.
#           So, it makes sense since in tha national routes you have more stops.

#The idea is: I perform firstly a MANOVA to see if the 6-dim distribution varies depending on the type of service

B=1e4
fit <- manova(as.matrix(dati_4[,1:6]) ~ dati_4$service)
summary.manova(fit,test="Wilks") 
T0_4 <- -summary.manova(fit,test="Wilks")$stats[1,2]
T0_4
T_stat_4 <- numeric(B)
n=dim(dati_4)[1]

for(perm in 1:B){
  # choose random permutation
  permutation <- sample(1:n)
  service.perm <- dati_4$service[permutation]
  fit.perm <- manova(as.matrix(dati_4[,1:6]) ~ service.perm)
  T_stat_4[perm] <- -summary.manova(fit.perm,test="Wilks")$stats[1,2]
}

#plot of the permutational distribution
finestra_grafica(t)
hist(T_stat_4,xlim=range(c(T_stat_4,T0_4)),breaks=30, main='Permutational distribution of statistic in the MANOVA_4_1')
abline(v=T0_4,col=3,lwd=2)

finestra_grafica(t)
plot(ecdf(T_stat_4),xlim=c(-1,20), main='ECDF of the statistic in the MANOVA_4_1')
abline(v=T0_4,col=3,lwd=4)

# p-value
p_val_4 <- sum(T_stat_4>=T0_4)/B
p_val_4
#Il pvalue è praticamente nullo: rifiuto: provo a fare 6 ANOVA separatamente per vedere se per 
#quali tratte risentano maggiormente di alcune cose

#EXTERNAL CAUSES: risulta essere la più difficile da interpretare
B=1e4
fit_4_external <- aov(dati_4$external_causes ~ dati_4$service)
summary(fit_4_external)

T0_4_external <- summary(fit_4_external)[[1]][1,4]  # extract the test statistic
T0_4_external
#To compute the permutational distribution, I assign at random the treatments (that, under H0, should all be equal, and equal to 0)
T_stat_4_external <- numeric(B) 
n <- dim(dati_4)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  prop_perm <- dati_4$external_causes[permutation]    #sto cambiando esclusivamente la parte categorica dei dati
  fit_perm <- aov(prop_perm ~ dati_4$service) 
  
  # Test statistic:
  T_stat_4_external[perm] <- summary(fit_perm)[[1]][1,4]
}

#plot of the permutational distribution
finestra_grafica(t)
hist(T_stat_4_external,xlim=range(c(T_stat_4_external,T0_4_external)),breaks=30, main='Permutational distribution of statistic in the ANOVA_4_external')
abline(v=T0_4_external,col=3,lwd=2)

finestra_grafica(t)
plot(ecdf(T_stat_4_external),xlim=c(-1,20), main='ECDF of the statistic in the ANOVA_4_external')
abline(v=T0_4_external,col=3,lwd=4)

# p-value
p_val_4_external <- sum(T_stat_4_external>=T0_4_external)/B
p_val_4_external
#pvalue nullo: vi è una differenza

finestra_grafica(t)
b4_external=boxplot(external_causes ~ service, data = dati_4, main="Delay due to external causes", xlab='Route', ylab="Proportion", col=c('blue','red'))
legend('top', legend=livelli_servizio, fill=c('blue','red'), cex=.7)
b4_external$stats
#i viaggi nazionali sono maggiormente soggetti a cause di ritardo esterne, quindi a veri e propri imprevisti

i_imprevisto = which(dati_4$external_causes==1) #sono le tratte in cui devo imputare tutte le cause del ritardo a cause esterne
i_imprevisto 
length(i_imprevisto)
data[i_imprevisto,c(3,7,8,10,11,14,15)] #prendo tipo di servizio, viaggi totali, numero di cancellati,
#in ritardo alla partenza, di quanto, in ritardo all'arrivo, di quanto
#queste sono le tratte in cui i ritardi sono stati dovuti tutti essere imputati a cause esterne:
row.names(dati_4[i_imprevisto,])
prop_ritardi_partenza=data[i_imprevisto,10]/data[i_imprevisto,7]
prop_ritardi_partenza
prop_ritardi_arrivo=data[i_imprevisto,14]/data[i_imprevisto,7]
prop_ritardi_arrivo
#la tratta BELLEGARDE (AIN) - PARIS LYON / 2018 7 ha un'altissima percentuale di ritardi (quasi il 50% sia a partire che ad arrivare nel luglio 2018) 
data[which(data$route=="BELLEGARDE (AIN) - PARIS LYON" & data$year=='2018' & data$month==7) ,c(7,10,11,14,15)]
#ci sono state 220 corse, e quasi metà di queste si sono rivelate essere ritardi, con un ritardo medio di
#partenza intorno ai 7 minuti, mentre di arrivo più di 30
#su internet non ho trovato nulla a riguardo di eclatante


#RAIL INFRASTRUCTURE: 
B=1e4
fit_4_rail_infrastructure <- aov(dati_4$rail_infrastructure ~ dati_4$service)
summary(fit_4_rail_infrastructure)

T0_4_rail_infrastructure <- summary(fit_4_rail_infrastructure)[[1]][1,4]  # extract the test statistic
T0_4_rail_infrastructure
#To compute the permutational distribution, I assign at random the treatments (that, under H0, should all be equal, and equal to 0)
T_stat_4_rail_infrastructure <- numeric(B) 
n <- dim(dati_4)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  prop_perm <- dati_4$rail_infrastructure[permutation]    #sto cambiando esclusivamente la parte categorica dei dati
  fit_perm <- aov(prop_perm ~ dati_4$service) 
  
  # Test statistic:
  T_stat_4_rail_infrastructure[perm] <- summary(fit_perm)[[1]][1,4]
}

#plot of the permutational distribution
finestra_grafica(t)
hist(T_stat_4_rail_infrastructure,xlim=range(c(T_stat_4_rail_infrastructure,T0_4_rail_infrastructure)),breaks=30, main='Permutational distribution of statistic in the ANOVA_4_rail_infrastructure')
abline(v=T0_4_rail_infrastructure,col=3,lwd=2)

finestra_grafica(t)
plot(ecdf(T_stat_4_rail_infrastructure),xlim=c(-1,20), main='ECDF of the statistic in the ANOVA_4_rail_infrastructure')
abline(v=T0_4_rail_infrastructure,col=3,lwd=4)

# p-value
p_val_4_rail_infrastructure <- sum(T_stat_4_rail_infrastructure>=T0_4_rail_infrastructure)/B
p_val_4_rail_infrastructure
#anche in questo caso il pvalue è 0

finestra_grafica(t)
b4_rail_infrastructure=boxplot(rail_infrastructure ~ service, data = dati_4, main="Delay due to rail infrastructure", xlab='Route', ylab="Proportion", col=c('blue','red'))
legend('top', legend=livelli_servizio, fill=c('blue','red'), cex=.7)
#dal boxplot si nota come la percentuale di ritardi dovuti alle rail infrastructure nei viaggi nazionali sia 
#stocasticamente greater di quella negli internazionali (la coda è anche più pesante)
b4_rail_infrastructure$stats
length((which(b4_rail_infrastructure$group==1))) 
length((which(b4_rail_infrastructure$group==2)))
#il baffo superiore si assesta circa a 0.6 per entrmabe le distribuzioni però
#analizzo le tratte in cui in un determinato mese e in un determinato anno questa percentuale sia stata più grande di 0.6
#in proporzione è 
# 11/608 = 0.01809211
# 91/4854 = 0.01874742
# la proporzione è sempre la stessa sul numero di tratte, identificando la tratta come un partenza-arrivo in un determinato mese e in un determinato anno
# però potrebbe essere utile valutare in queste tratte specifiche
# 1) quanto ritardo medio viene causato
# 2) valutare quanto la tratta sia trafficata effettivamente

treshold_nazionali=b4_rail_infrastructure$stats[10]
treshold_internazionali=b4_rail_infrastructure$stats[5]

out_ri_nazionali = data[which(data$delay_cause_rail_infrastructure>=treshold_nazionali & data$service==livelli_servizio[2]),]
out_ri_internazionali = data[which(data$delay_cause_rail_infrastructure>=treshold_internazionali & data$service==livelli_servizio[1]),]

colMeans(cbind(out_ri_nazionali$avg_delay_late_at_departure, out_ri_nazionali$avg_delay_late_on_arrival))
colMeans(cbind(out_ri_internazionali$avg_delay_late_at_departure, out_ri_internazionali$avg_delay_late_on_arrival))

#nelle tratte "outlier", il disagio medio causato sembra essere differente a seconda che la tratta sia nazionale o internazionale
#soprattutto per quanto concerne l'arrivo: l'accumulo del ritardo sembra ingrandirsi maggiormente in quelle tratte
# in cui le rail infrastructure non funzionano molto bene

#adesso sarebbe il caso di valutare più nel dettaglio queste tratte specifiche
#lo faccio anno per anno

#2015
anno = 2015

viaggi_totali_nazionali = sum(data[which(data$year==anno & data$service==livelli_servizio[2]),7])
viaggi_totali_nazionali_outliers = sum(data[which(out_ri_nazionali$year==anno & out_ri_nazionali$service==livelli_servizio[2]),7])
prop_naz_2015=viaggi_totali_nazionali_outliers/viaggi_totali_nazionali 

viaggi_totali_internazionali = sum(data[which(data$year==anno & data$service==livelli_servizio[1]),7])
viaggi_totali_internazionali_outliers = sum(data[which(out_ri_internazionali$year==anno & out_ri_internazionali$service==livelli_servizio[1]),7])
prop_inter_2015=viaggi_totali_internazionali_outliers/viaggi_totali_internazionali
#2015: la proporzione di viaggi che risentiva di uno scarso livello delle infrastrutture ferroviarie (scarso livello=grande ritardo dovuto ad esse) è
#     -nazionali: 0.02852151
#     -internazionali: 0.03876679

#2016
anno = 2016

viaggi_totali_nazionali = sum(data[which(data$year==anno & data$service==livelli_servizio[2]),7])
viaggi_totali_nazionali_outliers = sum(data[which(out_ri_nazionali$year==anno & out_ri_nazionali$service==livelli_servizio[2]),7])
prop_naz_2016=viaggi_totali_nazionali_outliers/viaggi_totali_nazionali 

viaggi_totali_internazionali = sum(data[which(data$year==anno & data$service==livelli_servizio[1]),7])
viaggi_totali_internazionali_outliers = sum(data[which(out_ri_internazionali$year==anno & out_ri_internazionali$service==livelli_servizio[1]),7])
prop_inter_2016=viaggi_totali_internazionali_outliers/viaggi_totali_internazionali
#2016: la proporzione di viaggi che risentiva di uno scarso livello delle infrastrutture ferroviarie (scarso livello=grande ritardo dovuto ad esse) è
#     -nazionali: 0.01535733
#     -internazionali: 0.06589384

#2017
anno = 2017

viaggi_totali_nazionali = sum(data[which(data$year==anno & data$service==livelli_servizio[2]),7])
viaggi_totali_nazionali_outliers = sum(data[which(out_ri_nazionali$year==anno & out_ri_nazionali$service==livelli_servizio[2]),7])
prop_naz_2017=viaggi_totali_nazionali_outliers/viaggi_totali_nazionali 

viaggi_totali_internazionali = sum(data[which(data$year==anno & data$service==livelli_servizio[1]),7])
viaggi_totali_internazionali_outliers = sum(data[which(out_ri_internazionali$year==anno & out_ri_internazionali$service==livelli_servizio[1]),7])
prop_inter_2017=viaggi_totali_internazionali_outliers/viaggi_totali_internazionali
#2017: la proporzione di viaggi che risentiva di uno scarso livello delle infrastrutture ferroviarie (scarso livello=grande ritardo dovuto ad esse) è
#     -nazionali: 0.02227834
#     -internazionali: 0.06991716

#2018
anno = 2018

viaggi_totali_nazionali = sum(data[which(data$year==anno & data$service==livelli_servizio[2]),7])
viaggi_totali_nazionali_outliers = sum(data[which(out_ri_nazionali$year==anno & out_ri_nazionali$service==livelli_servizio[2]),7])
prop_naz_2018=viaggi_totali_nazionali_outliers/viaggi_totali_nazionali 

viaggi_totali_internazionali = sum(data[which(data$year==anno & data$service==livelli_servizio[1]),7])
viaggi_totali_internazionali_outliers = sum(data[which(out_ri_internazionali$year==anno & out_ri_internazionali$service==livelli_servizio[1]),7])
prop_inter_2018=viaggi_totali_internazionali_outliers/viaggi_totali_internazionali
#2018: la proporzione di viaggi che risentiva di uno scarso livello delle infrastrutture ferroviarie (scarso livello=grande ritardo dovuto ad esse) è
#     -nazionali: 0.008825188
#     -internazionali: 0.01586637

dati_ri = data.frame(row.names =  c(2015,2016,2017,2018), c(prop_naz_2015,prop_naz_2016,prop_naz_2017,prop_naz_2018), c(prop_inter_2015,prop_inter_2016,prop_inter_2017,prop_inter_2018))
dati_viaggi = data.frame(row.names =  c(2015,2016,2017,2018), 
                         c(sum(data[which(data$year==2015 & data$service==livelli_servizio[2]),7]),sum(data[which(data$year==2016 & data$service==livelli_servizio[2]),7]),sum(data[which(data$year==2017 & data$service==livelli_servizio[2]),7]),sum(data[which(data$year==2018 & data$service==livelli_servizio[2]),7])),
                         c(sum(data[which(data$year==2015 & data$service==livelli_servizio[1]),7]),sum(data[which(data$year==2016 & data$service==livelli_servizio[1]),7]),sum(data[which(data$year==2017 & data$service==livelli_servizio[1]),7]),sum(data[which(data$year==2018 & data$service==livelli_servizio[1]),7])))
finestra_grafica(t)
par(mfrow=c(3,1))
matplot((dati_ri),type='l',main="Proportion of route that were affected by the bad rail infrastructure in a big way", xlab='Year', ylab="Proportion", col=c('blue','red'))
legend('topright', legend=livelli_servizio, fill=c('blue','red'), cex=.7)
matplot((dati_viaggi[,1]),type='l')
matplot((dati_viaggi[,2]),type='l')

