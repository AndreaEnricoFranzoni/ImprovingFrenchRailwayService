data=read_excel('trains_update_2610.xlsx')
N=dim(data)[1]

finestra_grafica=function(t){
  if(t==0) quartz()
  if(t==1) x11()
} #per usare il giusto comando per l'apertura della finestra grafica
t = 0 #0 per quartz, 1 per x11(): unica volta in cui viene usata la variabile t


library(tidyverse) #libreria che serve per lavorare con le stringhe
#io considero le unità definite unicamente da tratta_mese_anno: quindi ho n unità: le posso considerare tra loro indipendenti

tratte = vector(mode = "character", length = n)

for (i in 1:n) {
  tratte[i]=paste(data$route[i],paste(data$year[i], data$month[i], sep = " "), sep = " / ")
}

#l'idea è valutare la differenza di diverse risposte al fatto che siano tratte nazionali o internazionali:
#ogni unità è una tratta, in un determinato mese in un determinato anno
servizio = factor(data$service)
livelli_servizio = levels(servizio)
g=length(livelli_servizio)
g1 = length(which(servizio==livelli_servizio[2])) #cardinalità nazionali
g2 = length(which(servizio==livelli_servizio[1])) #cardinalità internazionali


#tutta la prima parte di analisi valuta risposte univariate

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

#le due distrinuzioni sembrano essere differenti: sebbene la proporzione di dati che sono outliers sia la stessa
#nei due gruppi: per essere un outlier dei viaggi nazionali, devi avere almeno 10 cancellazioni, mentre
#per i viaggi internazionali, almeno 2: da questa prima analisi molto terra-terra, si evince come
#un treno nazionale sia più facilmente soggetto a cancellazioni

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

perm_median_test(x,y)
#questo è il pvalue: the two distributions are different since they have two different medians

x=nazionali_1
y=internazionali_1
iter=1e3
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
fit <- aov(num_of_canceled_trains ~ service, data = dati_1)
summary(fit)

T0 <- summary(fit)[[1]][1,4]  # extract the test statistic
T0
#To compute the permutational distribution, I assign at random the treatments (that, under H0, should all be equal, and equal to 0)
T_stat <- numeric(B) 
n <- dim(dati_1)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  num_canc_perm <- dati_1$num_of_canceled_trains[permutation]    #sto cambiando esclusivamente la parte categorica dei dati
  fit_perm <- aov(num_canc_perm ~ service,data = dati_1)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

#plot of the permutational distribution
finestra_grafica(t)
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

finestra_grafica(t)
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val

#il fatto che la tratta sia nazionale/internazionale influenza pesantemente il numero di treni cancellati