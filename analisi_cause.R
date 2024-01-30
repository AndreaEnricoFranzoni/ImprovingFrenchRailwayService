library(readxl)
#install.packages('compositions') #libreria che serve per trattare compositional data
library(compositions)
library(robustbase)

finestra_grafica = function(t){
  if (t==0)
    return(quartz())
  else 
    return(x11())
}
t=1 #per chiamare in automatico il quartz, mettere un qualsiasi altro valore per avere x11()

#valuto l'effetto delle cause anno per anno: lo faccio con il 2018 perchè sono le più recenti
data = read_excel('aggregated_trains_by_year_2701.xlsx')
n = dim(data)[1]

i_2015=which(data$year==2015)
i_2016=which(data$year==2016)
i_2017=which(data$year==2017)
i_2018=which(data$year==2018)
indexes=list(i_2015, i_2016, i_2017, i_2018)
nomi_cause=c('External','Rail infrastructure','Traffic management',
             'Rolling stock','Station management','Travelers') #se mettiamo questi come nomi delle colonne i plot successivi vengono un pochino più carini
cause=data[,14:19]
colnames(cause)=nomi_cause

#prendo le cause del 2018
# per il momento ne lascio 6, però sarebbe sensato accorpare le due non direttamente imputabili
# alla compagnia, e vedere il rapporto di ogni causa riguardante la compgnai singolarmente su quelle che non la riguardano
cause_2018=cause[i_2018,]
n_2018 = dim(cause_2018)[1]
cause_2018_comp=acomp(cause_2018) #è semplicemente un oggetto di classe compositional data, i dati non sono trasformati
#The general approach in analysing acomp objects is thus to perform classical multivariate analysis on clr/alr/ilr-transformed coordinates and to backtransform or display the results in such a way that they can be interpreted in terms of the original compositional parts.

#Trasformazione: cemtered log ratio transform
cause_2018_transformed=clr(cause_2018) #questi sono i dati trasformati: è un oggetto da S^6 in R^6:
#same transformation used by the SPCA

finestra_grafica(t)
plot(cause_2018_comp)         #plot dei simplessi

finestra_grafica(t)
barplot(colMeans(cause_2018_comp))

finestra_grafica(t)
plot(cause_2018_transformed) #scatter dei dati trasformati: COME INTERPRETARLI??

finestra_grafica(t)
boxplot(cause_2018_comp)




#idea: per prima cosa, dividere in due: cause dovute alla compagnia e no
cause_tricotomiche_2018 = data.frame( interne = cause_2018[,2]+cause_2018[,3]+cause_2018[,4]+cause_2018[,5], travelers = cause_2018[,6], other=cause_2018[,6] )
colnames(cause_tricotomiche_2018)=c('Internal','Traverles','Other')
cause_tricotomiche_2018_comp = acomp(cause_tricotomiche_2018)

finestra_grafica(t)
plot(cause_tricotomiche_2018_comp)  
#è un plot molto brutto, però è significativo: la maggior parte della colpa per i
#ritardi è da imputare all'azienda

#passo a un dataset bivariato
cause_dicotomiche_2018 = data.frame(interne = cause_tricotomiche_2018$Internal, esterne = cause_tricotomiche_2018$Traverles+cause_tricotomiche_2018$Other)
colnames(cause_dicotomiche_2018)=c('Internal','External')
cause_dicotomiche_2018_comp = acomp(cause_dicotomiche_2018)

finestra_grafica(t)
barplot(colMeans(cause_dicotomiche_2018_comp))

#devo vedere il rapporto tra due dati: isometric log ratio
cause_dicotomiche_2018_trans = ilr(cause_dicotomiche_2018_comp)

finestra_grafica(t)
plot(1:n_2018,cause_dicotomiche_2018_trans)



###########
pc <- princomp(acomp(cause_2018))
pc
summary(pc)
finestra_grafica(t)
plot(pc)
pc$Loadings
pc$scores
biplot(pc)  



# Per Andrea o chi lo farà: 
# Partire da regressione di avg_delay_late_at_arrival con tutti i loadings insieme e valutare se ci sono coefficienti non significativi.
# Se ci sono coeff non significativi, interpretare quali cause non sono da considerare secondo la regressione
# Utilizzare i loading con coefficiente significativo per fare robust multivariato a 2 a 2 (un loading e il delay ogni volta)
# per capire quali tratte sono outlier secondo quel loading (e dunque secondo le cause associate)
# è giusto farlo sul 2018 perché sono le più recenti e dunque possiamo dire alla compagnia di intervenire se ci sono risultati
# significativi


response = as.matrix(data[i_2018,13]) #avg_delay_late_at_arrival
covariates = pc$scores
x1 = covariates[,1]
x2 = covariates[,2]
x3 = covariates[,3]
x4 = covariates[,4]
x5 = covariates[,5]

fit = lm( response ~ x1 + x2 + x3 + x4 + x5)
summary(fit)

