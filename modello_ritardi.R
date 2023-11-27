library(readxl)
data=read_excel('trains_update_2610.xlsx')
N=dim(data)[1]
set.seed(1)

finestra_grafica=function(t){
  if(t==0) quartz()
  if(t==1) x11()
} #per usare il giusto comando per l'apertura della finestra grafica
t = 0 #0 per quartz, 1 per x11(): unica volta in cui viene usata la variabile t

library(tidyverse)
tratte = vector(mode = "character", length = N)
for (i in 1:N) {
  tratte[i]=paste(data$route[i],paste(data$year[i], data$month[i], sep = " "), sep = " / ")
}

servizio = factor(data$service)
livelli_servizio = levels(servizio)

#idea: fare un additive model che mi permetta di spiegare quale sia la probabilità di avere un treno in
#      ritardo alla partenza o all'arrivo, in ritardo di più di un tot in funzione delle varie
#      cause possibili, per poi poterci fare sopra sensitivity analysis, in modo da capire su cosa si debba intervenire
#
#avendo fatto ANOVA: differenza tra dati nazionali ed internazionali: faccio le due analisi separatamente (mi concentro sui nazionali)
#
#Vari punti di domanda: -è meglio usare dei dati funzionali?
#                       -è meglio tenere la proporzione, oppure passare al logit? 
#                         Al momento ritengo l'utilizzo del logit più sensato:
#                         PROBLEMA: noi non abbiamo una risposta binaria: non possiamo crearla siccome non sapremmo come spezzare le varie cause del ritardo nel caso vi sia effettivamente un ritardo
#                       -come tenere in considerazione l'andamento temporale: 
#                        o si inseriscono delle dummy per i vari anni, trattandoli come mixed effects
#                        oppure si potrebbe utilizzare il modello di regressione con dati funzionali(al momento, questo è il modello più complesso ma anche forse il più sensato)

#STEP 1: prendo i logit di tutte queste proporzioni, e faccio un modello di regressione prendendo tutti
#        i dati, senza tenere in considerazione dell'andamento temporale

dati = data_frame(prop_ritardi_partenza=(data$num_late_at_departure/data$total_num_trips),
                  prop_ritard_arrivo=(data$num_arriving_late/data$total_num_trips),
                  external_causes=(data$delay_cause_external_cause),
                  rail_infrastructure=(data$delay_cause_rail_infrastructure),
                  traffic_management=(data$delay_cause_traffic_management),
                  rolling_stock=(data$delay_cause_rolling_stock),
                  station_management=(data$delay_cause_station_management),
                  cause_travelers=(data$delay_cause_travelers),
                  ) #la mia variabile risposta è la probabilità di partire in ritardo
dati=dati[which(data$service==livelli_servizio[2]),]
dati=na.omit(dati)

finestra_grafica(t)
par(mfrow=c(3,2))
plot(dati$external_causes,dati$prop_ritardi_partenza, xlab="External causes", ylab="Delay at departure", main="External causes")
plot(dati$rail_infrastructure ,dati$prop_ritardi_partenza, xlab="Rail infrastrcture", ylab="Delay at departure", main="Rail infrastructure")
plot(dati$traffic_management ,dati$prop_ritardi_partenza, xlab="Traffic management", main="Traffic management causes")
plot(dati$rolling_stock,dati$prop_ritardi_partenza, xlab="Rolling stock", ylab="Delay at departure", main="Rolling stock")
plot(dati$station_management,dati$prop_ritardi_partenza, xlab="Station management", ylab="Delay at departure", main="Station management")
plot(dati$cause_travelers,dati$prop_ritardi_partenza, xlab="Travelers", ylab="Delay at departure", main="Travelers")
#in nessun caso vi è una relazione lineare: devo usare le splines


#external causes
model_ext <-
  lm(dati$prop_ritardi_partenza ~ bs(dati$external_causes, degree = 2,knots=c(10000)), data = dati)
summary(model_ext)

new_data= data.frame( seq( range(dati$external_causes)[1], range(dati$external_causes)[2], by=0.01 ) ) 
preds=predict(model_ext,se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)

finestra_grafica(t)
plot(dati$external_causes,dati$prop_ritardi_partenza,cex =.5, col =" darkgrey " )
lines(dati$external_causes,preds$fit ,lwd =2, col =" blue")
matlines(dati$external_causes, se.bands ,lwd =1, col =" blue",lty =3)
#questa strada non mi sembra assolutamente percorribile: la prima volta ho usato il logit, ora le semplici 
#proporzioni, però non mi sembrano risultati soddisfacenti: è sufficiente trovare un buon modello
#per collegare la prop della risposta con le probabilità delle varie cause di ritardo, e poi si usa il gam
#però verosimilmente bisognerà passare al'utilizzo di dati funzionali