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
                  cause_travelers=(data$delay_cause_travelers)
                  ) #la mia variabile risposta è la probabilità di partire in ritardo
dati = data_frame(prop_ritardi_partenza=logit(data$num_late_at_departure/data$total_num_trips),
                  prop_ritard_arrivo=logit(data$num_arriving_late/data$total_num_trips),
                  external_causes=logit(data$delay_cause_external_cause),
                  rail_infrastructure=logit(data$delay_cause_rail_infrastructure),
                  traffic_management=logit(data$delay_cause_traffic_management),
                  rolling_stock=logit(data$delay_cause_rolling_stock),
                  station_management=logit(data$delay_cause_station_management),
                  cause_travelers=logit(data$delay_cause_travelers))
dati=dati[which(data$service==livelli_servizio[2] & data$year=='2015'),]
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
#tutta questa parte fino a qui è da buttare via


#FUNCTIONAL DATA
graphics.off()
p=dim(data)[2]
p
years = unique(data$year)
months = unique(data$month)
num_tratte = length(unique(data$route))
cols = (length(unique(data$year)))*(length(unique(data$month)))


#risposta; proporzione di treni in ritardo alla partenza
response=data.frame(data$num_late_at_departure/data$total_num_trips)
external_cause=data.frame(data$delay_cause_external_cause)
rail_infrastructure_cause=data.frame(data$delay_cause_rail_infrastructure)
traffic_management_cause=data.frame(data$delay_cause_traffic_management)
rolling_stock_cause=data.frame(data$delay_cause_rolling_stock)
station_management_cause=data.frame(data$delay_cause_station_management)
travelers_cause=data.frame(data$delay_cause_travelers)


#Trasformo in funzionale, in funzione del tempo, ogni unità è una specifica tratta
r = (unique(data$route))
time = 1:47














##############
response_fd = matrix(nrow=num_tratte, ncol = cols)
external_cause_fd=matrix(nrow=num_tratte, ncol = cols)
rail_infrastructure_cause_fd=matrix(nrow=num_tratte, ncol = cols)
traffic_management_cause_fd=matrix(nrow=num_tratte, ncol = cols)
rolling_stock_cause_fd=matrix(nrow=num_tratte, ncol = cols)
station_management_cause_fd=matrix(nrow=num_tratte, ncol = cols)
travelers_cause_fd=matrix(nrow=num_tratte, ncol = cols)
for (k in 1:num_tratte) {
  for (j in years) {
    for ( i in months){
      if(length(response[which(data$route==r[k] & data$year==j & data$month==i),1])!=0 &
         length(external_cause[which(data$route==r[k] & data$year==j & data$month==i),1])!=0 &
         length(rail_infrastructure_cause[which(data$route==r[k] & data$year==j & data$month==i),1])!=0 &
         length(rolling_stock_cause[which(data$route==r[k] & data$year==j & data$month==i),1])!=0 &
         length(traffic_management_cause[which(data$route==r[k] & data$year==j & data$month==i),1])!=0 &
         length(station_management_cause[which(data$route==r[k] & data$year==j & data$month==i),1])!=0 &
         length(travelers_cause[which(data$route==r[k] & data$year==j & data$month==i),1])!=0)
          {
              response_fd[k,(j-2015)*12 + (i)]=response[which(data$route==r[k] & data$year==j & data$month==i),1]
              external_cause_fd[k,(j-2015)*12 + (i)]=external_cause[which(data$route==r[k] & data$year==j & data$month==i),1]
              rail_infrastructure_cause_fd[k,(j-2015)*12 + (i)]=rail_infrastructure_cause[which(data$route==r[k] & data$year==j & data$month==i),1]
              rolling_stock_cause_fd[k,(j-2015)*12 + (i)]=rolling_stock_cause[which(data$route==r[k] & data$year==j & data$month==i),1]
              traffic_management_cause_fd[k,(j-2015)*12 + (i)]=traffic_management_cause[which(data$route==r[k] & data$year==j & data$month==i),1]
              station_management_cause_fd[k,(j-2015)*12 + (i)]=station_management_cause[which(data$route==r[k] & data$year==j & data$month==i),1]
              travelers_cause_fd[k,(j-2015)*12 + (i)]=travelers_cause[which(data$route==r[k] & data$year==j & data$month==i),1]
          }
    }
  }
  response_fd[k,48]=r[k]
  external_cause_fd[k,48]=r[k]
  rail_infrastructure_cause_fd[k,48]=r[k]
  rolling_stock_cause_fd[k,48]=r[k]
  traffic_management_cause_fd[k,48]=r[k]
  station_management_cause_fd[k,48]=r[k]
  travelers_cause_fd[k,48]=r[k]
  
}

#tolgo tutti i NaN: ho necessità di tenere solo le tratte tali che per risposta e tutte e 6 le covariate non vi
#siano NaN lungo il periodo considerato

#risposta
response_fd_ = data.frame(response_fd)
response_fd_ = na.omit(response_fd_) #tolgo i Na
response_fd_ = data.frame(response_fd_)
tratte_response = response_fd_$X48

#external causes
external_causes_fd_ = data.frame(external_cause_fd)
external_causes_fd_ = na.omit(external_causes_fd_) #tolgo i Na
external_causes_fd_ = data.frame(external_causes_fd_)
tratte_external_causes = external_causes_fd_$X48

#rail infrastructure causes
rail_infrastructure_causes_fd_ = data.frame(rail_infrastructure_cause_fd)
rail_infrastructure_causes_fd_ = na.omit(rail_infrastructure_causes_fd_) #tolgo i Na
rail_infrastructure_causes_fd_ = data.frame(rail_infrastructure_causes_fd_)
tratte_rail_infrastructure_causes = rail_infrastructure_causes_fd_$X48

#rolling stock causes
rolling_stock_cause_fd_ = data.frame(rolling_stock_cause_fd)
rolling_stock_cause_fd_ = na.omit(rolling_stock_cause_fd_) #tolgo i Na
rolling_stock_cause_fd_ = data.frame(rolling_stock_cause_fd_)
tratte_rolling_stock_cause = rolling_stock_cause_fd_$X48

#traffic management causes
traffic_management_cause_fd_ = data.frame(traffic_management_cause_fd)
traffic_management_cause_fd_ = na.omit(traffic_management_cause_fd_) #tolgo i Na
traffic_management_cause_fd_ = data.frame(traffic_management_cause_fd_)
tratte_traffic_management_cause = traffic_management_cause_fd_$X48

#station management causes
station_management_cause_fd_ = data.frame(station_management_cause_fd)
station_management_cause_fd_ = na.omit(station_management_cause_fd_) #tolgo i Na
station_management_cause_fd_ = data.frame(station_management_cause_fd_)
tratte_station_management_cause = station_management_cause_fd_$X48

#travelers causes
travelers_cause_fd_ = data.frame(travelers_cause_fd)
travelers_cause_fd_ = na.omit(travelers_cause_fd_) #tolgo i Na
travelers_cause_fd_ = data.frame(travelers_cause_fd_)
tratte_travelers_cause = travelers_cause_fd_$X48

#adesso devo tenere solo le tratte che hanno tutti i dati:
#il vettore che ha più elementi è response: questa parte del codice non è minimamente riproducibile, dal momento 
#che parte dal presupposto che tratte_response sia la più lunga di tutte
tratte_comuni=NULL
for (i in 1:length(tratte_response)) {
  
  indicatrice=0
  
  for (j in 1:length(tratte_external_causes)) {
    
    if (tratte_response[i]==tratte_external_causes[j] & indicatrice==0){
      for (k in 1:length(tratte_rail_infrastructure_causes)) {
        
        if (tratte_response[i]==tratte_rail_infrastructure_causes[k] & indicatrice==0){
          for (l in 1:length(tratte_rolling_stock_cause)) {
            
            if (tratte_response[i]==tratte_rolling_stock_cause[l] & indicatrice==0){
              for (m in 1:length(tratte_traffic_management_cause)) {
                
                if(tratte_response[i]==tratte_traffic_management_cause[m] & indicatrice==0){
                  for (n in 1:length(tratte_station_management_cause)) {
                    
                    if(tratte_response[i]==tratte_station_management_cause[n] & indicatrice==0){
                      for (o in 1:length(tratte_travelers_cause)) {
                        
                        if(tratte_response[i]==tratte_travelers_cause[o] & indicatrice==0){
                          indicatrice=1
                        }
                        
                      }
                    }
                  }
                }
              }
            }
              
          }
        }
          
      }
    }
      
  }
  
  if(indicatrice==1){
    tratte_comuni=c(tratte_comuni,tratte_response[i])
  }
  
}


indici_response=NULL
indici_external_causes=NULL
indici_rail_infrastructure_causes=NULL
indici_rolling_stock_causes=NULL
indici_traffic_management_causes=NULL
indici_station_management_causes=NULL
indici_traveler_causes=NULL

for (i in 1:length(tratte_comuni)) {
  temp_r=which(tratte_response==tratte_comuni[i])
  indici_response=c(indici_response,temp_r)
  
  temp_a=which(tratte_external_causes==tratte_comuni[i])
  indici_external_causes=c(indici_external_causes,temp_a)
  
  temp_b=which(tratte_rail_infrastructure_causes==tratte_comuni[i])
  indici_rail_infrastructure_causes=c(indici_rail_infrastructure_causes,temp_b)
  
  temp_c=which(tratte_rolling_stock_cause==tratte_comuni[i])
  indici_rolling_stock_causes=c(indici_rolling_stock_causes,temp_c)
  
  temp_d=which(tratte_traffic_management_cause==tratte_comuni[i])
  indici_traffic_management_causes=c(indici_traffic_management_causes,temp_d)
  
  temp_e=which(tratte_station_management_cause==tratte_comuni[i])
  indici_station_management_causes=c(indici_station_management_causes,temp_e)
  
  temp_f=which(tratte_travelers_cause==tratte_comuni[i])
  indici_traveler_causes=c(indici_traveler_causes,temp_f)
  
}

#ora trasformo i dati in funzionali: per ognuno separatamente
response_fd_cut = data.frame(response_fd_[indici_response,1:47], row.names = tratte_comuni)
external_causes_fd_cut = data.frame(external_causes_fd_[indici_external_causes,1:47], row.names = tratte_comuni)
rail_infrastructure_causes_fd_cut = data.frame(rail_infrastructure_causes_fd_[indici_rail_infrastructure_causes,1:47], row.names = tratte_comuni)
rolling_stock_cause_fd_cut = data.frame(rolling_stock_cause_fd_[indici_rolling_stock_causes,1:47], row.names = tratte_comuni)
traffic_management_cause_fd_cut = data.frame(traffic_management_cause_fd_[indici_traffic_management_causes,1:47], row.names = tratte_comuni)
station_management_cause_fd_cut = data.frame(station_management_cause_fd_[indici_station_management_causes,1:47], row.names = tratte_comuni)
travelers_cause_fd_cut = data.frame(travelers_cause_fd_[indici_traveler_causes,1:47], row.names = tratte_comuni)



### questo è necessario per trasformare tutti i dati
nbasis=10
m=3
basis <- create.bspline.basis(rangeval=c(min(time),max(time)), nbasis=nbasis, norder=m)
basismat <- eval.basis(time, basis)

y=smooth.basis(argvals=time, as.numeric(response_fd_cut[1,]), fdParobj=basis)$fd 
x1=smooth.basis(argvals=time, as.numeric(external_causes_fd_cut[1,]), fdParobj=basis)$fd 

library(fRegression)
modello = fRegress( y,x1)

#così lo faccio
est_coef_resp = lsfit(basismat, t(response_fd_cut[1,]), intercept=FALSE)$coef
est_coef_cov1 = lsfit(basismat, t(external_causes_fd_cut[1,]), intercept=FALSE)$coef
est_coef_cov2 = lsfit(basismat, t(rail_infrastructure_causes_fd_cut[1,]), intercept=FALSE)$coef
est_coef_cov3 = lsfit(basismat, t(rolling_stock_cause_fd_cut[1,]), intercept=FALSE)$coef
est_coef_cov4 = lsfit(basismat, t(traffic_management_cause_fd_cut[1,]), intercept=FALSE)$coef
est_coef_cov5 = lsfit(basismat, t(station_management_cause_fd_cut[1,]), intercept=FALSE)$coef
est_coef_cov6 = lsfit(basismat, t(travelers_cause_fd_cut[1,]), intercept=FALSE)$coef



y <- basismat %*% est_coef_resp
y=t(y)
x1 <- basismat %*% est_coef_cov1
x1=t(x1)
x2 <- basismat %*% est_coef_cov2
x2=t(x2)
x3 <- basismat %*% est_coef_cov3
x3=t(x3)
x4 <- basismat %*% est_coef_cov4
x4=t(x4)
x5 <- basismat %*% est_coef_cov5
x5=t(x5)
x6 <- basismat %*% est_coef_cov6
x6=t(x6)





















#######
f_data_response = fData(time,response_fd_cut)
f_data_external_causes = fData(time,external_causes_fd_cut)
f_data_rail_infrastructure_causes = fData(time,rail_infrastructure_causes_fd_cut)
f_data_rolling_stock_causes = fData(time,rolling_stock_cause_fd_cut)
f_data_traffic_management_cause = fData(time,traffic_management_cause_fd_cut)
f_data_station_management_cause = fData(time,station_management_cause_fd_cut)
f_data_travelers_cause = fData(time,travelers_cause_fd_cut)
#####
#mediane
band_depth_response = BD(Data = f_data_response)
modified_band_depth_response = MBD(Data = f_data_response)
median_curve_response = median_fData(fData = f_data_response, type = "MBD") 

band_depth_external_causes = BD(Data = f_data_external_causes)
modified_band_depth_external_causes = MBD(Data = f_data_external_causes)
median_curve_external_causes = median_fData(fData = f_data_external_causes, type = "MBD") 

band_depth_rail_infrastructure_cause = BD(Data = f_data_rail_infrastructure_causes)
modified_band_depth_rail_infrastructure_cause = MBD(Data = f_data_rail_infrastructure_causes)
median_curve_rail_infrastructure_cause = median_fData(fData = f_data_rail_infrastructure_causes, type = "MBD") 

band_depth_rolling_stock_cause = BD(Data = f_data_rolling_stock_causes)
modified_band_depth_rolling_stock_cause = MBD(Data = f_data_rolling_stock_causes)
median_curve_rolling_stock_cause = median_fData(fData = f_data_rolling_stock_causes, type = "MBD")

band_depth_traffic_management_cause = BD(Data = f_data_traffic_management_cause)
modified_band_depth_traffic_management_cause = MBD(Data = f_data_traffic_management_cause)
median_curve_traffic_management_cause = median_fData(fData = f_data_traffic_management_cause, type = "MBD")

band_depth_station_management_cause = BD(Data = f_data_station_management_cause)
modified_band_depth_station_management_cause = MBD(Data = f_data_station_management_cause)
median_curve_station_management_cause = median_fData(fData = f_data_station_management_cause, type = "MBD") 

band_depth_travelers_cause = BD(Data = f_data_travelers_cause)
modified_band_depth_travelers_cause = MBD(Data = f_data_travelers_cause)
median_curve_travelers_cause = median_fData(fData = f_data_travelers_cause, type = "MBD") 


#plot of the response
finestra_grafica(t)
plot(time,y[,1], main="Average delay at departure") 
lines(time,median_curve_response$values, lwd=2)
 
#plot of the covariates
finestra_grafica(t)
par(mfrow=c(3,2))
plot(x1, main="External causes") 
lines(time,median_curve_external_causes$values, lwd=2)
plot(f_data_rail_infrastructure_causes, main="Rail infrastructure causes") 
lines(time,median_curve_rail_infrastructure_cause$values, lwd=2)
plot(f_data_rolling_stock_causes, main="Rolling stock causes") 
lines(time,median_curve_rolling_stock_cause$values, lwd=2)
plot(f_data_traffic_management_cause, main="Traffic management causes") 
lines(time,median_curve_traffic_management_cause$values, lwd=2)
plot(f_data_station_management_cause, main="Station management causes") 
lines(time,median_curve_station_management_cause$values, lwd=2)
plot(f_data_travelers_cause, main="Travelers causes") 
lines(time,median_curve_travelers_cause$values, lwd=2)

