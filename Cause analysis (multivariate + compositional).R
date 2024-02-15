library(readxl)
#install.packages('compositions') #libreria che serve per trattare compositional data
library(compositions)
library(robustbase)
graphics.off()


finestra_grafica = function(t){
  if (t==0)
    return(quartz())
  else 
    return(x11())
}
t=1 #per chiamare in automatico il quartz, mettere un qualsiasi altro valore per avere x11()


#no scioperi
data_no_strike = read_excel('Data_by_year_nostrikes.xlsx')
i_2018=which(data_no_strike$year==2018)
cause_2018_no_strike=data.frame(data_no_strike$delay_cause_rail_infrastructure[i_2018],
                                data_no_strike$delay_cause_traffic_management[i_2018],
                                data_no_strike$delay_cause_rolling_stock[i_2018],
                                data_no_strike$delay_cause_station_management[i_2018],
                                data_no_strike$delay_cause_external_cause[i_2018]+data_no_strike$delay_cause_travelers[i_2018])
nomi_cause=c('Rail infrastructure','Traffic management',
             'Rolling stock','Station management','Externals')

colnames(cause_2018_no_strike)=nomi_cause
n_2018_no_strike = dim(cause_2018_no_strike)[1]

cause_2018_comp_no_strike=acomp(cause_2018_no_strike) 

finestra_grafica(t)
plot(cause_2018_comp_no_strike)     


#Trasformazione: isometric log ratio transformation: data are mapped from simplex to R^4
cause_2018__no_strike_transformed=(-1)*ilr(cause_2018_no_strike) 

x1_no_strike = cause_2018__no_strike_transformed[,1] 
x2_no_strike = cause_2018__no_strike_transformed[,2] 
x3_no_strike = cause_2018__no_strike_transformed[,3] 
x4_no_strike = cause_2018__no_strike_transformed[,4]

#RESPONSE
response_no_strike = data_no_strike$avg_delay_late_on_arrival[i_2018]

#firstly, I try with a simple linear model
fit_no_strike = lm( response_no_strike ~ x1_no_strike + x2_no_strike + x3_no_strike + x4_no_strike)
summary(fit_no_strike)
# R^2 of 0.1971
shapiro.test(fit_no_strike$residuals)
#gaussian residuals

vif(fit_no_strike)
#no problem of collinearity

#I reduce: exact test

fit_red_no_strike = lm( response_no_strike ~ x1_no_strike + x3_no_strike + x4_no_strike)
summary(fit_red_no_strike)
shapiro.test(fit_red_no_strike$residuals)
#best linear model that I can have: R^2=0.1915: not a lot

#introducing gam
model_gam_no_strike=gam(response_no_strike ~ s(x1_no_strike,bs='cr') + 
                                   s(x2_no_strike,bs='cr') + 
                                   s(x3_no_strike,bs='cr') + 
                                   s(x4_no_strike,bs='cr') )
summary(model_gam_no_strike)
#it seems to explaining it a bit better: R2=0.219
shapiro.test(model_gam_no_strike$residuals)
#I have gaussianity

#firstly, I try to do it semiparametric since I have some terms that appears to be linear

model_semiparam_no_strike = gam(response_no_strike ~ x1_no_strike + x2_no_strike + s(x3_no_strike,bs='cr') + s(x4_no_strike,bs='cr') )
summary(model_semiparam_no_strike)
shapiro.test(model_semiparam_no_strike$residuals)
#R2=0.219

#I can also reduce my model
model_gam_red_no_strike = gam(response_no_strike ~ s(x3_no_strike,bs='cr') + s(x4_no_strike,bs='cr'))
summary(model_gam_red_no_strike)

anova(model_gam_red_no_strike,model_gam_no_strike, test = "F") 
#pvalue of 0.1751: I can take the simplest one: can I put the last term linear?
model_gam_red_2_no_strike = gam(response_no_strike ~ x3_no_strike + s(x4_no_strike,bs='cr'))
summary(model_gam_red_2_no_strike)

anova(model_gam_red_no_strike,model_gam_red_2_no_strike, test = "F") 
#I can accept H0, since the pvalue is 0.2194

#Model for no_strike: R2=0.199
model_no_strike = model_gam_red_2_no_strike
summary(model_no_strike)

# Best model found:
# y = x3 + f4(x4) + eps
shapiro.test(model_no_strike$residuals)     #pvalue: 0.05745: I can assume them gaussian
anova(gam(response_no_strike~1),model_no_strike,test="F") #pvalue: 6.353e-05: I reject H0: the model is significative: it is an exact test
#
#Interpretazione: 
# x1: rail_inf/traffic_man: non c'è
# x2: rail_inf*traffic_man/rolling_stock: non c'è
# x3: rail_inf*traffic_man*rolling_stock/station_man: è lineare con coefficiente positivo
# x4: interazione_interne/esterne: funzione non lineare

# effect of x3
min(x3_no_strike)
max(x3_no_strike)
sequence_x3_no_strike = seq(min(x3_no_strike),max(x3_no_strike),by=0.001)
finestra_grafica(t)
plot(sequence_x3_no_strike,model_no_strike$coefficients[1] + sequence_x3_no_strike*model_no_strike$coefficients[2],
     xlab='x3',ylab='b1 + b2*x3',main='Average delay late on arrival vs x3')
#all'aumentare dell'interazione fra rail_inf*traffic_man*rolling_stock rispetto che station_manag, aumenta la media
#del ritardo medio: la magnitudo è notevole, a livello di quanto aumenti in effetti il ritardo

#effect of x4
finestra_grafica(t)
plot(model_no_strike,xlab='x4',ylab='f4(x4)',main='Average delay late on arrival vs x4')
# cresce se x4 <-1 o se x4 > -0.5: questo significa che se c'è un grande sbilanciamento di fattori: 
# il ritardo medio aumenta , e anche velocemente, se cè il rapporto è minore di 0.3
# il ritardo aumenta anche se il rapporto radice_4(cause_int_int)/cause_ext è circa maggiore di 0.6(1/radice(e))
# cosa significa: lo sbilanciamento tra l'avere un maggior effetto (su cui non ci si può fare nulla) di cause esterne
# o l'avere una grande interazione di cause interne(su cui si può lavorare) aumenta il ritardo

# Unendo i due casi, sicuramente il termine relativo a x3 ha una magnitudo maggiore sul ritardo medio:
# potremmo interpretare il termine relativo a x4 come un termine che indica il giusto bilanciamento di cause:
# la radice_4(interazione_interne)/esterne deve essere circa 0.4: quindi le esterne devono avere un maggior impatto:
# essenzialmente, sta dicendo una cosa ovvia, ovvero che le cause interne debbano essere tenute separate tra di loro,
# in maniera tale da potervi intervenire indipendentemente (a numeratore abbiamo un'interazione)
# Il termine relativo a x3, di contro, mostra come l'introduzione di rolling stock al numeratore sia il maggior
# 'fattore di rischio': è ciò su cui bisogna lavorare




################################
##### ROBUST BIVARIATE #########
################################
Rail_infrastructure = data.frame(data_no_strike$avg_delay_late_on_arrival[i_2018],
                                 data_no_strike$delay_cause_rail_infrastructure[i_2018])
colnames(Rail_infrastructure)=c('Avg delay late on arrival','Rail infrastructure')
Traffic_management = data.frame(data_no_strike$avg_delay_late_on_arrival[i_2018],
                                 data_no_strike$delay_cause_traffic_management[i_2018])
colnames(Traffic_management)=c('Avg delay late on arrival','Traffic management')
Rolling_stock = data.frame(data_no_strike$avg_delay_late_on_arrival[i_2018],
                                 data_no_strike$delay_cause_rolling_stock[i_2018])
colnames(Rolling_stock)=c('Avg delay late on arrival','Rolling stock')
Station_management = data.frame(data_no_strike$avg_delay_late_on_arrival[i_2018],
                                 data_no_strike$delay_cause_station_management[i_2018])
colnames(Station_management)=c('Avg delay late on arrival','Station management')
Externals = data.frame(data_no_strike$avg_delay_late_on_arrival[i_2018],
                                 data_no_strike$delay_cause_external_cause[i_2018]+data_no_strike$delay_cause_travelers[i_2018])
colnames(Externals)=c('Avg delay late on arrival','Externals')
p=2


#rail infrastructure
#firstly, I visualize the data
finestra_grafica(t)
plot(Rail_infrastructure$`Rail infrastructure`,Rail_infrastructure$`Avg delay late on arrival`)
#robust bivariate
MCD_rail_inf <- covMcd(x = Rail_infrastructure, alpha = .75, nsamp = 2000) 

#trovo gli outliers
ind_out_rail_inf <- (1:n_2018_no_strike)[MCD_rail_inf$mcd.wt==0]  #51

finestra_grafica(t)
plot(Rail_infrastructure$`Rail infrastructure`,Rail_infrastructure$`Avg delay late on arrival`,
     col=ifelse(1:n%in%ind_out_rail_inf,'red',"black"),pch=20,
      cex.lab=1.2, cex.axis=1.2, xlab='Proportion of trains delayed due to rail infrastructure', 
     ylab='Average delay late on arrival')
points(MCD_rail_inf$center[2], MCD_rail_inf$center[1], pch=4, col='coral', lwd=3, cex=1.5)

finestra_grafica(t)
plot(Rail_infrastructure$`Rail infrastructure`,Rail_infrastructure$`Avg delay late on arrival`,main='Avg delay late on arrival vs due to rail infrastructure',xlab='Due to rail infrastructure',ylab='Avg delay late on arrival')
points(Rail_infrastructure$`Rail infrastructure`[ind_out_rail_inf],Rail_infrastructure$`Avg delay late on arrival`[ind_out_rail_inf],col='red')
legend("topleft", legend=levels(factor(c('No outliers','Outliers'))), fill=c('blue','red'), cex=.7)
ind_out_rail_inf
Rail_infrastructure[ind_out_rail_inf,]
#tratta 51 non la considererei tanto un outlier 'dannoso': anzi, è un po' particolare perchè nonostante risenta così
#tanto di questa causa, il ritardo medio in quella tratta è basso


#traffic management
#firstly, I visualize the data
finestra_grafica(t)
plot(Traffic_management$`Traffic management`,Traffic_management$`Avg delay late on arrival`)
#robust bivariate
MCD_traf_man <- covMcd(x = Traffic_management, alpha = .75, nsamp = 2000) 

ind_out_traf_man <- (1:n_2018_no_strike)[MCD_traf_man$mcd.wt==0] #33 39

finestra_grafica(t)
plot(Traffic_management$`Traffic management`,Traffic_management$`Avg delay late on arrival`,
     col=ifelse(1:n%in%ind_out_traf_man,'red',"black"),pch=20,
     cex.lab=1.2, cex.axis=1.2, xlab='Proportion of trains delayed due to traffic management', 
     ylab='Average delay late on arrival')
points(MCD_traf_man$center[2], MCD_traf_man$center[1], pch=4, col='coral', lwd=3, cex=1.5)

finestra_grafica(t)
plot(Traffic_management$`Traffic management`,Traffic_management$`Avg delay late on arrival`,main='Avg delay late on arrival vs due to traffic management',xlab='Due to traffic management',ylab='Avg delay late on arrival')
points(Traffic_management$`Traffic management`[ind_out_traf_man],Traffic_management$`Avg delay late on arrival`[ind_out_traf_man],col='red')
legend("topleft", legend=levels(factor(c('No outliers','Outliers'))), fill=c('blue','red'), cex=.7)
ind_out_traf_man
Traffic_management[ind_out_traf_man,]
#in questo caso, si può imputare alla tratta 33 il fatto di avere un ritardo elevato per questo motivo: onestamente non saprei
#direi più che vi sia sia un ritardo alto che una proporzione alta

#rolling stock
#firstly, I visualize the data
finestra_grafica(t)
plot(Rolling_stock$`Rolling stock`,Rolling_stock$`Avg delay late on arrival`)
#robust bivariate
MCD_rol_sto <- covMcd(x = Rolling_stock, alpha = .75, nsamp = 2000) 

ind_out_rol_sto <- (1:n_2018_no_strike)[MCD_rol_sto$mcd.wt==0] #3 14 19 26 28 44 54 74 75 76 88 89

finestra_grafica(t)
plot(Rolling_stock$`Rolling stock`,Rolling_stock$`Avg delay late on arrival`,
     col=ifelse(1:n%in%ind_out_rol_sto,'red',"black"),pch=20,
     cex.lab=1.2, cex.axis=1.2, xlab='Proportion of trains delayed due to rolling stock', 
     ylab='Average delay late on arrival')
points(MCD_rol_sto$center[2], MCD_rol_sto$center[1], pch=4, col='coral', lwd=3, cex=1.5)

finestra_grafica(t)
plot(Rolling_stock$`Rolling stock`,Rolling_stock$`Avg delay late on arrival`,main='Avg delay late on arrival vs due to rolling stock',xlab='Due to rolling stock',ylab='Avg delay late on arrival')
points(Rolling_stock$`Rolling stock`[ind_out_rol_sto],Rolling_stock$`Avg delay late on arrival`[ind_out_rol_sto],col='red')
legend("topleft", legend=levels(factor(c('No outliers','Outliers'))), fill=c('blue','red'), cex=.7)
ind_out_rol_sto
Rolling_stock[ind_out_rol_sto,]
#in questo caso non sembra esserci una relazione tra alta proporzione e alto ritardo: i punti effettivamente fuori
#dalla nuvola di punti sono quelli che hanno una proporzione un po' maggiore di 0.3

#Station_management
#firstly, I visualize the data
finestra_grafica(t)
plot(Station_management$`Station management`,Station_management$`Avg delay late on arrival`)
#robust bivariate
MCD_stat_man <- covMcd(x = Station_management, alpha = .75, nsamp = 2000) 

ind_out_stat_man <- (1:n_2018_no_strike)[MCD_stat_man$mcd.wt==0] #23 54 92

finestra_grafica(t)
plot(Station_management$`Station management`,Station_management$`Avg delay late on arrival`,
     col=ifelse(1:n%in%ind_out_stat_man,'red',"black"),pch=20,
     cex.lab=1.2, cex.axis=1.2, xlab='Proportion of trains delayed due to station management', 
     ylab='Average delay late on arrival')
points(MCD_stat_man$center[2], MCD_stat_man$center[1], pch=4, col='coral', lwd=3, cex=1.5)

finestra_grafica(t)
plot(Station_management$`Station management`,Station_management$`Avg delay late on arrival`,main='Avg delay late on arrival vs due to station management',xlab='Due to station management',ylab='Avg delay late on arrival')
points(Station_management$`Station management`[ind_out_stat_man],Station_management$`Avg delay late on arrival`[ind_out_stat_man],col='red')
legend("topleft", legend=levels(factor(c('No outliers','Outliers'))), fill=c('blue','red'), cex=.7)
ind_out_stat_man
Station_management[ind_out_stat_man,]
#in questo caso non sembra esserci una relazione tra alta proporzione e alto ritardo: i punti effettivamente fuori
#dalla nuvola di punti sono quelli che hanno una proporzione un po' maggiore di 0.3

#EXTERNALS
#firstly, I visualize the data
finestra_grafica(t)
plot(Externals$Externals,Externals$`Avg delay late on arrival`)
#robust bivariate
MCD_ex <- covMcd(x= Externals, alpha = .75, nsamp = 2000) 

ind_out_ex <- (1:n_2018_no_strike)[MCD_ex$mcd.wt==0] # 0.75: nulla

finestra_grafica(t)
plot(Externals$Externals,Externals$`Avg delay late on arrival`,
     col=ifelse(1:n%in%ind_out_ex,'red',"black"),pch=20,
     cex.lab=1.2, cex.axis=1.2, xlab='Proportion of trains delayed due to external causes', 
     ylab='Average delay late on arrival')
points(MCD_ex$center[2], MCD_ex$center[1], pch=4, col='coral', lwd=3, cex=1.5)

finestra_grafica(t)
plot(Externals$Externals,Externals$`Avg delay late on arrival`,main='Avg delay late on arrival vs due to external causes',xlab='Due to external causes',ylab='Avg delay late on arrival')
points(Externals$Externals[ind_out_ex],Externals$`Avg delay late on arrival`[ind_out_ex],col='red')
legend("topleft", legend=levels(factor(c('No outliers','Outliers'))), fill=c('blue','red'), cex=.7)
ind_out_ex
Externals[ind_out_ex,]
#in questo caso non sembra esserci una relazione tra alta proporzione e alto ritardo: i punti effettivamente fuori
#dalla nuvola di punti sono quelli che hanno una proporzione un po' maggiore di 0.3

#Che tratte?
data_no_strike_2018 = data_no_strike[i_2018,]
#Outliers rail infrastructures
ind_out_rail_inf
data_no_strike_2018[ind_out_rail_inf,]$route
#Outliers traffic management
ind_out_traf_man
data_no_strike_2018[ind_out_traf_man,]$route
#Outliers rolling stock
length(ind_out_rol_sto)
ind_out_rol_sto
data_no_strike_2018[ind_out_rol_sto,]$route #interessante: molte tratte che partono da Parigi ne risentono
#Outliers station management
ind_out_stat_man
data_no_strike_2018[ind_out_stat_man,]$route
#Outliers externals
ind_out_ex
data_no_strike_2018[ind_out_ex,]$route

#each route that is an outlier has a station in Paris
# for the internal causes, the outliers are the ones that have an extreme value for the cause, not for a relation with 
# the response
