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
t=0 #per chiamare in automatico il quartz, mettere un qualsiasi altro valore per avere x11()


#Struttura
# Tutti i dati
# Cause esterne/interne
# ILR
# GAM, per vedere quale sia la più importante, se ve ne è una
#
# Scioperi
# Cause esterne/interne
# ILR
# GAM, per vedere quale sia la più importante, se ve ne è una: vedere più combinazioni di colonne
# Robusta bivariata per le coppie risposta-causa_i


#valuto l'effetto delle cause anno per anno: lo faccio con il 2018 perchè sono le più recenti
data = read_excel('aggregated_trains_by_year_2701.xlsx')
n = dim(data)[1]
i_2018=which(data$year==2018)


#prendo le cause del 2018: vengono divise in quali sono interne e quali esterne, è questa distinzione che ci interessa
cause_2018=data.frame(data$delay_cause_rail_infrastructure[i_2018],
                      data$delay_cause_traffic_management[i_2018],
                      data$delay_cause_rolling_stock[i_2018],
                      data$delay_cause_station_management[i_2018],
                      data$delay_cause_external_cause[i_2018]+data$delay_cause_travelers[i_2018])
nomi_cause=c('Rail infrastructure','Traffic management',
             'Rolling stock','Station management','Externals')
colnames(cause_2018)=nomi_cause
n_2018 = dim(cause_2018)[1]

cause_2018_comp=acomp(cause_2018) #è semplicemente un oggetto di classe compositional data, i dati non sono trasformati

#The general approach in analysing acomp objects is thus to perform classical multivariate analysis on clr/alr/ilr-transformed coordinates and to backtransform or display the results in such a way that they can be interpreted in terms of the original compositional parts.
finestra_grafica(t)
plot(cause_2018_comp)         #plot dei simplessi

finestra_grafica(t)
barplot(colMeans(cause_2018_comp))

#Trasformazione: isometric log ratio transformation: data are mapped from S^5 to R^4
cause_2018_transformed=(-1)*ilr(cause_2018) 

x1 = cause_2018_transformed[,1] #(1/sqrt(2))*log(rail_infrastructure/traffic_management)
x2 = cause_2018_transformed[,2] #(sqrt(2/3))*log(sqrt(rail_infrastructure*traffic_management/rolling_stock))
x3 = cause_2018_transformed[,3] #(sqrt(3/4))*log(radice3(rail_infrastructure*traffic_management*rolling_stock)/station_management)
x4 = cause_2018_transformed[,4] #(sqrt(4/5))*log(radice4(rail_infrastructure*traffic_management*rolling_stock*station_management)/externals)

#interpretazione:
# x1: quanto l'effetto di rail_inf sia maggiore di traffic_management
# x2: la radice quadrata dell'interazione tra rail_infrastructure e traffic_management sia maggiore di rolling stock
#     se non ci fosse la radice, sarebbe il rapporto dell'interazione tra rail_infrastructure e traffic_management su rolling stock:
#     il logaritmo è monotona, quindi tanto più l'interazione è grande, tanto più il log è grande.
#     ma anche la radice è monotona: qui di x2 esprime quanto l'interazione fra rail_infrastructure e traffic_management sia
#     maggiore di rolling stock. Lo stesso identico ragionamento si applica per x3 e x4: l'unico problema è che, inserendo 
#     una radice di ordine sempre maggiore, cambia l'ordine di grandezza
# x3: quanto l'interazione tra rail_infrastructure e traffic_management e rolling stock sia maggiore di station_management
# x4: quanto l'interazione tra le cause interne sia maggiore di quelle esterne
#
# Essenzialamente, più la covariata è grande, più l'interazione a denominatore è maggiore dell'effetto del denominatore


###########
#SPCA: non serve per l'analisi, l'ho messa per completezza, decidiamo se metterla nel codice finale
#come funziona: è la PCA normale su una trasformazione specifica:
#io devo passare un oggetto acomp
# di default usa la clr, ma se passo una trasformazione ilr o alr la fa su quelle
#pc <- princomp((acomp(cause_2018))) #usa la clr: ho 5 scores perchè sono in R5
#pc = princomp(ilr(acomp(cause_2018)))  #sto mettendo la ilr: ho 4 scores perchè fatto in R4
#pc
#summary(pc)
#finestra_grafica(t)
#plot(pc)
#pc$Loadings
#pc$scores
#biplot(pc)  


#RESPONSE
response = data$avg_delay_late_on_arrival[i_2018]

#firstly, I try with a simple linear model
fit = lm( response ~ x1 + x2 + x3 + x4)
summary(fit)
# R^2 of 0.2618
shapiro.test(fit$residuals)
#gaussian residuals

vif(fit)
#no problem of collinearity

#I reduce: exact test

fit_red = lm( response ~ x2 + x3 + x4)
summary(fit_red)
shapiro.test(fit_red$residuals)
#best linear model that I can have: R^2=0.2618: not a lot


#ora provo a fare un gam, per 
model_gam=gam(response ~ s(x1,bs='cr') + s(x2,bs='cr') + s(x3,bs='cr') + s(x4,bs='cr') )
summary(model_gam)
#it seems to explaining it better
shapiro.test(model_gam$residuals)
#I have gaussianity


# I try the semiparametric one to see if it is better
model_semiparam = gam(response ~ x1 + s(x2,bs='cr') + s(x3,bs='cr') + x4 )
summary(model_semiparam)
#R^2 is not better anymore: I try the ANOVA test
shapiro.test(model_semiparam$residuals)
#I can do it parametric: it is more powerful

anova(model_semiparam,model_gam, test = "F") 
#the pavalue is 0.2229: I do not have to reject H0: Ican retain the semiparanetric model
#I reduce it since the pvalue for the coefficients of x1 = 0 in H0 is high
model_gam_red=gam(response ~ s(x2,bs='cr') + s(x3,bs='cr') + x4 )
summary(model_gam_red)
#ANOVA test:
anova(model_gam_red,model_semiparam, test = "F") 
#pvalue: 0.623: I can use the reduced

model = model_gam_red # y = f2(x2) + f3(x3) + x4 + eps
summary(model)
shapiro.test(model$residuals)
#R^2: 0.329

#I try to plot it since I'd like to have an interpretation
finestra_grafica(t)
#par(mfrow=c(1,3))
plot(model)
#dal plot:
#f2(x2): cresce se x2<0.5, arrivando circa fino a 5-6
#        decresce se x2>0.5, sta tanto sopra a -5
#        circa costante tra 0 e 0.5
#
#f3(x3): circa crescente
#        torna giù dopo 2 (non rilevante) e tra 1 e 1.5
#
#f4(x4): retta con pendenza negativa di circa 10, intercetta di 26
#
#
#Interpretazione:
#f2(x2): si ha un aumento sul ritardo medio fino a che x2 x2 non raggiunge -0.5: il ritardo aumenta quando la radice
#        dell'interazione fra rail_infrastructure e traffic_management è più piccola di rolling stock
#        diminuisce se questa cosa diventa tanto grande
#f3(x3): dimostra come station management sia la causa più importante di tutte per un incremento del ritardo medio: il picco è circa 10, ma assume
#        valori positivi soprattutto dove il denominatore è maggiore del numeratore
#f4(x4): più x4 aumenta, più diminuisce: aumenta tanto più l'interazione tra interne diventa piccola rispetto a quelle esterne
#interpretazione molto a grandi linee, non l'ho approfondita volutamente siccome abbiamo deciso di usare il senza scioperi



#no scioperi
data_no_strike = read_excel('aggregated_trains_by_year_nostrike_0402.xlsx')

cause_2018_no_strike=data.frame(data_no_strike$delay_cause_rail_infrastructure[i_2018],
                                data_no_strike$delay_cause_traffic_management[i_2018],
                                data_no_strike$delay_cause_rolling_stock[i_2018],
                                data_no_strike$delay_cause_station_management[i_2018],
                                data_no_strike$delay_cause_external_cause[i_2018]+data_no_strike$delay_cause_travelers[i_2018])

colnames(cause_2018_no_strike)=nomi_cause
n_2018_no_strike = dim(cause_2018_no_strike)[1]

cause_2018_comp_no_strike=acomp(cause_2018_no_strike) 

finestra_grafica(t)
plot(cause_2018_comp_no_strike)     


#Trasformazione: isometric log ratio transformation: data are mapped from S^5 to R^4
cause_2018__no_strike_transformed=(-1)*ilr(cause_2018_no_strike) 

x1_no_strike = cause_2018__no_strike_transformed[,1] 
x2_no_strike = cause_2018__no_strike_transformed[,2] 
x3_no_strike = cause_2018__no_strike_transformed[,3] 
x4_no_strike = cause_2018__no_strike_transformed[,4]
#stesse identiche interpretazioni di prima

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

#ora provo a fare un gam, per 
model_gam_no_strike=gam(response_no_strike ~ s(x1_no_strike,bs='cr') + 
                                   s(x2_no_strike,bs='cr') + 
                                   s(x3_no_strike,bs='cr') + 
                                   s(x4_no_strike,bs='cr') )
summary(model_gam_no_strike)
#it seems to explaining it better: R2=0.219
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
#NB: ho provato a fare nuovamente regressione 1vs1: è una cosa insensata, i modelli che appaiono sono tutti in funzione solo
#dell'interecetta: avevo iniziato scrivendo il primo, ma poi vedendo i dati verrebbero tutti così. Tra l'altro R2 sotto allo 0.1,
#non lo ritengo sensato
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
MCD_rail_inf <- covMcd(x = Rail_infrastructure, alpha = .75, nsamp = "best") 
finestra_grafica(t)
plot(MCD_rail_inf,classic=TRUE)
#trovo gli outliers
ind_out_rail_inf <- (1:n_2018_no_strike)[MCD_rail_inf$mcd.wt==0]  #51

##metodo alternativo da non usare
ind_out_rail_inf <- (1:n_2018_no_strike)[MCD_rail_inf$mcd.wt==0]  #51
ind_obs_rail_inf <-
  which(
    mahalanobis(
      x = Rail_infrastructure,
      center = MCD_rail_inf$raw.center,
      cov = MCD_rail_inf$raw.cov
    ) <= qchisq(p = .975, df = p) #p=number of covariates
  )
ind_out_rail_inf = setdiff(1:n_2018_no_strike,ind_obs_rail_inf)  #30  51 103


finestra_grafica(t)
plot(Rail_infrastructure$`Rail infrastructure`,Rail_infrastructure$`Avg delay late on arrival`,main='Avg delay late on arrival vs due to rail infrastructure',xlab='Due to rail infrastructure',ylab='Avg delay late on arrival')
points(Rail_infrastructure$`Rail infrastructure`[ind_out_rail_inf],Rail_infrastructure$`Avg delay late on arrival`[ind_out_rail_inf],col='red')
legend("topleft", legend=levels(factor(c('No outliers','Outliers'))), fill=c('blue','red'), cex=.7)
ind_out_rail_inf
Rail_infrastructure[ind_out_rail_inf,]
#tratta 51 non la considererei tanto un outlier 'dannoso': anz, è un po' particolare perchè nonostante risenta così
#tanto di questa causa, il ritardo medio in quella tratta è basso
summary(ltsReg(Rail_infrastructure$`Avg delay late on arrival`~Rail_infrastructure$`Rail infrastructure`, alpha=.75,mcd=TRUE)) #R2=0.05497



#traffic management
#firstly, I visualize the data
finestra_grafica(t)
plot(Traffic_management$`Traffic management`,Traffic_management$`Avg delay late on arrival`)
#robust bivariate
MCD_traf_man <- covMcd(x = Traffic_management, alpha = .5, nsamp = "best") 
finestra_grafica(t)
plot(MCD_traf_man,classic=TRUE)
ind_out_traf_man <- (1:n_2018_no_strike)[MCD_traf_man$mcd.wt==0] #33 39

##
ind_obs_traf_man <-
  which(
    mahalanobis(
      x = Traffic_management,
      center = MCD_traf_man$raw.center,
      cov = MCD_traf_man$raw.cov
    ) <= qchisq(p = .975, df = p) #p=number of covariates
  )
ind_out_traf_man= setdiff(1:n_2018_no_strike,ind_obs_traf_man) #33 39 77 87


finestra_grafica(t)
plot(Traffic_management$`Traffic management`,Traffic_management$`Avg delay late on arrival`,main='Avg delay late on arrival vs due to traffic management',xlab='Due to traffic management',ylab='Avg delay late on arrival')
points(Traffic_management$`Traffic management`[ind_out_traf_man],Traffic_management$`Avg delay late on arrival`[ind_out_traf_man],col='red')
legend("topleft", legend=levels(factor(c('No outliers','Outliers'))), fill=c('blue','red'), cex=.7)
ind_out_traf_man
Traffic_management[ind_out_traf_man,]
#in questo caso, si può imputare alla tratta 33 il fatto di avere un ritardo elevato per questo motivo: onestamente non saprei
#direi più che vi sia sia un ritardo alto che una proporzione alta

summary(ltsReg(Traffic_management$`Avg delay late on arrival`~Traffic_management$`Traffic management`, alpha=.75,mcd=TRUE)) #R2=0.07599



#rolling stock
#firstly, I visualize the data
finestra_grafica(t)
plot(Rolling_stock$`Rolling stock`,Rolling_stock$`Avg delay late on arrival`)
#robust bivariate
MCD_rol_sto <- covMcd(x = Rolling_stock, alpha = .75, nsamp = "best") 
finestra_grafica(t)
plot(MCD_rol_sto,classic=TRUE)
ind_out_rol_sto <- (1:n_2018_no_strike)[MCD_rol_sto$mcd.wt==0] #3 14 19 26 28 44 54 74 75 76 88 89

##
ind_obs_rol_stock <-
  which(
    mahalanobis(
      x = Rolling_stock,
      center = MCD_rol_sto$raw.center,
      cov = MCD_rol_sto$raw.cov
    ) <= qchisq(p = .975, df = p) #p=number of covariates
  )
ind_out_rol_sto = setdiff(1:n_2018_no_strike,ind_obs_rol_stock) #13  14  19  26  28  44  54  63  74  75  76  88  89 101

finestra_grafica(t)
plot(Rolling_stock$`Rolling stock`,Rolling_stock$`Avg delay late on arrival`,main='Avg delay late on arrival vs due to rolling stock',xlab='Due to rolling stock',ylab='Avg delay late on arrival')
points(Rolling_stock$`Rolling stock`[ind_out_rol_sto],Rolling_stock$`Avg delay late on arrival`[ind_out_rol_sto],col='red')
legend("topleft", legend=levels(factor(c('No outliers','Outliers'))), fill=c('blue','red'), cex=.7)
ind_out_rol_sto
Rolling_stock[ind_out_rol_sto,]
#in questo caso non sembra esserci una relazione tra alta proporzione e alto ritardo: i punti effettivamente fuori
#dalla nuvola di punti sono quelli che hanno una proporzione un po' maggiore di 0.3

summary(ltsReg(Rolling_stock$`Avg delay late on arrival`~Rolling_stock$`Rolling stock`, alpha=.75,mcd=TRUE)) #R2=0.000431


#Station_management
#firstly, I visualize the data
finestra_grafica(t)
plot(Station_management$`Station management`,Station_management$`Avg delay late on arrival`)
#robust bivariate
MCD_stat_man <- covMcd(x = Station_management, alpha = .75, nsamp = "best") 
finestra_grafica(t)
plot(MCD_stat_man,classic=TRUE)
ind_out_stat_man <- (1:n_2018_no_strike)[MCD_stat_man$mcd.wt==0] #23 54 92

##
ind_obs_stat_man <-
  which(
    mahalanobis(
      x = Station_management,
      center = MCD_stat_man$raw.center,
      cov = MCD_stat_man$raw.cov
    ) <= qchisq(p = .975, df = p) #p=number of covariates
  )
ind_out_stat_man = setdiff(1:n_2018_no_strike,ind_obs_stat_man) #23  54  92  94 105

finestra_grafica(t)
plot(Station_management$`Station management`,Station_management$`Avg delay late on arrival`,main='Avg delay late on arrival vs due to station management',xlab='Due to station management',ylab='Avg delay late on arrival')
points(Station_management$`Station management`[ind_out_stat_man],Station_management$`Avg delay late on arrival`[ind_out_stat_man],col='red')
legend("topleft", legend=levels(factor(c('No outliers','Outliers'))), fill=c('blue','red'), cex=.7)
ind_out_stat_man
Station_management[ind_out_stat_man,]
#in questo caso non sembra esserci una relazione tra alta proporzione e alto ritardo: i punti effettivamente fuori
#dalla nuvola di punti sono quelli che hanno una proporzione un po' maggiore di 0.3

summary(ltsReg(Station_management$`Avg delay late on arrival`~Station_management$`Station management`, alpha=.75,mcd=TRUE)) #R2=0.1624


#EXTERNALS: NB: HO USATO ALHPA = 0.75: VEDETE QUALE PREFERITE TRA ESSO E 0.5
#firstly, I visualize the data
finestra_grafica(t)
plot(Externals$Externals,Externals$`Avg delay late on arrival`)
#robust bivariate
MCD_ex <- covMcd(x= Externals, alpha = .5, nsamp = "best") 
#MCD_ex <- covMcd(x= Externals, alpha = .75, nsamp = "best") 
finestra_grafica(t)
plot(MCD_ex,classic=TRUE)
ind_out_ex <- (1:n_2018_no_strike)[MCD_ex$mcd.wt==0] # 0.5: 27 38 48 52 98                     0.75: nulla

##
ind_obs_ex <-
  which(
    mahalanobis(
      x = Externals,
      center = MCD_ex$raw.center,
      cov = MCD_ex$raw.cov
    ) <= qchisq(p = .975, df = p) #p=number of covariates
  )
ind_out_ex = setdiff(1:n_2018_no_strike,ind_obs_ex) # 0.5: 27 37 38 48 52 61 76 89 97 98       0.75: 48

finestra_grafica(t)
plot(Externals$Externals,Externals$`Avg delay late on arrival`,main='Avg delay late on arrival vs due to external causes',xlab='Due to external causes',ylab='Avg delay late on arrival')
points(Externals$Externals[ind_out_ex],Externals$`Avg delay late on arrival`[ind_out_ex],col='red')
legend("topleft", legend=levels(factor(c('No outliers','Outliers'))), fill=c('blue','red'), cex=.7)
ind_out_ex
Externals[ind_out_ex,]
#in questo caso non sembra esserci una relazione tra alta proporzione e alto ritardo: i punti effettivamente fuori
#dalla nuvola di punti sono quelli che hanno una proporzione un po' maggiore di 0.3

summary(ltsReg(Externals$`Avg delay late on arrival`~Externals$Externals, alpha=.75,mcd=TRUE)) #R2=0.07699

#Che tratte?
data_no_strike_2018 = data_no_strike[i_2018,]
#Outliers rail infrastructures
ind_out_rail_inf
data_no_strike_2018[ind_out_rail_inf,]$route
#Outliers traffic management
ind_out_traf_man
data_no_strike_2018[ind_out_traf_man,]$route
#Outliers rolling stock
ind_out_rol_sto
data_no_strike_2018[ind_out_rol_sto,]$route #interessante: molte tratte che partono da Parigi ne risentono
#Outliers station management
ind_out_stat_man
data_no_strike_2018[ind_out_stat_man,]$route
#Outliers externals
ind_out_ex
data_no_strike_2018[ind_out_ex,]$route

#each route that is an outlier her has a station in Paris
#for the internal causes, the outliers are the ones that have an extreme value for the cause, not for a relation with 
# the response
# It changes for external causes, but we know that they have a different behaviour and that we cannot do much
# so it does not make sense to explore them
