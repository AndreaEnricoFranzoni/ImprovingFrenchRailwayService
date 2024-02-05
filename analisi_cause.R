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
# x1: quanto rail_inf sia maggiore di traffic_management
# x2: quanto l'interazione tra rail_infrastructure e traffic_management sia maggiore di rolling stock
# x3: quanto l'interazione tra rail_infrastructure e traffic_management e rolling stock sia maggiore di station_management
# x4: quanto l'interazione tra le cause interne sia maggiore di quelle esterne


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
# y = x3 + f4(x4) + eps

finestra_grafica(t)
plot(model_no_strike)
