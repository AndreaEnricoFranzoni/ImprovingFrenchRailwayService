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
finestra_grafica(t)
plot(cause_2018_comp)         #plot dei simplessi

finestra_grafica(t)
barplot(colMeans(cause_2018_comp))

#Trasformazione: centered log ratio transform: data are mapped from S^6 to R^6
cause_2018_transformed=clr(cause_2018) #questi sono i dati trasformati: è un oggetto da S^6 in R^6:
#same transformation used by the SPCA

finestra_grafica(t)
plot(cause_2018_transformed) #scatter dei dati trasformati: COME INTERPRETARLI??




#idea: per prima cosa, dividere in due: cause dovute alla compagnia e no
#in realtà parto dall'averne tre, perchè il simplesso viene più carino
cause_tricotomiche_2018 = data.frame( interne = cause_2018[,2]+cause_2018[,3]+cause_2018[,4]+cause_2018[,5], travelers = cause_2018[,6], other=cause_2018[,6] )
colnames(cause_tricotomiche_2018)=c('Internal','Travelers','Other')
cause_tricotomiche_2018_comp = acomp(cause_tricotomiche_2018)

finestra_grafica(t)
plot(cause_tricotomiche_2018_comp)  
#è un plot brutto da vedere, però mostra come la maggior parte delle cause dei ritardi siano dovuti 
#a un qualcosa di direttamente imputabile alla compagnia

#passo a un dataset bivariato
cause_dicotomiche_2018 = data.frame(interne = cause_tricotomiche_2018$Internal, esterne = cause_tricotomiche_2018$Travelers+cause_tricotomiche_2018$Other)
colnames(cause_dicotomiche_2018)=c('Internal','External')
cause_dicotomiche_2018_comp = acomp(cause_dicotomiche_2018)

finestra_grafica(t)
barplot(colMeans(cause_dicotomiche_2018_comp))

#devo vedere il rapporto tra due dati: isometric log ratio
cause_dicotomiche_2018_trans = ilr(cause_dicotomiche_2018_comp)
# è praticamente (1/radice(2))log(esterne/interne)

finestra_grafica(t)
plot(1:n_2018,cause_dicotomiche_2018_trans)
#sono tutte negative: le interne sono preponderanti rispetto alle esterne



###########
#come funziona: è la PCA normale su una trasformazione specifica:
#io devo passare un oggetto acomp
# di default usa la clr
#pc <- princomp((acomp(cause_2018))) #usa la clr: ho 6 scores perchè sono in R6
#pc = princomp(ilr(acomp(cause_2018)))  #sto mettendo la ilr: ho 5 scores perchè fatto in R5
#pc
#summary(pc)
#finestra_grafica(t)
#plot(pc)
#pc$Loadings
#pc$scores
#biplot(pc)  

pc = princomp(acomp(cause_dicotomiche_2018))
pc
summary(pc)
finestra_grafica(t)
plot(pc)
pc$Loadings
pc$scores
biplot(pc)  #non funziona perchè ho solo una PC importante

# Per Andrea o chi lo farà: 
# Partire da regressione di avg_delay_late_at_arrival con tutti i loadings insieme e valutare se ci sono coefficienti non significativi.
# Se ci sono coeff non significativi, interpretare quali cause non sono da considerare secondo la regressione
# Utilizzare i loading con coefficiente significativo per fare robust multivariato a 2 a 2 (un loading e il delay ogni volta)
# per capire quali tratte sono outlier secondo quel loading (e dunque secondo le cause associate)
# è giusto farlo sul 2018 perché sono le più recenti e dunque possiamo dire alla compagnia di intervenire se ci sono risultati
# significativi

#in realtà, provo con
response = data$avg_delay_late_on_arrival[i_2018]
#response = data$avg_delay_late_on_arrival[i_2018] - data$avg_delay_late_at_departure[i_2018]

#first model: about only the two
clr_internal = pc$scores[,1] 
clr_external = pc$scores[,2]

#linear model
fit_1 = lm( response ~ clr_internal + clr_external )
summary(fit_1)
shapiro.test(fit_1$residuals)
#considero i residui gaussiani: il test globale è esatto: tutti i coefficienti nulli
#not a good thing

#I take into account the 4 internal causes separately, putting together the other
#two: traverls and external

cause_2018_dati = data.frame(cause_2018$`Rail infrastructure`,
                             cause_2018$`Traffic management`,
                             cause_2018$`Rolling stock`,
                             cause_2018$`Station management`,
                             cause_2018$External+cause_2018$Travelers
                             )
colnames(cause_2018_dati) = c('Rail infrastructure','Traffic management','Rolling stock', 'Station management','Externals')
cause_2018_dati_comp = acomp(cause_2018_dati)

finestra_grafica(t)
plot(cause_2018_dati_comp)  
#it makes sense to try to understand how that works

trasf_cause = ilr(cause_2018_dati)
#what I have: I am in R4, having the log of the ratio between all the internal over
# the externals: take a look to better see it

x1 = trasf_cause[,1] #costante*log(rail_infrastructure/traffic_management)
x2 = trasf_cause[,2] #costante*log(rail_infrastructure*traffic_management/Rolling_stock)
x3 = trasf_cause[,3] #costante*log(rail_infrastructure*traffic_management*Rolling_stock/station_management)
x4 = trasf_cause[,4] #costante*log(prodotto_cause_interne/cause_esterne)

#interpretazione:
# x1: quanto rail_inf sia maggiore di traffic_management
# x2: quanto l'interazione tra rail_infrastructure e traffic_management sia maggiore di rolling stock
# x3: quanto l'interazione tra rail_infrastructure e traffic_management e rolling stock sia maggiore di station_management
# x4: quanto l'interazione tra le cause interne sia maggiore di quelle esterne






######QUESTA PARTE SE LA RISPOSTA E' SOLO IL RITARDO ALL'ARRIVO
fit_2 = lm( response ~ x1 + x2 + x3 + x4)
summary(fit_2)
shapiro.test(fit_2$residuals)
#gaussian

vif(fit_2)
#no problem of collinearity



fit_2_red = lm( response ~ x1 + x3 + x4)
summary(fit_2_red)

fit_2_red_2 = lm(response ~ x3 + x4)
summary(fit_2_red_2)
#it is a model based on the interaction with rolling stock and all the others
#problema: R^2 bassissimo


pc = princomp(trasf_cause)
pc
summary(pc)
finestra_grafica(t)
plot(pc)
pc$Loadings
pc$scores
biplot(pc)  
#la differenza tra le teste degli archi rappresenta log-ratio tra le due componenti:
# più sono diversi, più è grande, e dunque il loro rapporto è tanto più >1
#Penso che sia meglio fare il modello con la trasformazione piuttosto che con la PCA

#ora provo a fare un gam, per 
model_gam=gam(response ~ s(x1,bs='cr') + s(x2,bs='cr') + s(x3,bs='cr') + s(x4,bs='cr') )
summary(model_gam)
#it seems to explaining it better
shapiro.test(model_gam$residuals)
#I have gaussianity

#since I have already seen that there is linearity between response and x3 and x4:
# I try the semiparametric one to see if it is better
model_semiparam = gam(response ~ s(x1,bs='cr') + s(x2,bs='cr') + x3 + x4 )
summary(model_semiparam)
#R^2 is not better anymore: I try the ANOVA test
shapiro.test(model_semiparam$residuals)
#I can do it parametric: it is more powerful

anova(model_semiparam,model_gam, test = "F") 
#the pavalue is 0.02955: I have to reject H0: I have to retain the nonparametric model
#but I try another one
model_gam_red=gam(response ~ s(x2,bs='cr') + s(x3,bs='cr') + s(x4,bs='cr') )
summary(model_gam_red)
#ANOVA test:
anova(model_gam_red,model_gam, test = "F") 
#pvalue: 0.5846: I can use the reduced: so it seems that the first one(related to rail infrastrcuture, does not influence anything)

model = model_gam_red # y = f2(x2) + f3(x3) + f4(x4) + eps
summary(model)

finestra_grafica(t)
#par(mfrow=c(1,3))
plot(model)

bounds_x1 = c(min(x1),max(x1))
bounds_x2 = c(min(x2),max(x2))
bounds_x3 = c(min(x3),max(x3))
bounds_x4 = c(min(x4),max(x4))
#I try maybe something not very cool: prendo questi bounds e faccio sampling 
# con le uniformi su questi bounds
install.packages('lhs')
library(lhs)

sample1 = randomLHS(1000,3)
sample1_gam_red = data.frame(bounds_x2[1] + (bounds_x2[2]-bounds_x2[1])*sample1[,1],
                        bounds_x3[1] + (bounds_x3[2]-bounds_x3[1])*sample1[,2],
                        bounds_x4[1] + (bounds_x4[2]-bounds_x4[1])*sample1[,3]
                        )
colnames(sample1_gam_red)=c('x2','x3','x4')
sample2 = randomLHS(1000,3)
sample2_gam_red = data.frame(bounds_x2[1] + (bounds_x2[2]-bounds_x2[1])*sample2[,1],
                        bounds_x3[1] + (bounds_x3[2]-bounds_x3[1])*sample2[,2],
                        bounds_x4[1] + (bounds_x4[2]-bounds_x4[1])*sample2[,3]
                        )
colnames(sample2_gam_red)=c('x2','x3','x4')

S <- sobol(model = model_gam_red, X1 = sample1_gam_red, X2 = sample2_gam_red, order = 1, nboot = 100)
print(S)

finestra_grafica(t)
ggplot(S)
#x3 seems to be the most relevant features: the introduction of the rolling stock in the 
#variables made it increase

#for the sake of curiosity: I run it on the complete gam
sample1 = randomLHS(1000,4)
sample1_gam = data.frame(    bounds_x1[1] + (bounds_x1[2]-bounds_x1[1])*sample1[,1],
                             bounds_x2[1] + (bounds_x2[2]-bounds_x2[1])*sample1[,2],
                             bounds_x3[1] + (bounds_x3[2]-bounds_x3[1])*sample1[,3],
                             bounds_x4[1] + (bounds_x4[2]-bounds_x4[1])*sample1[,4]
)
colnames(sample1_gam)=c('x1','x2','x3','x4')
sample2 = randomLHS(1000,4)
sample2_gam = data.frame(bounds_x1[1] + (bounds_x1[2]-bounds_x1[1])*sample2[,1],
                         bounds_x2[1] + (bounds_x2[2]-bounds_x2[1])*sample2[,2],
                         bounds_x3[1] + (bounds_x3[2]-bounds_x3[1])*sample2[,3],
                         bounds_x4[1] + (bounds_x4[2]-bounds_x4[1])*sample2[,4]
)
colnames(sample2_gam)=c('x1','x2','x3','x4')

S_gam <- sobol(model = model_gam, X1 = sample1_gam, X2 = sample2_gam, order = 1, nboot = 100)
print(S_gam)

finestra_grafica(S_gam)
ggplot(S)

#it demonstrates what we have seen until now: rail infrastructure is not so important.
#Taking account of the rolling stock increases it


#TODO: taking account of the variables one at the time as regressor
#Firstly: linear model: do not make sense obv making the gam since it is already here

fit_x1 = lm(response ~ x1)
summary(fit_x1)
#questo modello non è assolutamente significativo dal momento che ha un R2 di 0.02922

fit_x2 = lm(response ~ x2)
summary(fit_x2)
#questo modello non è assolutamente significativo dal momdento che ha un R2 di 0.006124

fit_x3 = lm(response ~ x3)
summary(fit_x3)
#questo modello non è assolutamente significativo dal momdento che ha un R2 di 0.1241

fit_x4 = lm(response ~ x4)
summary(fit_x4)
#questo modello non è assolutamente significativo dal momdento che ha un R2 di 0.1202

#model to be used: the gam above this section
















######QUESTA PARTE SE LA RISPOSTA E' IL RITARDO ACCUMULATO
fit_2 = lm( response ~ x1 + x2 + x3 + x4)
summary(fit_2)
shapiro.test(fit_2$residuals)
#i residui sono gaussiani: posso considerare i test esatti

fit_2_red = lm( response ~ x2 + x3 + x4)
summary(fit_2_red)
shapiro.test(fit_2_red$residuals)

fit_2_red_2 = lm( response ~ x3 + x4)
summary(fit_2_red_2)
shapiro.test(fit_2_red_2$residuals)

fit_2_red_3 = lm( response ~ x4)
summary(fit_2_red_3)
shapiro.test(fit_2_red_3$residuals)

#in this case, the interaction between the four internal causes is the only one that matters:
# it is the inclusion of Station management that make that regressor matters
# the coefficient is positive: so, if x4 increases, also the response does
# the R^2 is 0.2106: not a lot

#ora provo a fare un gam, per 
model_gam=gam(response ~ s(x1,bs='cr') + s(x2,bs='cr') + s(x3,bs='cr') + s(x4,bs='cr') )
summary(model_gam)
#it seems to explaining it better
shapiro.test(model_gam$residuals)
#I have gaussianity

#there is linearity between some coefficients
model_gam_red = gam(response ~ s(x2,bs='cr') + s(x3,bs='cr') + s(x4,bs='cr')  )
summary(model_gam_red)
# I have gaussianity

model_gam_red_2 = gam(response ~  s(x3,bs='cr') + s(x4,bs='cr')  )
summary(model_gam_red_2)

anova(model_gam_red_2,model_gam, test = "F") 
#pvalue high: 0.6594: I take the reduced

model_semiparam_red_2 = gam(response ~  s(x3,bs='cr') + x4  )
summary(model_semiparam_red_2)

anova(model_semiparam_red_2,model_gam_red_2)


#ANOVA permutational: il test parametrico non plotta il pvalue
fitted.obs <- model_semiparam_red_2$fitted.values 
res.obs <- model_semiparam_red_2$residuals
T_0 <- anova(model_semiparam_red_2,model_gam_red_2, test = "F")$F[2]
B=1000
T2 <- numeric(B)
set.seed(230300)
for (perm in 1:B) {
  res_reduced_perm <- res.obs[sample(1:length(i_2018))]
  y_perm <- fitted.obs + res_reduced_perm

  
  model_semiparam_red_perm = gam(y_perm ~  s(x3,bs='cr') + x4  )
  model_gam_red_perm = gam(y_perm ~ s(x3,bs='cr') + s(x4,bs='cr'))
  T2[perm] <- anova(model_semiparam_red_perm,model_gam_red_perm, test = "F")$F[2]
  
}
pval=sum(T2>=T_0)/B
pval






