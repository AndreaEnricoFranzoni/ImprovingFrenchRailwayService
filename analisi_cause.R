library(readxl)
install.packages('compositions')
library(compositions)

finestra_grafica = function(t){
  if (t==0)
    return(quartz())
  else 
    return(x11())
}
t=0 #per chiamare in automatico il quartz, mettere un qualsiasi altro valore per avere x11()

#valuto l'effetto delle cause anno per anno: lo faccio con il 2018 perchè sono le più recenti
data = read_excel('aggregated_trains_by_year.xlsx')
#tolgo questo outlier(?)
data=data[-which(data$year==2017 & data$route=='PARIS LYON - GRENOBLE'),]
data=data[-which(data$year==2017 & data$route=='SAINT ETIENNE CHATEAUCREUX - PARIS LYON'),]
n = dim(data)[1]

i_2015=which(data$year==2015)
i_2016=which(data$year==2016)
i_2017=which(data$year==2017)
i_2018=which(data$year==2018)
indexes=list(i_2015, i_2016, i_2017, i_2018)
nomi_cause=c('External','Rail infrastructure','Traffic management',
             'Rolling stock','Station management','Travelers') #se mettiamo questi come nomi delle colonne i plot successivi vengono un pochino più carini

#prendo le cause del 2018
cause_2018=data[i_2018,14:19]
#le trasformo
cause_2018_transformed=acomp(cause_2018)
finestra_grafica(t)
plot(cause_2018_transformed)

pc <- princomp(acomp(cause_2015))
pc
summary(pc)
finestra_grafica(t)
plot(pc)
pc$Loadings
