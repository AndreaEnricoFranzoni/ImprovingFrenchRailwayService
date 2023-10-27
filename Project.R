data=read_excel('trains_update_2610.xlsx')
n=dim(data)[1]
n
p=dim(data)[2]
p

unique(data$service)
length(which(data$service=='International'))

summary(data$delay_cause_external_cause )
years = unique(data$year)
months = unique(data$month)
#concentrarsi, solo per i treni nazionali: per ogni, per ogni mese, l'unità diviene la tratta specifica
num_tratte = length(unique(data$route))
cols = (length(unique(data$year)))*(length(unique(data$month)))
dati_nuovi = matrix(nrow=num_tratte, ncol = cols)

r=(unique(data$route))
for (k in 1:num_tratte) {
  for (j in years) {
    for ( i in months){
      if(length(data$delay_cause_rail_infrastructure[which(data$route==r[k] & data$year==j & data$month==i)])!=0)
      dati_nuovi[k,(j-2015)*12 + (i)]=data$delay_cause_rail_infrastructure[which(data$route==r[k] & data$year==j & data$month==i)]
    }
  }
  dati_nuovi[k,48]=r[k]
}

dati_nuovi=data.frame(dati_nuovi)

fun = t(as.matrix(dati_nuovi[1:112,1:47]))
time=1:47

x11()
matplot(time,fun, type='l')


f_data <- fData(time, t(fun))
x11()
plot(f_data)

band_depth <- BD(Data = f_data) #in input va un FD, e ritorna un vector con la band depth di ogni elemento
modified_band_depth <- MBD(Data = f_data) #in input va un FD, e ritorna un vector con la modified band depth di ogni elemento

median_curve <- median_fData(fData = f_data, type = "MBD") # still an fData object: la mediana è la curva con la depth più alta

x11()
plot(f_data) 
lines(time,median_curve$values, lwd=10)
#lines(time,modified_band_depth, lwd=10, col='black')

EI(f_data) #per ogni curva, calcola l'Epigraph Index 
MEI(f_data) #per ogni curva, calcola il Modified EI
HI(f_data) #per ogni curva, calcola l'Hypograph Index 
MHI(f_data) #per ogni curva, calcola il modified Hypograph Index 

#cor_spearman(f_data, ordering='MHI') #(puoi anche mettere MEI, che è la default)
#se i FDs sono bivariati, dà uno scalare, sennò l'intera matrice

#outlier detection
fbplot(f_data, adjust = T) 
outliergram(f_data,adjust = T) 
