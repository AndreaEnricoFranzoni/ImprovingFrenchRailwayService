library(readxl)
library(writexl)
rm(list=ls())
graphics.off()


data = read_excel('aggregated_trains_by_year.xlsx')

external = data$delay_cause_external_cause
rail = data$delay_cause_rail_infrastructure
rolling = data$delay_cause_rolling_stock
station = data$delay_cause_station_management
traffic = data$delay_cause_traffic_management
travelers = data$delay_cause_travelers

n=dim(data)[1]
main_cause = rep(NA, n)
cont = rep(0, n)
argmax = pmax(external, rail, rolling, station, traffic, travelers)
for(i in 1:n){
  if(argmax[i]==external[i]){
  cont[i]=cont[i]+1
  main_cause[i] = 'External'
  }
  if(argmax[i]==rail[i]){
    cont[i]=cont[i]+1
    main_cause[i] = 'Rail'
  }
  if(argmax[i]==rolling[i]){
    cont[i]=cont[i]+1
    main_cause[i] = 'Rolling'
  }
  if(argmax[i]==station[i]){
    cont[i]=cont[i]+1
    main_cause[i] = 'Station'
  }
  if(argmax[i]==traffic[i]){
    cont[i]=cont[i]+1
    main_cause[i] = 'Traffic'
  }
  if(argmax[i]==travelers[i]){
    cont[i]=cont[i]+1
    main_cause[i] = 'Travelers'
  }
}
cont[which(cont!=1)]

data$main_cause = main_cause


write_xlsx(data,'trains_by_cause.xlsx')
