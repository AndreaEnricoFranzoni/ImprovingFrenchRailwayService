#semplicemente, provo a rifittare il gam con vari ordini delle colonne
#per vedere i risultati

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
t=0 


data_no_strike = read_excel('aggregated_trains_by_year_nostrike_0402.xlsx')
i_2018=which(data_no_strike$year==2018)
response = data_no_strike$avg_delay_late_on_arrival[i_2018]








#attempt 1
cause_2018_no_strike_1=data.frame(data_no_strike$delay_cause_station_management[i_2018],
  data_no_strike$delay_cause_rail_infrastructure[i_2018],
                                data_no_strike$delay_cause_traffic_management[i_2018],
                                data_no_strike$delay_cause_rolling_stock[i_2018],
                                data_no_strike$delay_cause_external_cause[i_2018]+data_no_strike$delay_cause_travelers[i_2018])
nomi_cause1=c('Station management','Rail infrastructure','Traffic management',
             'Rolling stock','Externals')
colnames(cause_2018_no_strike_1)=nomi_cause1
n_2018_no_strike = dim(cause_2018_no_strike_1)[1]

cause_2018_comp_no_strik_1e=acomp(cause_2018_no_strike_1) 

cause_2018_transformed_1=(-1)*ilr(cause_2018_no_strike_1) 

x1_1 = cause_2018_transformed_1[,1] #(1/sqrt(2))*log(station_management/rail_infrastructure)
x2_1 = cause_2018_transformed_1[,2] #(sqrt(2/3))*log(sqrt(station_management*rail_infrastructure/traffic_management))
x3_1 = cause_2018_transformed_1[,3] #(sqrt(3/4))*log(radice3(station_management*rail_infrastructure*traffic_management)/rolling_stock)
x4_1 = cause_2018_transformed_1[,4] #(sqrt(4/5))*log(radice4(rail_infrastructure*traffic_management*rolling_stock*station_management)/externals)

  
fit_1 = lm( response ~ x1_1 + x2_1 + x3_1 + x4_1)
summary(fit_1)


gam_1=gam(response ~ s(x1_1,bs='cr') + s(x2_1,bs='cr') + s(x3_1,bs='cr') + s(x4_1,bs='cr') )
summary(gam_1)

gam_1_red = gam(response ~ s(x1_1,bs='cr') )
summary(gam_1_red)

anova(gam_1_red,gam_1,test='F')
#I have to reject

gam_1_red = gam(response ~ s(x1_1,bs='cr')+ s(x4_1,bs='cr') )
summary(gam_1_red)

anova(gam_1_red,gam_1,test='F') # I accept
#I have to retain the term with external causes





#attempt 2
cause_2018_no_strike_2=data.frame(
  data_no_strike$delay_cause_external_cause[i_2018]+data_no_strike$delay_cause_travelers[i_2018],
data_no_strike$delay_cause_station_management[i_2018],
                                  data_no_strike$delay_cause_rail_infrastructure[i_2018],
                                  data_no_strike$delay_cause_traffic_management[i_2018],
                                  data_no_strike$delay_cause_rolling_stock[i_2018])
                                  
nomi_cause2=c('Externals','Station management','Rail infrastructure','Traffic management',
              'Rolling stock')
colnames(cause_2018_no_strike_2)=nomi_cause2

cause_2018_comp_no_strik_2e=acomp(cause_2018_no_strike_2) 

cause_2018_transformed_2=(-1)*ilr(cause_2018_no_strike_2) 

x1_2 = cause_2018_transformed_2[,1] #(1/sqrt(2))*log(externals/station_management)
x2_2 = cause_2018_transformed_2[,2] #(sqrt(2/3))*log(sqrt(externals*station_management/rail_infrastructure))
x3_2 = cause_2018_transformed_2[,3] #(sqrt(3/4))*log(radice3(station_management*rail_infrastructure*externals)/traffic_management)
x4_2 = cause_2018_transformed_2[,4] #(sqrt(4/5))*log(radice4(rail_infrastructure*traffic_management*externals*station_management)/rolling_stock)


gam_2 = gam(response ~ s(x1_2,bs='cr') + s(x2_2,bs='cr') + s(x3_2,bs='cr') + s(x4_2,bs='cr') )
summary(gam_2)

gam_2_red = gam(response ~ s(x1_2,bs='cr'))
summary(gam_2_red)

shapiro.test(gam_2_red$residuals)

anova(gam_2_red,gam_2,test='F')
#I reject


gam_2_red = gam(response ~ s(x1_2,bs='cr') + s(x4_2,bs='cr') )
summary(gam_2_red)
anova(gam_2_red,gam_2,test='F')
#I can accept: I have to retain at least two also here: there is always external and station management
#now I try to put in the first term neither one of the two




#attempt 3
cause_2018_no_strike_3=data.frame(
  data_no_strike$delay_cause_rolling_stock[i_2018],
  data_no_strike$delay_cause_traffic_management[i_2018],
  data_no_strike$delay_cause_external_cause[i_2018]+data_no_strike$delay_cause_travelers[i_2018],
  data_no_strike$delay_cause_station_management[i_2018],
  data_no_strike$delay_cause_rail_infrastructure[i_2018])

nomi_cause3=c('Rolling stock','Traffic management','Externals','Station management','Rail infrastructure'
              )
colnames(cause_2018_no_strike_3)=nomi_cause3

cause_2018_comp_no_strik_3e=acomp(cause_2018_no_strike_3) 

cause_2018_transformed_3=(-1)*ilr(cause_2018_no_strike_3) 

x1_3 = cause_2018_transformed_2[,1] #(1/sqrt(2))*log(rolling_stock/traffic_management)
x2_3 = cause_2018_transformed_2[,2] #(sqrt(2/3))*log(sqrt(rolling_stock*traffic_management/externals))
x3_3 = cause_2018_transformed_2[,3] #(sqrt(3/4))*log(radice3(rolling_stock*traffic_management*externals)/station_management)
x4_3 = cause_2018_transformed_2[,4] #(sqrt(4/5))*log(radice4(rolling_stock*traffic_management*externals*station_management)/rail_infrastructure)



gam_3 = gam( response ~ s(x1_3,bs='cr') + s(x2_3,bs='cr') + s(x3_3,bs='cr') + s(x4_3,bs='cr') )
summary(gam_3)

gam_3_red = gam( response ~ s(x1_3,bs='cr') )
summary(gam_3_red)

anova(gam_3_red,gam_3,test='F')
#I have to reject (fiiuuuuu)

gam_3_red = gam( response ~ s(x1_3,bs='cr')  + s(x2_3,bs='cr') )
summary(gam_3_red)

anova(gam_3_red,gam_3,test='F')
#I have also to reject: GOOD

gam_3_red = gam( response ~ s(x1_3,bs='cr') + s(x2_3,bs='cr') + s(x3_3,bs='cr') )
summary(gam_3_red)

anova(gam_3,gam_3_red,test='F')
#I can accept: good, since I had to insert externals and station management to make it meaningful