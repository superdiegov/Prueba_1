

######################################################################################
#####SNASPE: Tendencias y Proyección de Visitaciones Anuales Periodo 1990-2026###############################
######################################################################################

### vISITAS ANUALES TOTALES
##Cambio de directorio
setwd("C:/CONAF/ReporteVisitantes/Salidas de R/Nueva_prueba")

visitas.anuales<-read.table("visitas.anuales.txt",header=T)


tail(visitas.anuales)


data<-subset(visitas.anuales,visitas.anuales$variable == "Total")

str(data)
head(data)
tail(data)
length(unique(data$Id2))

# Eliminar unidades con cero visitas en el periodo 1990-2016

dat0 <- subset(data,Id2!="100.RN.Katalalixar" & Id2!="109.MN.Laguna_de_los_Cisnes" & Id2!="111.PN.Alberto_de_Agostini" & Id2!="12.PN.Llullaillaco"& Id2!="22.MN.Isla_Cachagua"& Id2!="24.RN.Rio_Blanco"& Id2!="34.PN.Las_Palmas_de_Cocalan" & Id2!="64.PN.VILLARICA.SUR"& Id2!="67.SN.RIO_CRUCES"& Id2!="79.PN.Corcovado"& Id2!="81.RN.Lago_Rosselot"& Id2!="83.RN.Lago_Carlota"&Id2!="85.PN.Isla_Magdalena"&Id2!="86.PN.Isla_Guamblin"&Id2!="88.MN.Cinco_Hermanas"&Id2!="93.RN.Las_Guaitecas"&Id2!="95.RN.LAGO_GENERAL_CARRERA")                                     

dat0$Id2 <- factor(dat0$Id2)     # this is needed to remove   t h e   l e v e l s   o f   the excluded units
length(unique(dat0$Id2))

sum.anual<-aggregate(value~year,data=dat0,sum)
is.data.frame(sum.anual)
names(sum.anual)
head(sum.anual)



library(ggplot2)

options(scipen=12)

pdf(file="Figura1_v2.pdf", height=17/2.54, width = 30/2.54)#size in cm
par(bg="white")
p<-ggplot(sum.anual, aes(year, value)) + geom_line() + geom_point(size=2)+
  xlab("") + ylab("n°de visitantes\n")+scale_x_continuous(breaks=c(1990,1994,2000,2006,2010,2014,
                                                                   2018), labels=c(1990,1994,2000,2006,2010,2014,
                                                                                   2018))
p+theme(text = element_text(size=12),axis.text=element_text(size=12),axis.text.y = element_text())
dev.off()


#Standarized measures of rate of change in the time series for total visits in the SNASPE
#m1<-lm(sum.anual$value~sum.anual$year)
#summary(m1)


#Stnd.RateChange<-(coef(m1)[2]/max(predict.lm(m1)))
#Stnd.RateChange


###################################
#Variacion anual tasa de visitacion-FIGURA S1
#############################

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors<-ts(sum.anual$value, start=1990,end=2017, frequency = 1)


#Change in visitors

diff.visitors<-(diff(visitors))

Percent.Change<-(diff.visitors/sum.anual$value[1:27])*100
Percent.Change

#log.return<-diff(log(visitors)) #tasa de retorno
#log.return

#plotting percent of change
#plot(log.return)

plot(Percent.Change)

#TRANSFORMAR SERIES DE TIEMPO A NUMEROS PARA PLOT
library(zoo)


df <- data.frame(date=as.Date(as.yearqtr(time(Percent.Change))), Y=as.matrix(Percent.Change))
df$date<-as.numeric(df$date)
library(ggplot2)

pdf(file="FigureS1_g.pdf", height=15/2.54, width = 20/2.54)#size in cm
par(bg="white")
anual.PChange<-ggplot(df, aes(date,Y)) + geom_line() + geom_point(size=2)+
  xlab("") + ylab("Tasa de cambio (%)\n")
                                                                                      
anual.PChange+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text())
dev.off()


# promedio de las tasas de cambio anuales para el periodo 1990-2016

mean(Percent.Change)

#########################################################################
############# PROYECCION VISITANTES A 10 AÑOS
#############################################################################


#####Three different time series models

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 20 years

library(forecast)
library(xts)

m_ets = ets(visitors)
f_ets = forecast(m_ets, h=10) # forecast 20 years into the future
plot(f_ets)
title(main = "Exponential State Smoothing")

pdf(file="ETS.1990-2036_v3.pdf")
par(bg="white")
plot(f_ets, xlab='',ylab="Visitantes")
dev.off()


#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=10)
plot(f_aa)


pdf(file="ARIMA.1990-2036_v2.pdf")
par(bg="white")
plot(f_aa, xlab='',ylab="Visitantes")
dev.off()

#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors)
f_tbats = forecast(m_tbats, h=10)
plot(f_tbats)

pdf(file="TBATS.1990-2036.pdf")
par(bg="white")
plot(f_tbats, xlab='',ylab="Visitantes")
dev.off()

#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="grey",
        ylab="AIC")


#We see that the ARIMA model performs the best. We have to turn the output of forecast() into a data.frame.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors)[length(visitors)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 



forecast_df = data.frame(visitors_predicted=f_aa$mean,
                         visitors_lower=f_aa$lower[,2],
                         visitors_upper=f_aa$upper[,2],
                         date=last_date + seq(1, 10, by=1))

(forecast_df)



write.table(forecast_df,file="prediccion_2018_2027.txt")
write.table(f_tbats,file="prediccions.txt")




##############################################################################################
#DESGLOSE DE VISITANTES por nacionalidad
###############################################################################################


data.nac<-subset(visitas.anuales,visitas.anuales$variable == "Ext"|visitas.anuales$variable == "CL")

str(data.nac)
head(data.nac)

length(unique(data.nac$Id2))


# Eliminar unidades con cero visitas en el periodo 1990-2016
#wide.format<-xtabs(visitas.anuales$value~visitas.anuales$Id2+visitas.anuales$year)
#write.table(wide.format, file="visitas_historicasWide.txt")

dat.nac <- subset(data.nac,Id2!="100.RN.Katalalixar" & Id2!="109.MN.Laguna_de_los_Cisnes" & Id2!="111.PN.Alberto_de_Agostini" & Id2!="12.PN.Llullaillaco"& Id2!="22.MN.Isla_Cachagua"& Id2!="24.RN.Rio_Blanco"& Id2!="34.PN.Las_Palmas_de_Cocalan" & Id2!="64.PN.VILLARICA.SUR"& Id2!="67.SN.RIO_CRUCES"& Id2!="79.PN.Corcovado"& Id2!="81.RN.Lago_Rosselot"& Id2!="83.RN.Lago_Carlota"&Id2!="85.PN.Isla_Magdalena"&Id2!="86.PN.Isla_Guamblin"&Id2!="88.MN.Cinco_Hermanas"&Id2!="93.RN.Las_Guaitecas"&Id2!="95.RN.LAGO_GENERAL_CARRERA")                                     

dat.nac$Id2 <- factor(dat.nac$Id2)     # this is needed to remove   t h e   l e v e l s   o f   the excluded units
length(unique(dat.nac$Id2))



sum.anual.nac<-aggregate(value~year+variable,data=dat.nac,sum)
is.data.frame(sum.anual.nac)
names(sum.anual.nac)
head(sum.anual.nac)



library(ggplot2)
options(scipen=10)

pdf(file="Visitantes.Nacionalidad.pdf", height=15/2.54, width = 20/2.54)#size in cm
par(bg="white")
p<-ggplot(sum.anual.nac, aes(year, value, colour=variable, shape=variable, group=variable)) + geom_line() + geom_point(size=2)+
  xlab("") + ylab("n°de visitantes\n")+scale_x_continuous(breaks=c(1992,1996,2000,2004,2008,2012,
                                                                   2016), labels=c(1992,1996,2000,2004,2008,2012,
                                                                                   2016))+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text())
p+scale_colour_discrete(name="Nacionalidad",breaks=c("CL", "Ext"),labels=c("Chilenos", "Extranjeros"))+scale_shape_discrete(name="Nacionalidad",breaks=c("CL", "Ext"),labels=c("Chilenos", "Extranjeros"))
dev.off()


##########################################
#Chilenos
######################################

dat.cl<-subset(sum.anual.nac,sum.anual.nac$variable == "CL")

#Standarized measures of rate of change
m3<-lm(dat.cl$value~dat.cl$year)
summary(m3)


Stnd.RateChange<-(coef(m3)[2]/max(predict.lm(m3)))
Stnd.RateChange


#mAKE chilean VISITOR VARIABLE A TIME SERIE OBJECT

visitors.cl<-ts(dat.cl$value, start=1990,end=2016, frequency = 1)


#Change in visitors

diff.visitors.cl<-(diff(visitors.cl))

Percent.Change<-(diff.visitors.cl/dat.cl$value[1:26])*100
Percent.Change
mean(Percent.Change)

log.return<-diff(log(visitors.cl))
log.return


###############################
#Per capita chilean visitors
##################################

dat.cl$cl.population<-c(13178782,13422010,13665241,13908473,14151708,14394940,14595504,14796076,14996647,15197213,15397784,15571679,15745583,15919479,16093378,16267278,16432674,16598074,16763470,16928873,17094275,17248450,17402630,17556815,17711004,17865185,18001964)

dat.cl$per.capita<-dat.cl$value/dat.cl$cl.population

pdf(file="Visitantes.Cl_erCapita.Total.pdf", height=15/2.54, width = 20/2.54)#size in cm
par(bg="white")
options(scipen=10)
pc<-ggplot(dat.cl, aes(year, per.capita)) + geom_line() + geom_point(size=2)+
  xlab("") + ylab("visitantes per cápita\n")+scale_x_continuous(breaks=c(1992,1996,2000,2004,2008,2012,
                                                                         2016), labels=c(1992,1996,2000,2004,2008,2012,2016))+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text())
pc
dev.off()



#Standarized measures of rate of change per capita visitors

m2<-lm(dat.cl$per.capita~dat.cl$year)
summary(m2)


PerCapita.RateChange<-(coef(m2)[2]/max(predict.lm(m2)))
PerCapita.RateChange



#change Per Capita

#mAKE per.capita VARIABLE A TIME SERIE OBJECT

visitors.cl.pc<-ts(dat.cl$per.capita, start=1990,end=2016, frequency = 1)

diff.per.capita<-(diff(visitors.cl.pc))

Percent.percapita.Change<-(diff.per.capita/dat.cl$per.capita[1:26])*100
Percent.percapita.Change
plot(Percent.percapita.Change)
mean(Percent.percapita.Change)

log.return.pc<-diff(log(visitors.cl.pc))
log.return.pc
mean(log.return.pc)



######################################
#Extranjeros
#####################################

dat.ext<-subset(sum.anual.nac,sum.anual.nac$variable == "Ext")

#Standarized measures of rate of change
m4<-lm(dat.ext$value~dat.ext$year)
summary(m4)


Stnd.RateChange<-(coef(m4)[2]/max(predict.lm(m4)))
Stnd.RateChange


#mAKE foreging VISITOR VARIABLE A TIME SERIE OBJECT

visitors.ext<-ts(dat.ext$value, start=1990,end=2016, frequency = 1)


#Change in visitors

diff.visitors.ext<-(diff(visitors.ext))

Percent.Change<-(diff.visitors.ext/dat.ext$value[1:26])*100
Percent.Change
mean(Percent.Change)

log.return<-diff(log(visitors.ext))
log.return



##############################################################################################
#DESGLOSE DE VISITANTES por SEXO
###############################################################################################

data.sex<-subset(visitas.anuales,visitas.anuales$variable == "Fem"|visitas.anuales$variable == "Mas")

str(data.sex)
head(data.sex)

length(unique(data.sex$Id2))


# Eliminar unidades con cero visitas en el periodo 1990-2016
#wide.format<-xtabs(visitas.anuales$value~visitas.anuales$Id2+visitas.anuales$year)
#write.table(wide.format, file="visitas_historicasWide.txt")

dat.sex <- subset(data.sex,Id2!="100.RN.Katalalixar" & Id2!="109.MN.Laguna_de_los_Cisnes" & Id2!="111.PN.Alberto_de_Agostini" & Id2!="12.PN.Llullaillaco"& Id2!="22.MN.Isla_Cachagua"& Id2!="24.RN.Rio_Blanco"& Id2!="34.PN.Las_Palmas_de_Cocalan" & Id2!="64.PN.VILLARICA.SUR"& Id2!="67.SN.RIO_CRUCES"& Id2!="79.PN.Corcovado"& Id2!="81.RN.Lago_Rosselot"& Id2!="83.RN.Lago_Carlota"&Id2!="85.PN.Isla_Magdalena"&Id2!="86.PN.Isla_Guamblin"&Id2!="88.MN.Cinco_Hermanas"&Id2!="93.RN.Las_Guaitecas"&Id2!="95.RN.LAGO_GENERAL_CARRERA")                                     

dat.sex$Id2 <- factor(dat.sex$Id2)     # this is needed to remove   t h e   l e v e l s   o f   the excluded units
length(unique(dat.sex$Id2))



sum.anual.sex<-aggregate(value~year+variable,data=dat.sex,sum)
is.data.frame(sum.anual.sex)
names(sum.anual.sex)
head(sum.anual.sex)



library(ggplot2)
options(scipen=10)

pdf(file="Visitantes.Sexo.pdf", height=15/2.54, width = 20/2.54)#size in cm
par(bg="white")
p<-ggplot(sum.anual.sex, aes(year, value, colour=variable, shape=variable, group=variable)) + geom_line() +
  xlab("") + ylab("N°de visitantes\n")+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text())
p+scale_colour_discrete(name="Sexo",breaks=c("Fem", "Mas"),labels=c("Femenino", "Masculino"))
dev.off()




################
# Visitantes Femeninas
###############

dat.fem<-subset(sum.anual.sex,sum.anual.sex$variable == "Fem")

#Standarized measures of rate of change
m5<-lm(dat.fem$value~dat.fem$year)
summary(m5)


Stnd.RateChange<-(coef(m5)[2]/max(predict.lm(m5)))
Stnd.RateChange


#mAKE foreging VISITOR VARIABLE A TIME SERIE OBJECT

visitors.fem<-ts(dat.fem$value, start=1990,end=2016, frequency = 1)


#Change in visitors

diff.visitors.fem<-(diff(visitors.fem))

Percent.Change<-(diff.visitors.fem/dat.fem$value[1:26])*100
Percent.Change
mean(Percent.Change)



################
# Visitantes Masculinos
###############

dat.mas<-subset(sum.anual.sex,sum.anual.sex$variable == "Mas")

#Standarized measures of rate of change
m6<-lm(dat.fem$value~dat.fem$year)
summary(m6)


Stnd.RateChange<-(coef(m6)[2]/max(predict.lm(m6)))
Stnd.RateChange


#mAKE foreging VISITOR VARIABLE A TIME SERIE OBJECT

visitors.mas<-ts(dat.mas$value, start=1990,end=2016, frequency = 1)


#Change in visitors

diff.visitors.mas<-(diff(visitors.mas))

Percent.Change<-(diff.visitors.mas/dat.mas$value[1:26])*100
Percent.Change
mean(Percent.Change)


##############################################################################################
#DESGLOSE DE VISITANTES por GRUPO ETARIO
###############################################################################################

data.group<-subset(visitas.anuales,visitas.anuales$variable == "Adu"|visitas.anuales$variable == "ADULTOS.MAYOR"|visitas.anuales$variable == "Men")

str(data.group)
head(data.group)

length(unique(data.group$Id2))


# Eliminar unidades con cero visitas en el periodo 1990-2016
#wide.format<-xtabs(visitas.anuales$value~visitas.anuales$Id2+visitas.anuales$year)
#write.table(wide.format, file="visitas_historicasWide.txt")

dat.group <- subset(data.group,Id2!="100.RN.Katalalixar" & Id2!="109.MN.Laguna_de_los_Cisnes" & Id2!="111.PN.Alberto_de_Agostini" & Id2!="12.PN.Llullaillaco"& Id2!="22.MN.Isla_Cachagua"& Id2!="24.RN.Rio_Blanco"& Id2!="34.PN.Las_Palmas_de_Cocalan" & Id2!="64.PN.VILLARICA.SUR"& Id2!="67.SN.RIO_CRUCES"& Id2!="79.PN.Corcovado"& Id2!="81.RN.Lago_Rosselot"& Id2!="83.RN.Lago_Carlota"&Id2!="85.PN.Isla_Magdalena"&Id2!="86.PN.Isla_Guamblin"&Id2!="88.MN.Cinco_Hermanas"&Id2!="93.RN.Las_Guaitecas"&Id2!="95.RN.LAGO_GENERAL_CARRERA")                                     

dat.group$Id2 <- factor(dat.group$Id2)     # this is needed to remove   t h e   l e v e l s   o f   the excluded units
length(unique(dat.group$Id2))



sum.anual.group<-aggregate(value~year+variable,data=dat.group,sum)
is.data.frame(sum.anual.group)
names(sum.anual.group)
head(sum.anual.group)



library(ggplot2)
options(scipen=10)

pdf(file="Visitantes.grupoEtario.pdf", height=15/2.54, width = 20/2.54)#size in cm
par(bg="white")
p<-ggplot(sum.anual.group, aes(year, value, colour=variable, shape=variable, group=variable)) + geom_line() + geom_point(size=2)+
  xlab("") + ylab("n°de visitantes\n")+scale_x_continuous(breaks=c(1992,1996,2000,2004,2008,2012,
                                                                   2016), labels=c(1992,1996,2000,2004,2008,2012,
                                                                                   2016))+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text())
p+scale_colour_discrete(name="Grupo etario",breaks=c("Adu", "ADULTOS.MAYOR","Men"),labels=c("Adultos", "Adultos mayores","Menores"))+scale_shape_discrete(name="Grupo etario",breaks=c("Adu", "ADULTOS.MAYOR","Men"),labels=c("Adultos", "Adultos mayores","Menores"))
dev.off()





################
# Visitantes Adultos
###############

dat.adu<-subset(sum.anual.group,sum.anual.group$variable == "Adu")

#Standarized measures of rate of change
m6<-lm(dat.adu$value~dat.adu$year)
summary(m6)


Stnd.RateChange<-(coef(m6)[2]/max(predict.lm(m6)))
Stnd.RateChange


#mAKE foreging VISITOR VARIABLE A TIME SERIE OBJECT

visitors.adu<-ts(dat.adu$value, start=1990,end=2016, frequency = 1)

visitors.adu
#Change in visitors

diff.visitors.adu<-(diff(visitors.adu))

Percent.Change<-(diff.visitors.adu/dat.adu$value[1:26])*100
Percent.Change
mean(Percent.Change)



################
# Visitantes adultos mayores
###############


dat.adum<-subset(sum.anual.group,sum.anual.group$variable == "ADULTOS.MAYOR" & sum.anual.group$value>0)
dat.adum

#Standarized measures of rate of change
m7<-lm(dat.adum$value~dat.adum$year)
summary(m7)


Stnd.RateChange<-(coef(m7)[2]/max(predict.lm(m7)))
Stnd.RateChange


#mAKE foreging VISITOR VARIABLE A TIME SERIE OBJECT

visitors.adum<-ts(dat.adum$value, start=2006,end=2016, frequency = 1)


#Change in visitors

diff.visitors.adum<-(diff(visitors.adum))

Percent.Change<-(diff.visitors.adum/dat.adum$value[2:11])*100
Percent.Change
mean(Percent.Change)#######eliminar NAS


################
# Visitantes menores
###############


dat.men<-subset(sum.anual.group,sum.anual.group$variable == "Men")
dat.men

#Standarized measures of rate of change
m8<-lm(dat.men$value~dat.men$year)
summary(m8)


Stnd.RateChange<-(coef(m8)[2]/max(predict.lm(m8)))
Stnd.RateChange


#mAKE foreging VISITOR VARIABLE A TIME SERIE OBJECT

visitors.men<-ts(dat.men$value, start=2006,end=2016, frequency = 1)


#Change in visitors

diff.visitors.men<-(diff(visitors.men))

Percent.Change<-(diff.visitors.men/dat.men$value[2:11])*100
Percent.Change
mean(Percent.Change)#######eliminar NAS



################################################################################
########################################VARIACION GEOGRAFICA EN TENDENCIA DE VISITACIONES
################################################################################
######MACROZONA NORTE

visitas.anuales<-read.table("visitas.anuales.txt",header=T)


tail(visitas.anuales)


data<-subset(visitas.anuales,visitas.anuales$variable == "Total")

str(data)
head(data)

length(unique(data$Id2))


# To change plot order of facet wrap,
# change the order of varible levels with factor()
data$Id2 <- factor(data$Id2, levels = c("1.PN.Lauca", "3.RN.las_Vicunas", "4.MN.Salar_de_Surire","5.PN.Volcan_Isluga","6.RN.Pampa_del_Tamarugal","7.RN.Los_Flamencos","8.MN.La_Portada","9.RN.La_Chimba","11.MN.Paposo_Norte","12.PN.Llullaillaco","13.PN.Pan_de_Azucar","14.PN.Nevado_de_Tres_Cruces", 
                                        "15.PN.Llanos_de_Challe","17.RN.Pinguino_de_Humboldt.2","18.PN.Bosque_Fray_Jorge","19.MN.Pichasca","20.RN.Las_Chinchillas","21.PN.Rapa_Nui"                
                                        ,"22.MN.Isla_Cachagua","23.PN.La_Campana","24.RN.Rio_Blanco","25.RN.Lago_Penuelas","26.RN.El_Yali","27.SN.JARDIN_BOTANICO"         
                                        ,"28.SN.LAGUNA_EL_PERAL","29.PN.Arch_de_Juan_Fernandez","30.RN.Rio_Clarillo","31.MN.El_Morado","32.SN.YERBA_LOCA",              
                                        "33.RN.Rob_del_Cobre_de_Loncha","34.PN.Las_Palmas_de_Cocalan","35.RN.Rio_de_los_Cipreses","36.RN.Laguna_Torca","37.PN.Radal_Siete_Tazas","39.RN.Altos_de_Lircay",         
                                        "40.RN.Federico_Albert","41.RN.Los_Ruiles", "42.RN.Los_Bellotos_del_Melado","43.RN.Los_Queules","44.RN.Los_Huemules_de_Niblinto","45.RN.Nonguen","46.RN.nuble","47.PN.Laguna_del_Laja"         
                                        ,"48.PN.Nahuelbuta","49.RN.Ralco" ,"50.RN.Altos_de_Pemehue","51.RN.Malleco","52.MN.Contulmo",
                                        "53.PN.Tolhuaca","54.RN.Nalcas","55.RN.Malalcahuello","56.RN.Isla_Mocha","57.MN.Cerro_nielol","58.PN.Conguillio","59.RN.Alto_Bio_Bio","60.RN.China_Muerta","61.RN.Villarrica","62.PN.Huerquehue","63.PN.Villarrica" ,
                                        "64.PN.VILLARICA.SUR","66.RN.Mocho-Choshuenco","67.SN.RIO_CRUCES","68.RN.VALDIVIA","69.PN.Alerce_Costero","70.PN.Puyehue","71.PN.Vicente_Perez_Rosales","72.RN.Llanquihue","73.MN.LahuEn_nadi","74.PN.Alerce_Andino","75.PN.Hornopiren"              
                                        ,"76.MN.Islotes_de_Punihuil","77.PN.Chiloe","78.RN.Futaleufu","79.PN.Corcovado","80.RN.Lago_Palena","81.RN.Lago_Rosselot","82.PN.Queulat","83.RN.Lago_Carlota","84.RN.Lago_Las_Torres","85.PN.Isla_Magdalena","86.PN.Isla_Guamblin","87.RN.Trapananda","88.MN.Cinco_Hermanas","89.RN.Rio_Simpson","90.RN.Coyhaique",
                                        "91.MN.Dos_Lagunas" ,"92.RN.Cerro_Castillo","93.RN.Las_Guaitecas","94.AP.CERRO_HUEMULES" ,"95.RN.LAGO_GENERAL_CARRERA","96.RN.MANIHUALES","97.RN.Lago_Jeinimeni","98.PN.Laguna_San_Rafael","99.RN.Lago_Cochrane",                                   
                                        "100.RN.Katalalixar","101.PN.Bernardo_Ohiggins.1","102.PN.Bernardo_Ohiggins.2","103.PN.Torres_del_Paine","104.MN.Cueva_del_Milodon","105.PN.Pali_Aike","106.RN.Alacalufes","107.MN.Los_Pinguinos","108.RN.Magallanes","109.MN.Laguna_de_los_Cisnes","110.RN.Laguna_Parrillar",
                                        "111.PN.Alberto_de_Agostini","113.PN.Cabo_de_Hornos"))         
                   
                   



# Eliminar unidades con cero visitas en el periodo 1990-2016
#wide.format<-xtabs(visitas.anuales$value~visitas.anuales$Id2+visitas.anuales$year)
#write.table(wide.format, file="visitas_historicasWide.txt")

dat0 <- subset(data,Id2!="100.RN.Katalalixar" & Id2!="109.MN.Laguna_de_los_Cisnes" & Id2!="111.PN.Alberto_de_Agostini" & Id2!="12.PN.Llullaillaco"& Id2!="22.MN.Isla_Cachagua"& Id2!="24.RN.Rio_Blanco"& Id2!="34.PN.Las_Palmas_de_Cocalan" & Id2!="64.PN.VILLARICA.SUR"& Id2!="67.SN.RIO_CRUCES"& Id2!="79.PN.Corcovado"& Id2!="81.RN.Lago_Rosselot"& Id2!="83.RN.Lago_Carlota"&Id2!="85.PN.Isla_Magdalena"&Id2!="86.PN.Isla_Guamblin"&Id2!="88.MN.Cinco_Hermanas"&Id2!="93.RN.Las_Guaitecas"&Id2!="95.RN.LAGO_GENERAL_CARRERA")                                     

dat0$Id2 <- factor(dat0$Id2)     # this is needed to remove   t h e   l e v e l s   o f   the excluded units
length(unique(dat0$Id2))



###### NORTE n=17 unidades

norte <-c("1.PN.Lauca", 
  "3.RN.las_Vicunas",
  "4.MN.Salar_de_Surire",
  "5.PN.Volcan_Isluga",
  "6.RN.Pampa_del_Tamarugal",
  "7.RN.Los_Flamencos",
  "8.MN.La_Portada",
  "9.RN.La_Chimba",
  "11.MN.Paposo_Norte",
  "12.PN.Llullaillaco",
  "13.PN.Pan_de_Azucar",
  "14.PN.Nevado_de_Tres_Cruces", 
  "15.PN.Llanos_de_Challe",
  "17.RN.Pinguino_de_Humboldt.2",
  "18.PN.Bosque_Fray_Jorge",
  "19.MN.Pichasca",
  "20.RN.Las_Chinchillas")


Mnorte<- dat0[dat0$Id2%in%norte,]

sum.norte<-aggregate(value~year,data=Mnorte,sum)
is.data.frame(sum.norte)
names(sum.norte)
head(sum.norte)
sum.norte$macrozona<-"Norte"

library(ggplot2)

options(scipen=10)

pdf(file="Visitantes.MacroZonaNorte.pdf", height=15/2.54, width = 20/2.54)#size in cm
par(bg="white")
Mnorte<-ggplot(sum.norte, aes(year, value)) + geom_line() + geom_point(size=2)+
  xlab("") + ylab("n°de visitantes\n")+scale_x_continuous(breaks=c(1992,1996,2000,2004,2008,2012,
                                                                  2016), labels=c(1992,1996,2000,2004,2008,2012,
                                                                                  2016))
Mnorte<-Mnorte+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text())
Mnorte
dev.off()


#Standarized measures of rate of change in the time series
mod.norte<-lm(sum.norte$value~sum.norte$year)
summary(mod.norte)

Stnd.RateChange<-(coef(mod.norte)[2]/max(predict.lm(mod.norte)))
Stnd.RateChange


###################################
#Variacion anual tasa de visitacion
#############################

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors<-ts(sum.norte$value, start=1990,end=2016, frequency = 1)


#Change in visitors

diff.visitors<-(diff(visitors))

Percent.Change<-(diff.visitors/sum.norte$value[1:26])*100
mean(Percent.Change)

##################################MACROZONA CENTRO

###### NORTE n=17 unidades

CENTRO <-c("21.PN.Rapa_Nui"                
          ,"22.MN.Isla_Cachagua","23.PN.La_Campana","24.RN.Rio_Blanco","25.RN.Lago_Penuelas","26.RN.El_Yali","27.SN.JARDIN_BOTANICO"         
          ,"28.SN.LAGUNA_EL_PERAL","29.PN.Arch_de_Juan_Fernandez","30.RN.Rio_Clarillo","31.MN.El_Morado","32.SN.YERBA_LOCA",              
          "33.RN.Rob_del_Cobre_de_Loncha","34.PN.Las_Palmas_de_Cocalan","35.RN.Rio_de_los_Cipreses","36.RN.Laguna_Torca","37.PN.Radal_Siete_Tazas","39.RN.Altos_de_Lircay",         
          "40.RN.Federico_Albert","41.RN.Los_Ruiles", "42.RN.Los_Bellotos_del_Melado","43.RN.Los_Queules","44.RN.Los_Huemules_de_Niblinto","45.RN.Nonguen","46.RN.nuble","47.PN.Laguna_del_Laja")


MCENTRO<- dat0[dat0$Id2%in%CENTRO,]

sum.CENTRO<-aggregate(value~year,data=MCENTRO,sum)
is.data.frame(sum.CENTRO)
names(sum.CENTRO)
head(sum.CENTRO)
sum.CENTRO$macrozona<-"Centro"

library(ggplot2)

options(scipen=10)

pdf(file="Visitantes.MacroZonaCENTRO.pdf", height=15/2.54, width = 20/2.54)#size in cm
par(bg="white")
p<-ggplot(sum.CENTRO, aes(year, value)) + geom_line() +
  xlab("") + ylab("n°de visitantes\n")
p+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text(angle=90))
dev.off()


#Standarized measures of rate of change in the time series
mod.CENTRO<-lm(sum.CENTRO$value~sum.CENTRO$year)
summary(mod.CENTRO)

Stnd.RateChange<-(coef(mod.CENTRO)[2]/max(predict.lm(mod.CENTRO)))
Stnd.RateChange


###################################
#Variacion anual tasa de visitacion
#############################

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors<-ts(sum.CENTRO$value, start=1990,end=2016, frequency = 1)


#Change in visitors

diff.visitors<-(diff(visitors))

Percent.Change<-(diff.visitors/sum.CENTRO$value[1:26])*100
mean(Percent.Change)



##################################MACROZONA SUR

###### sur n=17 unidades

sur <-c("48.PN.Nahuelbuta","49.RN.Ralco" ,"50.RN.Altos_de_Pemehue","51.RN.Malleco","52.MN.Contulmo",
                                        "53.PN.Tolhuaca","54.RN.Nalcas","55.RN.Malalcahuello","56.RN.Isla_Mocha","57.MN.Cerro_nielol","58.PN.Conguillio","59.RN.Alto_Bio_Bio","60.RN.China_Muerta","61.RN.Villarrica","62.PN.Huerquehue","63.PN.Villarrica" ,
        "64.PN.VILLARICA.SUR","66.RN.Mocho-Choshuenco","67.SN.RIO_CRUCES","68.RN.VALDIVIA","69.PN.Alerce_Costero","70.PN.Puyehue","71.PN.Vicente_Perez_Rosales","72.RN.Llanquihue","73.MN.LahuEn_nadi","74.PN.Alerce_Andino","75.PN.Hornopiren"              
        ,"76.MN.Islotes_de_Punihuil","77.PN.Chiloe","78.RN.Futaleufu","79.PN.Corcovado","80.RN.Lago_Palena")

Msur<- dat0[dat0$Id2%in%sur,]

sum.sur<-aggregate(value~year,data=Msur,sum)
is.data.frame(sum.sur)
names(sum.sur)
head(sum.sur)
sum.sur$macrozona<-"Sur"

library(ggplot2)

options(scipen=10)

pdf(file="Visitantes.MacroZonasur.pdf", height=15/2.54, width = 20/2.54)#size in cm
par(bg="white")
p<-ggplot(sum.sur, aes(year, value)) + geom_line() +
  xlab("") + ylab("n°de visitantes\n")
p+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text(angle=90))
dev.off()


#Standarized measures of rate of change in the time series
mod.sur<-lm(sum.sur$value~sum.sur$year)
summary(mod.sur)

Stnd.RateChange<-(coef(mod.sur)[2]/max(predict.lm(mod.sur)))
Stnd.RateChange


###################################
#Variacion anual tasa de visitacion
#############################

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors<-ts(sum.sur$value, start=1990,end=2016, frequency = 1)


#Change in visitors

diff.visitors<-(diff(visitors))

Percent.Change<-(diff.visitors/sum.sur$value[1:26])*100
mean(Percent.Change)


##################################MACROZONA austral

###### austral 

austral <-c("81.RN.Lago_Rosselot","82.PN.Queulat","83.RN.Lago_Carlota","84.RN.Lago_Las_Torres","85.PN.Isla_Magdalena","86.PN.Isla_Guamblin","87.RN.Trapananda","88.MN.Cinco_Hermanas","89.RN.Rio_Simpson","90.RN.Coyhaique",
            "91.MN.Dos_Lagunas" ,"92.RN.Cerro_Castillo","93.RN.Las_Guaitecas","94.AP.CERRO_HUEMULES" ,"95.RN.LAGO_GENERAL_CARRERA","96.RN.MANIHUALES","97.RN.Lago_Jeinimeni","98.PN.Laguna_San_Rafael","99.RN.Lago_Cochrane",                                   
            "100.RN.Katalalixar","101.PN.Bernardo_Ohiggins.1","102.PN.Bernardo_Ohiggins.2","103.PN.Torres_del_Paine","104.MN.Cueva_del_Milodon","105.PN.Pali_Aike","106.RN.Alacalufes","107.MN.Los_Pinguinos","108.RN.Magallanes","109.MN.Laguna_de_los_Cisnes","110.RN.Laguna_Parrillar",
            "111.PN.Alberto_de_Agostini","113.PN.Cabo_de_Hornos")

Maustral<- dat0[dat0$Id2%in%austral,]

sum.austral<-aggregate(value~year,data=Maustral,sum)
is.data.frame(sum.austral)
names(sum.austral)
head(sum.austral)
sum.austral$macrozona<-"SurAustral"

library(ggplot2)

options(scipen=10)

pdf(file="Visitantes.MacroZonaAustral.pdf", height=15/2.54, width = 20/2.54)#size in cm
par(bg="white")
p<-ggplot(sum.austral, aes(year, value)) + geom_line() +
  xlab("") + ylab("n°de visitantes\n")
p+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text(angle=90))
dev.off()


#Standarized measures of rate of change in the time series
mod.austral<-lm(sum.austral$value~sum.austral$year)
summary(mod.austral)

Stnd.RateChange<-(coef(mod.austral)[2]/max(predict.lm(mod.austral)))
Stnd.RateChange


###################################
#Variacion anual tasa de visitacion
#############################

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors<-ts(sum.austral$value, start=1990,end=2016, frequency = 1)


#Change in visitors

diff.visitors<-(diff(visitors))

Percent.Change<-(diff.visitors/sum.austral$value[1:26])*100
mean(Percent.Change)




#####MERGE DATA SETS


MACROZONAS <- rbind(sum.norte,sum.CENTRO,sum.sur,sum.austral)
MACROZONAS$macrozona<-as.factor(MACROZONAS$macrozona)
levels(MACROZONAS$macrozona)
MACROZONAS$macrozona <- factor(MACROZONAS$macrozona, levels = c("Norte", "Centro", "Sur", "SurAustral"))
levels(MACROZONAS$macrozona)

levels(MACROZONAS$macrozona) <- c("Norte", "Centro", "Sur", "Austral")


library(ggplot2)

options(scipen=10)


pdf(file="Visitantes.Macrozonas.pdf", height=15/2.54, width = 20/2.54)#size in cm
par(bg="white")
M<-ggplot(MACROZONAS, aes(year, value)) + geom_line() + geom_point(size=2)+
  xlab("") + ylab("n°de visitantes\n")+scale_x_continuous(breaks=c(1992,2002,2012), labels=c(1992,2002,2012))
M<-M+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text())
M+facet_wrap(~macrozona, scales = "free_y")
dev.off()


################################################################################
########################################VARIACION GEOGRAFICA y por nacionalidad EN TENDENCIA DE VISITACIONES
################################################################################
######MACROZONA NORTE

visitas.anuales<-read.table("visitas.anuales.txt",header=T)


tail(visitas.anuales)


data<-subset(visitas.anuales,visitas.anuales$variable == "Ext"|visitas.anuales$variable == "CL")

str(data)
head(data)

length(unique(data$Id2))


# To change plot order of facet wrap,
# change the order of varible levels with factor()
data$Id2 <- factor(data$Id2, levels = c("1.PN.Lauca", "3.RN.las_Vicunas", "4.MN.Salar_de_Surire","5.PN.Volcan_Isluga","6.RN.Pampa_del_Tamarugal","7.RN.Los_Flamencos","8.MN.La_Portada","9.RN.La_Chimba","11.MN.Paposo_Norte","12.PN.Llullaillaco","13.PN.Pan_de_Azucar","14.PN.Nevado_de_Tres_Cruces", 
                                        "15.PN.Llanos_de_Challe","17.RN.Pinguino_de_Humboldt.2","18.PN.Bosque_Fray_Jorge","19.MN.Pichasca","20.RN.Las_Chinchillas","21.PN.Rapa_Nui"                
                                        ,"22.MN.Isla_Cachagua","23.PN.La_Campana","24.RN.Rio_Blanco","25.RN.Lago_Penuelas","26.RN.El_Yali","27.SN.JARDIN_BOTANICO"         
                                        ,"28.SN.LAGUNA_EL_PERAL","29.PN.Arch_de_Juan_Fernandez","30.RN.Rio_Clarillo","31.MN.El_Morado","32.SN.YERBA_LOCA",              
                                        "33.RN.Rob_del_Cobre_de_Loncha","34.PN.Las_Palmas_de_Cocalan","35.RN.Rio_de_los_Cipreses","36.RN.Laguna_Torca","37.PN.Radal_Siete_Tazas","39.RN.Altos_de_Lircay",         
                                        "40.RN.Federico_Albert","41.RN.Los_Ruiles", "42.RN.Los_Bellotos_del_Melado","43.RN.Los_Queules","44.RN.Los_Huemules_de_Niblinto","45.RN.Nonguen","46.RN.nuble","47.PN.Laguna_del_Laja"         
                                        ,"48.PN.Nahuelbuta","49.RN.Ralco" ,"50.RN.Altos_de_Pemehue","51.RN.Malleco","52.MN.Contulmo",
                                        "53.PN.Tolhuaca","54.RN.Nalcas","55.RN.Malalcahuello","56.RN.Isla_Mocha","57.MN.Cerro_nielol","58.PN.Conguillio","59.RN.Alto_Bio_Bio","60.RN.China_Muerta","61.RN.Villarrica","62.PN.Huerquehue","63.PN.Villarrica" ,
                                        "64.PN.VILLARICA.SUR","66.RN.Mocho-Choshuenco","67.SN.RIO_CRUCES","68.RN.VALDIVIA","69.PN.Alerce_Costero","70.PN.Puyehue","71.PN.Vicente_Perez_Rosales","72.RN.Llanquihue","73.MN.LahuEn_nadi","74.PN.Alerce_Andino","75.PN.Hornopiren"              
                                        ,"76.MN.Islotes_de_Punihuil","77.PN.Chiloe","78.RN.Futaleufu","79.PN.Corcovado","80.RN.Lago_Palena","81.RN.Lago_Rosselot","82.PN.Queulat","83.RN.Lago_Carlota","84.RN.Lago_Las_Torres","85.PN.Isla_Magdalena","86.PN.Isla_Guamblin","87.RN.Trapananda","88.MN.Cinco_Hermanas","89.RN.Rio_Simpson","90.RN.Coyhaique",
                                        "91.MN.Dos_Lagunas" ,"92.RN.Cerro_Castillo","93.RN.Las_Guaitecas","94.AP.CERRO_HUEMULES" ,"95.RN.LAGO_GENERAL_CARRERA","96.RN.MANIHUALES","97.RN.Lago_Jeinimeni","98.PN.Laguna_San_Rafael","99.RN.Lago_Cochrane",                                   
                                        "100.RN.Katalalixar","101.PN.Bernardo_Ohiggins.1","102.PN.Bernardo_Ohiggins.2","103.PN.Torres_del_Paine","104.MN.Cueva_del_Milodon","105.PN.Pali_Aike","106.RN.Alacalufes","107.MN.Los_Pinguinos","108.RN.Magallanes","109.MN.Laguna_de_los_Cisnes","110.RN.Laguna_Parrillar",
                                        "111.PN.Alberto_de_Agostini","113.PN.Cabo_de_Hornos"))         





# Eliminar unidades con cero visitas en el periodo 1990-2016
#wide.format<-xtabs(visitas.anuales$value~visitas.anuales$Id2+visitas.anuales$year)
#write.table(wide.format, file="visitas_historicasWide.txt")

dat0 <- subset(data,Id2!="100.RN.Katalalixar" & Id2!="109.MN.Laguna_de_los_Cisnes" & Id2!="111.PN.Alberto_de_Agostini" & Id2!="12.PN.Llullaillaco"& Id2!="22.MN.Isla_Cachagua"& Id2!="24.RN.Rio_Blanco"& Id2!="34.PN.Las_Palmas_de_Cocalan" & Id2!="64.PN.VILLARICA.SUR"& Id2!="67.SN.RIO_CRUCES"& Id2!="79.PN.Corcovado"& Id2!="81.RN.Lago_Rosselot"& Id2!="83.RN.Lago_Carlota"&Id2!="85.PN.Isla_Magdalena"&Id2!="86.PN.Isla_Guamblin"&Id2!="88.MN.Cinco_Hermanas"&Id2!="93.RN.Las_Guaitecas"&Id2!="95.RN.LAGO_GENERAL_CARRERA")                                     

dat0$Id2 <- factor(dat0$Id2)     # this is needed to remove   t h e   l e v e l s   o f   the excluded units
length(unique(dat0$Id2))



###### NORTE n=17 unidades

norte <-c("1.PN.Lauca", 
          "3.RN.las_Vicunas",
          "4.MN.Salar_de_Surire",
          "5.PN.Volcan_Isluga",
          "6.RN.Pampa_del_Tamarugal",
          "7.RN.Los_Flamencos",
          "8.MN.La_Portada",
          "9.RN.La_Chimba",
          "11.MN.Paposo_Norte",
          "12.PN.Llullaillaco",
          "13.PN.Pan_de_Azucar",
          "14.PN.Nevado_de_Tres_Cruces", 
          "15.PN.Llanos_de_Challe",
          "17.RN.Pinguino_de_Humboldt.2",
          "18.PN.Bosque_Fray_Jorge",
          "19.MN.Pichasca",
          "20.RN.Las_Chinchillas")


Mnorte<- dat0[dat0$Id2%in%norte,]

sum.norte<-aggregate(value~year+variable+Id2,data=Mnorte,sum)
is.data.frame(sum.norte)
names(sum.norte)
head(sum.norte)
sum.norte$macrozona<-"Norte"


library(ggplot2)

options(scipen=10)


pdf(file="UNIDADES.Norte.Nacionalidad.pdf", height=18/2.54, width = 30/2.54)#size in cm
par(bg="white")
M<-ggplot(sum.norte, aes(year, value, colour=variable, shape=variable, group=variable))  + geom_line() + geom_point(size=2)+
  xlab("") + ylab("n°de visitantes\n")+scale_x_continuous(breaks=c(1992,2002,2012), labels=c(1992,2002,2012))+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text())
M<-M+scale_colour_discrete(name="Nacionalidad",breaks=c("CL", "Ext"),labels=c("Chilenos", "Extranjeros"))+scale_shape_discrete(name="Nacionalidad",breaks=c("CL", "Ext"),labels=c("Chilenos", "Extranjeros"))
M+facet_wrap(~Id2, scales = "free_y")
dev.off()







##########################################
#Chilenos
######################################

dat.cl<-subset(sum.norte,sum.norte$variable == "CL")

#Standarized measures of rate of change
m3<-lm(dat.cl$value~dat.cl$year)
summary(m3)


Stnd.RateChange<-(coef(m3)[2]/max(predict.lm(m3)))
Stnd.RateChange


#mAKE chilean VISITOR VARIABLE A TIME SERIE OBJECT

visitors.cl<-ts(dat.cl$value, start=1990,end=2016, frequency = 1)


#Change in visitors

diff.visitors.cl<-(diff(visitors.cl))

Percent.Change<-(diff.visitors.cl/dat.cl$value[1:26])*100
Percent.Change
mean(Percent.Change)

log.return<-diff(log(visitors.cl))
log.return


##########################################
#Extranjeros
######################################

dat.ext<-subset(sum.norte,sum.norte$variable == "Ext")

#Standarized measures of rate of change
me<-lm(dat.ext$value~dat.ext$year)
summary(me)


Stnd.RateChange<-(coef(me)[2]/max(predict.lm(me)))
Stnd.RateChange


#mAKE foreing VISITOR VARIABLE A TIME SERIE OBJECT

visitors.ext<-ts(dat.ext$value, start=1990,end=2016, frequency = 1)


#Change in visitors

diff.visitors.ext<-(diff(visitors.ext))

Percent.Change<-(diff.visitors.ext/dat.ext$value[1:26])*100
Percent.Change
mean(Percent.Change)



##################################MACROZONA CENTRO


CENTRO <-c("21.PN.Rapa_Nui"                
           ,"22.MN.Isla_Cachagua","23.PN.La_Campana","24.RN.Rio_Blanco","25.RN.Lago_Penuelas","26.RN.El_Yali","27.SN.JARDIN_BOTANICO"         
           ,"28.SN.LAGUNA_EL_PERAL","29.PN.Arch_de_Juan_Fernandez","30.RN.Rio_Clarillo","31.MN.El_Morado","32.SN.YERBA_LOCA",              
           "33.RN.Rob_del_Cobre_de_Loncha","34.PN.Las_Palmas_de_Cocalan","35.RN.Rio_de_los_Cipreses","36.RN.Laguna_Torca","37.PN.Radal_Siete_Tazas","39.RN.Altos_de_Lircay",         
           "40.RN.Federico_Albert","41.RN.Los_Ruiles", "42.RN.Los_Bellotos_del_Melado","43.RN.Los_Queules","44.RN.Los_Huemules_de_Niblinto","45.RN.Nonguen","46.RN.nuble","47.PN.Laguna_del_Laja")


MCENTRO<- dat0[dat0$Id2%in%CENTRO,]

sum.CENTRO<-aggregate(value~year+variable+Id2,data=MCENTRO,sum)
is.data.frame(sum.CENTRO)
names(sum.CENTRO)
head(sum.CENTRO)
sum.CENTRO$macrozona<-"Centro"




library(ggplot2)

options(scipen=10)


pdf(file="UNIDADES.Centro.Nacionalidad.pdf", height=20/2.54, width = 40/2.54)#size in cm
par(bg="white")
M<-ggplot(sum.CENTRO, aes(year, value, colour=variable, shape=variable, group=variable))  + geom_line() + geom_point(size=2)+
  xlab("") + ylab("n°de visitantes\n")+scale_x_continuous(breaks=c(1992,2002,2012), labels=c(1992,2002,2012))+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text())
M<-M+scale_colour_discrete(name="Nacionalidad",breaks=c("CL", "Ext"),labels=c("Chilenos", "Extranjeros"))+scale_shape_discrete(name="Nacionalidad",breaks=c("CL", "Ext"),labels=c("Chilenos", "Extranjeros"))
M+facet_wrap(~Id2, scales = "free_y")
dev.off()






##########################################
#Chilenos
######################################

dat.cl<-subset(sum.CENTRO,sum.CENTRO$variable == "CL")

#Standarized measures of rate of change
m3<-lm(dat.cl$value~dat.cl$year)
summary(m3)


Stnd.RateChange<-(coef(m3)[2]/max(predict.lm(m3)))
Stnd.RateChange


#mAKE chilean VISITOR VARIABLE A TIME SERIE OBJECT

visitors.cl<-ts(dat.cl$value, start=1990,end=2016, frequency = 1)


#Change in visitors

diff.visitors.cl<-(diff(visitors.cl))

Percent.Change<-(diff.visitors.cl/dat.cl$value[1:26])*100
Percent.Change
mean(Percent.Change)


##########################################
#Extranjeros
######################################

dat.ext<-subset(sum.CENTRO,sum.CENTRO$variable == "Ext")

#Standarized measures of rate of change
me<-lm(dat.ext$value~dat.ext$year)
summary(me)


Stnd.RateChange<-(coef(me)[2]/max(predict.lm(me)))
Stnd.RateChange


#mAKE foreing VISITOR VARIABLE A TIME SERIE OBJECT

visitors.ext<-ts(dat.ext$value, start=1990,end=2016, frequency = 1)


#Change in visitors

diff.visitors.ext<-(diff(visitors.ext))

Percent.Change<-(diff.visitors.ext/dat.ext$value[1:26])*100
Percent.Change
mean(Percent.Change)
##################################MACROZONA SUR

###### sur n=17 unidades

sur <-c("48.PN.Nahuelbuta","49.RN.Ralco" ,"50.RN.Altos_de_Pemehue","51.RN.Malleco","52.MN.Contulmo",
        "53.PN.Tolhuaca","54.RN.Nalcas","55.RN.Malalcahuello","56.RN.Isla_Mocha","57.MN.Cerro_nielol","58.PN.Conguillio","59.RN.Alto_Bio_Bio","60.RN.China_Muerta","61.RN.Villarrica","62.PN.Huerquehue","63.PN.Villarrica" ,
        "64.PN.VILLARICA.SUR","66.RN.Mocho-Choshuenco","67.SN.RIO_CRUCES","68.RN.VALDIVIA","69.PN.Alerce_Costero","70.PN.Puyehue","71.PN.Vicente_Perez_Rosales","72.RN.Llanquihue","73.MN.LahuEn_nadi","74.PN.Alerce_Andino","75.PN.Hornopiren"              
        ,"76.MN.Islotes_de_Punihuil","77.PN.Chiloe","78.RN.Futaleufu","79.PN.Corcovado","80.RN.Lago_Palena")

Msur<- dat0[dat0$Id2%in%sur,]

sum.sur<-aggregate(value~year+variable+Id2,data=Msur,sum)
is.data.frame(sum.sur)
names(sum.sur)
head(sum.sur)
sum.sur$macrozona<-"Sur"

library(ggplot2)

options(scipen=10)


pdf(file="UNIDADES.Sur.Nacionalidad.pdf", height=20/2.54, width = 40/2.54)#size in cm
par(bg="white")
M<-ggplot(sum.sur, aes(year, value, colour=variable, shape=variable, group=variable))  + geom_line() + geom_point(size=2)+
  xlab("") + ylab("n°de visitantes\n")+scale_x_continuous(breaks=c(1992,2002,2012), labels=c(1992,2002,2012))+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text())
M<-M+scale_colour_discrete(name="Nacionalidad",breaks=c("CL", "Ext"),labels=c("Chilenos", "Extranjeros"))+scale_shape_discrete(name="Nacionalidad",breaks=c("CL", "Ext"),labels=c("Chilenos", "Extranjeros"))
M+facet_wrap(~Id2, scales = "free_y")
dev.off()


##########################################
#Chilenos
######################################

dat.cl<-subset(sum.sur,sum.sur$variable == "CL")

#Standarized measures of rate of change
m3<-lm(dat.cl$value~dat.cl$year)
summary(m3)


Stnd.RateChange<-(coef(m3)[2]/max(predict.lm(m3)))
Stnd.RateChange


#mAKE chilean VISITOR VARIABLE A TIME SERIE OBJECT

visitors.cl<-ts(dat.cl$value, start=1990,end=2016, frequency = 1)


#Change in visitors

diff.visitors.cl<-(diff(visitors.cl))

Percent.Change<-(diff.visitors.cl/dat.cl$value[1:26])*100
Percent.Change
mean(Percent.Change)


##########################################
#Extranjeros
######################################

dat.ext<-subset(sum.sur,sum.sur$variable == "Ext")

#Standarized measures of rate of change
me<-lm(dat.ext$value~dat.ext$year)
summary(me)


Stnd.RateChange<-(coef(me)[2]/max(predict.lm(me)))
Stnd.RateChange


#mAKE foreing VISITOR VARIABLE A TIME SERIE OBJECT

visitors.ext<-ts(dat.ext$value, start=1990,end=2016, frequency = 1)


#Change in visitors

diff.visitors.ext<-(diff(visitors.ext))

Percent.Change<-(diff.visitors.ext/dat.ext$value[1:26])*100
Percent.Change
mean(Percent.Change)
##################################MACROZONA austral

###### austral 

austral <-c("81.RN.Lago_Rosselot","82.PN.Queulat","83.RN.Lago_Carlota","84.RN.Lago_Las_Torres","85.PN.Isla_Magdalena","86.PN.Isla_Guamblin","87.RN.Trapananda","88.MN.Cinco_Hermanas","89.RN.Rio_Simpson","90.RN.Coyhaique",
            "91.MN.Dos_Lagunas" ,"92.RN.Cerro_Castillo","93.RN.Las_Guaitecas","94.AP.CERRO_HUEMULES" ,"95.RN.LAGO_GENERAL_CARRERA","96.RN.MANIHUALES","97.RN.Lago_Jeinimeni","98.PN.Laguna_San_Rafael","99.RN.Lago_Cochrane",                                   
            "100.RN.Katalalixar","101.PN.Bernardo_Ohiggins.1","102.PN.Bernardo_Ohiggins.2","103.PN.Torres_del_Paine","104.MN.Cueva_del_Milodon","105.PN.Pali_Aike","106.RN.Alacalufes","107.MN.Los_Pinguinos","108.RN.Magallanes","109.MN.Laguna_de_los_Cisnes","110.RN.Laguna_Parrillar",
            "111.PN.Alberto_de_Agostini","113.PN.Cabo_de_Hornos")

Maustral<- dat0[dat0$Id2%in%austral,]

sum.austral<-aggregate(value~year+variable+Id2,data=Maustral,sum)
is.data.frame(sum.austral)
names(sum.austral)
head(sum.austral)
sum.austral$macrozona<-"SurAustral"



library(ggplot2)

options(scipen=10)


pdf(file="UNIDADES.Austral.Nacionalidad.pdf", height=20/2.54, width = 40/2.54)#size in cm
par(bg="white")
M<-ggplot(sum.austral, aes(year, value, colour=variable, shape=variable, group=variable))  + geom_line() + geom_point(size=2)+
  xlab("") + ylab("n°de visitantes\n")+scale_x_continuous(breaks=c(1992,2002,2012), labels=c(1992,2002,2012))+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text())
M<-M+scale_colour_discrete(name="Nacionalidad",breaks=c("CL", "Ext"),labels=c("Chilenos", "Extranjeros"))+scale_shape_discrete(name="Nacionalidad",breaks=c("CL", "Ext"),labels=c("Chilenos", "Extranjeros"))
M+facet_wrap(~Id2, scales = "free_y")
dev.off()



##########################################
#Chilenos
######################################

dat.cl<-subset(sum.austral,sum.austral$variable == "CL")

#Standarized measures of rate of change
m3<-lm(dat.cl$value~dat.cl$year)
summary(m3)


Stnd.RateChange<-(coef(m3)[2]/max(predict.lm(m3)))
Stnd.RateChange


#mAKE chilean VISITOR VARIABLE A TIME SERIE OBJECT

visitors.cl<-ts(dat.cl$value, start=1990,end=2016, frequency = 1)


#Change in visitors

diff.visitors.cl<-(diff(visitors.cl))

Percent.Change<-(diff.visitors.cl/dat.cl$value[1:26])*100
Percent.Change
mean(Percent.Change)


##########################################
#Extranjeros
######################################

dat.ext<-subset(sum.austral,sum.austral$variable == "Ext")

#Standarized measures of rate of change
me<-lm(dat.ext$value~dat.ext$year)
summary(me)


Stnd.RateChange<-(coef(me)[2]/max(predict.lm(me)))
Stnd.RateChange


#mAKE foreing VISITOR VARIABLE A TIME SERIE OBJECT

visitors.ext<-ts(dat.ext$value, start=1990,end=2016, frequency = 1)


#Change in visitors

diff.visitors.ext<-(diff(visitors.ext))

Percent.Change<-(diff.visitors.ext/dat.ext$value[1:26])*100
Percent.Change
mean(Percent.Change)


#####MERGE DATA SETS


MACROZONAS <- rbind(sum.norte,sum.CENTRO,sum.sur,sum.austral)
MACROZONAS$macrozona<-as.factor(MACROZONAS$macrozona)
levels(MACROZONAS$macrozona)
MACROZONAS$macrozona <- factor(MACROZONAS$macrozona, levels = c("Norte", "Centro", "Sur", "SurAustral"))
levels(MACROZONAS$macrozona)

levels(MACROZONAS$macrozona) <- c("Norte", "Centro", "Sur", "Austral")


library(ggplot2)

options(scipen=10)


pdf(file="Visitantes.Macrozonas.Nacionalidad.pdf", height=15/2.54, width = 20/2.54)#size in cm
par(bg="white")
M<-ggplot(MACROZONAS, aes(year, value, colour=variable, shape=variable, group=variable))  + geom_line() + geom_point(size=2)+
  xlab("") + ylab("n°de visitantes\n")+scale_x_continuous(breaks=c(1992,2002,2012), labels=c(1992,2002,2012))+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text())
M<-M+scale_colour_discrete(name="Nacionalidad",breaks=c("CL", "Ext"),labels=c("Chilenos", "Extranjeros"))+scale_shape_discrete(name="Nacionalidad",breaks=c("CL", "Ext"),labels=c("Chilenos", "Extranjeros"))
M+facet_wrap(~macrozona, scales = "free_y")
dev.off()
















########################################################################################################
########################################################################################################
############ VISITANTES POR REGION
########################################################################################################
########################################################################################################

### vISITAS ANUALES TOTALES

visitas.region<-read.table("visitas.anuales.region.txt",header=T)


tail(visitas.region)


data<-subset(visitas.region,visitas.region$variable == "Total")

str(data)
head(data)

length(unique(data$Id2))

# Eliminar unidades con cero visitas en el periodo 1990-2016
#wide.format<-xtabs(visitas.anuales$value~visitas.anuales$Id2+visitas.anuales$year)
#write.table(wide.format, file="visitas_historicasWide.txt")

dat0 <- subset(data,Id2!="100.RN.Katalalixar" & Id2!="109.MN.Laguna_de_los_Cisnes" & Id2!="111.PN.Alberto_de_Agostini" & Id2!="12.PN.Llullaillaco"& Id2!="22.MN.Isla_Cachagua"& Id2!="24.RN.Rio_Blanco"& Id2!="34.PN.Las_Palmas_de_Cocalan" & Id2!="64.PN.VILLARICA.SUR"& Id2!="67.SN.RIO_CRUCES"& Id2!="79.PN.Corcovado"& Id2!="81.RN.Lago_Rosselot"& Id2!="83.RN.Lago_Carlota"&Id2!="85.PN.Isla_Magdalena"&Id2!="86.PN.Isla_Guamblin"&Id2!="88.MN.Cinco_Hermanas"&Id2!="93.RN.Las_Guaitecas"&Id2!="95.RN.LAGO_GENERAL_CARRERA")                                     

dat0$Id2 <- factor(dat0$Id2)     # this is needed to remove   t h e   l e v e l s   o f   the excluded units
length(unique(dat0$Id2))

sum.region<-aggregate(value~region+year,data=dat0,sum)
is.data.frame(sum.region)
names(sum.region)
head(sum.region)

levels(sum.region$region)
sum.region$region <- factor(sum.region$region, levels = c("XV","I","II","III","IV", "IPA","RM","V","VI","VII","VIII","IX","XIV","X","XI","XII"))
levels(sum.region$region)
levels(sum.region$region) <- c("Arica y Parinacota","Tarapacá","Antofagasta","Atacama","Coquimbo", "Isla de Pascua","Metropolitana","Valparaiso","O´Higgins","Maule","Bio-Bio","La Araucanía","Los Ríos","Los Lagos","Aysén","Magallanes")
levels(sum.region$region)


library(ggplot2)

options(scipen=10)

pdf(file="Visitantes.regiones.pdf", height=14/2.54, width = 28/2.54)#size in cm
par(bg="white")
M<-ggplot(sum.region, aes(year, value)) + geom_line() + geom_point(size=2)+
  xlab("") + ylab("n°de visitantes\n")+scale_x_continuous(breaks=c(1992,2002,2012), labels=c(1992,2002,2012))
M<-M+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text())
M+facet_wrap(~region, scales = "free_y")
dev.off()


#########################################################################################
######TASAS DE CRECIMIENTO POR REGIÓN 
#########################################################################################

head(sum.region)

Tarapaca<-subset(sum.region, sum.region$region=="Tarapacá")

#mAKE foreing VISITOR VARIABLE A TIME SERIE OBJECT

visitors.Tarapaca<-ts(Tarapaca$value, start=1990,end=2016, frequency = 1)
plot(visitors.Tarapaca)

diff.visitors.Tarapaca<-(diff(visitors.Tarapaca))
diff.visitors.Tarapaca
Percent.Change.tarapaca<-(diff.visitors.Tarapaca/Tarapaca$value[1:26])
Percent.Change.tarapaca
mean(Percent.Change.tarapaca)


#Standarized measures of rate of change
me<-lm(Tarapaca$value~Tarapaca$year)
summary(me)


Stnd.RateChange<-(coef(me)[2]/max(predict.lm(me)))
Stnd.RateChange




#########################################################################################
#########################################################################################
######TASAS DE CRECIMIENTO POR REGIÓN LOOP
#########################################################################################
#########################################################################################

### vISITAS ANUALES TOTALES

visitas.region<-read.table("visitas.anuales.region.txt",header=T)


tail(visitas.region)


data<-subset(visitas.region,visitas.region$variable == "Total")

str(data)
head(data)

length(unique(data$Id2))

# Eliminar unidades con cero visitas en el periodo 1990-2016
#wide.format<-xtabs(visitas.anuales$value~visitas.anuales$Id2+visitas.anuales$year)
#write.table(wide.format, file="visitas_historicasWide.txt")

dat0 <- subset(data,Id2!="100.RN.Katalalixar" & Id2!="109.MN.Laguna_de_los_Cisnes" & Id2!="111.PN.Alberto_de_Agostini" & Id2!="12.PN.Llullaillaco"& Id2!="22.MN.Isla_Cachagua"& Id2!="24.RN.Rio_Blanco"& Id2!="34.PN.Las_Palmas_de_Cocalan" & Id2!="64.PN.VILLARICA.SUR"& Id2!="67.SN.RIO_CRUCES"& Id2!="79.PN.Corcovado"& Id2!="81.RN.Lago_Rosselot"& Id2!="83.RN.Lago_Carlota"&Id2!="85.PN.Isla_Magdalena"&Id2!="86.PN.Isla_Guamblin"&Id2!="88.MN.Cinco_Hermanas"&Id2!="93.RN.Las_Guaitecas"&Id2!="95.RN.LAGO_GENERAL_CARRERA")                                     

dat0$Id2 <- factor(dat0$Id2)     # this is needed to remove   t h e   l e v e l s   o f   the excluded units
length(unique(dat0$Id2))

sum.region<-aggregate(value~region+year,data=dat0,sum)
is.data.frame(sum.region)
names(sum.region)
head(sum.region)



levels(sum.region$region)
sum.region$region <- factor(sum.region$region, levels = c("XV","I","II","III","IV", "IPA","RM","V","VI","VII","VIII","IX","XIV","X","XI","XII"))
levels(sum.region$region)
levels(sum.region$region) <- c("Arica y Parinacota","Tarapacá","Antofagasta","Atacama","Coquimbo", "Isla de Pascua","Metropolitana","Valparaiso","O´Higgins","Maule","Bio-Bio","La Araucanía","Los Ríos","Los Lagos","Aysén","Magallanes")
levels(sum.region$region)



library(xts)

dl<-split(sum.region, sum.region$region)    # we create a list with the data of each region
ml <- lapply(dl,function(x)ts(x$value, start=1990,end=2016, frequency = 1))     # we create a list with time series for each region


####percent.Change.list<-lapply(ml,function(x)((diff(x)/index(x)[1:26])))     # we create a community matrix for each region

library(foreach)

changes.year<-foreach(i=ml,b=dl) %do% mean((diff(i)/b$value[1:26]))
names(changes.year)<-names(ml)

tasa.prom.region<-do.call(rbind, lapply(changes.year, data.frame, stringsAsFactors=FALSE))
str(tasa.prom.region)

tasa.prom.region$region<-row.names(tasa.prom.region)

colnames(tasa.prom.region)<-c("tasa")
names(tasa.prom.region)<-c("tasa","region")
tasa.prom.region$region<-as.factor(tasa.prom.region$region)
tasa.prom.region
levels(tasa.prom.region$region)
tasa.prom.region$region <- factor(tasa.prom.region$region, levels = c("Magallanes", "Aysén", "Los Lagos","Los Ríos","La Araucanía", "Bio-Bio","Maule","O´Higgins","Valparaiso","Metropolitana",
                                                                      "Isla de Pascua","Coquimbo","Atacama","Antofagasta","Tarapacá","Arica y Parinacota"))





write.table(tasa.prom.region,file="tasa.prom.region.txt")

library(ggplot2)

#SIN LINEAS GUIAS
#ggplot(tasa.prom.region, aes(x=tasa*100, y=region))+
 # geom_point(size=3)+
  #theme_bw()+
  #xlab("tasa crecimiento promedio (%)") + ylab("región")
#theme(panel.grid.major.x=element_blank(),
      #panel.grid.minor.x=element_blank(),
      #panel.grid.major.y=element_line(colour="grey60",linetype = "dashed")
      #)
  

pdf(file="Tasa.Prom.Reg.pdf", height=15/2.54, width = 15/2.54)#size in cm
par(bg="white")
tasa<-ggplot(tasa.prom.region, aes(x=tasa*100, y=region))+
  geom_segment(aes(yend=region),xend=0,colour="grey50")+
  geom_point(size=3)+
  theme_bw()+xlab("tasa crecimiento promedio (%)") + ylab("")
tasa+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text(),
  panel.grid.major.y=element_blank())
tasa
dev.off()


####TASA DE CRECIMIENTO ESTANDARIZADA POR REGION


dl<-split(sum.region, sum.region$region)    # we create a list with the data of each region
ml <- lapply(dl,function(x)ts(x$value, start=1990,end=2016, frequency = 1))     # we create a list with time series for each region


####percent.Change.list<-lapply(ml,function(x)((diff(x)/index(x)[1:26])))     # we create a community matrix for each region

library(foreach)

me<-foreach(i=dl) %do% lm(i$value~i$year)

res<-foreach(b=me) %do% (coef(b)[2]/max(predict.lm(b)))

names(res)<-names(dl)

length(res)
res


tasa.std.region<-do.call(rbind, lapply(res, data.frame, stringsAsFactors=FALSE))
str(tasa.std.region)

tasa.std.region$region<-row.names(tasa.std.region)

colnames(tasa.std.region)<-c("tasa.std")
names(tasa.std.region)<-c("tasa.std","region")
tasa.std.region$region<-as.factor(tasa.std.region$region)
tasa.std.region
levels(tasa.std.region$region)
tasa.std.region$region <- factor(tasa.std.region$region, levels = c("Magallanes", "Aysén", "Los Lagos","Los Ríos","La Araucanía", "Bio-Bio","Maule","O´Higgins","Valparaiso","Metropolitana",
                                                                      "Isla de Pascua","Coquimbo","Atacama","Antofagasta","Tarapacá","Arica y Parinacota"))
library(ggplot2)

pdf(file="Tasa.Std.Reg.pdf", height=15/2.54, width = 15/2.54)#size in cm
par(bg="white")
tasa<-ggplot(tasa.std.region, aes(x=tasa.std , y=region))+
  geom_segment(aes(yend=region),xend=0,colour="grey50")+
  geom_point(size=3)+
  theme_bw()+xlab("tasa crecimiento estandarizada") + ylab("")
tasa+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text(),
           panel.grid.major.y=element_blank())
dev.off()


write.table(res,file="tasa.std.region.txt")


###########################################################################
### vISITAS ANUALES NACIONALIDAD
###########################################################################

visitas.region<-read.table("visitas.anuales.region.txt",header=T)


tail(visitas.region)
levels(visitas.region$variable)

data.cl<-subset(visitas.region,visitas.region$variable == "CL")

str(data.cl)
head(data.cl)

length(unique(data.cl$Id2))

# Eliminar unidades con cero visitas en el periodo 1990-2016
#wide.format<-xtabs(visitas.anuales$value~visitas.anuales$Id2+visitas.anuales$year)
#write.table(wide.format, file="visitas_historicasWide.txt")

dat0.cl <- subset(data.cl,Id2!="100.RN.Katalalixar" & Id2!="109.MN.Laguna_de_los_Cisnes" & Id2!="111.PN.Alberto_de_Agostini" & Id2!="12.PN.Llullaillaco"& Id2!="22.MN.Isla_Cachagua"& Id2!="24.RN.Rio_Blanco"& Id2!="34.PN.Las_Palmas_de_Cocalan" & Id2!="64.PN.VILLARICA.SUR"& Id2!="67.SN.RIO_CRUCES"& Id2!="79.PN.Corcovado"& Id2!="81.RN.Lago_Rosselot"& Id2!="83.RN.Lago_Carlota"&Id2!="85.PN.Isla_Magdalena"&Id2!="86.PN.Isla_Guamblin"&Id2!="88.MN.Cinco_Hermanas"&Id2!="93.RN.Las_Guaitecas"&Id2!="95.RN.LAGO_GENERAL_CARRERA")                                     

dat0.cl$Id2 <- factor(dat0.cl$Id2)     # this is needed to remove   t h e   l e v e l s   o f   the excluded units
length(unique(dat0.cl$Id2))

sum.region.cl<-aggregate(value~region+year,data=dat0.cl,sum)
is.data.frame(sum.region.cl)
names(sum.region.cl)
head(sum.region.cl)



levels(sum.region.cl$region)
sum.region.cl$region <- factor(sum.region.cl$region, levels = c("XV","I","II","III","IV", "IPA","RM","V","VI","VII","VIII","IX","XIV","X","XI","XII"))
levels(sum.region.cl$region)
levels(sum.region.cl$region) <- c("Arica y Parinacota","Tarapacá","Antofagasta","Atacama","Coquimbo", "Isla de Pascua","Metropolitana","Valparaiso","O´Higgins","Maule","Bio-Bio","La Araucanía","Los Ríos","Los Lagos","Aysén","Magallanes")
levels(sum.region.cl$region)

library(xts)

dl<-split(sum.region.cl, sum.region.cl$region)    # we create a list with the data of each region
ml <- lapply(dl,function(x)ts(x$value, start=1990,end=2016, frequency = 1))     # we create a list with time series for each region


####percent.Change.list<-lapply(ml,function(x)((diff(x)/index(x)[1:26])))     # we create a community matrix for each region

library(foreach)

changes.year.cl<-foreach(i=ml,b=dl) %do% mean((diff(i)/b$value[1:26]))
names(changes.year.cl)<-names(ml)

tasa.prom.region.cl<-do.call(rbind, lapply(changes.year.cl, data.frame, stringsAsFactors=FALSE))
str(tasa.prom.region.cl)

tasa.prom.region.cl$region<-row.names(tasa.prom.region.cl)

colnames(tasa.prom.region.cl)<-c("tasa")
names(tasa.prom.region.cl)<-c("tasa","region")
tasa.prom.region.cl$region<-as.factor(tasa.prom.region.cl$region)
tasa.prom.region.cl
levels(tasa.prom.region.cl$region)
tasa.prom.region.cl$region <- factor(tasa.prom.region.cl$region, levels = c("Magallanes", "Aysén", "Los Lagos","Los Ríos","La Araucanía", "Bio-Bio","Maule","O´Higgins","Valparaiso","Metropolitana",
                                                                      "Isla de Pascua","Coquimbo","Atacama","Antofagasta","Tarapacá","Arica y Parinacota"))


#######EXTRANJEROS


data.EXT<-subset(visitas.region,visitas.region$variable == "Ext")

str(data.EXT)
head(data.EXT)

length(unique(data.EXT$Id2))

# Eliminar unidades con cero visitas en el periodo 1990-2016
#wide.format<-xtabs(visitas.anuales$value~visitas.anuales$Id2+visitas.anuales$year)
#write.table(wide.format, file="visitas_historicasWide.txt")

dat0.EXT <- subset(data.EXT,Id2!="100.RN.Katalalixar" & Id2!="109.MN.Laguna_de_los_Cisnes" & Id2!="111.PN.Alberto_de_Agostini" & Id2!="12.PN.Llullaillaco"& Id2!="22.MN.Isla_Cachagua"& Id2!="24.RN.Rio_Blanco"& Id2!="34.PN.Las_Palmas_de_Cocalan" & Id2!="64.PN.VILLARICA.SUR"& Id2!="67.SN.RIO_CRUCES"& Id2!="79.PN.Corcovado"& Id2!="81.RN.Lago_Rosselot"& Id2!="83.RN.Lago_Carlota"&Id2!="85.PN.Isla_Magdalena"&Id2!="86.PN.Isla_Guamblin"&Id2!="88.MN.Cinco_Hermanas"&Id2!="93.RN.Las_Guaitecas"&Id2!="95.RN.LAGO_GENERAL_CARRERA")                                     

dat0.EXT$Id2 <- factor(dat0.EXT$Id2)     # this is needed to remove   t h e   l e v e l s   o f   the excluded units
length(unique(dat0.EXT$Id2))

sum.region.EXT<-aggregate(value~region+year,data=dat0.EXT,sum)
is.data.frame(sum.region.EXT)
names(sum.region.EXT)
head(sum.region.EXT)



levels(sum.region.EXT$region)
sum.region.EXT$region <- factor(sum.region.EXT$region, levels = c("XV","I","II","III","IV", "IPA","RM","V","VI","VII","VIII","IX","XIV","X","XI","XII"))
levels(sum.region.EXT$region)
levels(sum.region.EXT$region) <- c("Arica y Parinacota","Tarapacá","Antofagasta","Atacama","Coquimbo", "Isla de Pascua","Metropolitana","Valparaiso","O´Higgins","Maule","Bio-Bio","La Araucanía","Los Ríos","Los Lagos","Aysén","Magallanes")
levels(sum.region.EXT$region)

library(xts)

dl<-split(sum.region.EXT, sum.region.EXT$region)    # we create a list with the data of each region
ml <- lapply(dl,function(x)ts(x$value, start=1990,end=2016, frequency = 1))     # we create a list with time series for each region


####percent.Change.list<-lapply(ml,function(x)((diff(x)/index(x)[1:26])))     # we create a community matrix for each region

library(foreach)

changes.year.EXT<-foreach(i=ml,b=dl) %do% mean((diff(i)/b$value[1:26]))
names(changes.year.EXT)<-names(ml)

tasa.prom.region.EXT<-do.call(rbind, lapply(changes.year.EXT, data.frame, stringsAsFactors=FALSE))
str(tasa.prom.region.EXT)

tasa.prom.region.EXT$region<-row.names(tasa.prom.region.EXT)

colnames(tasa.prom.region.EXT)<-c("tasa")
names(tasa.prom.region.EXT)<-c("tasa","region")
tasa.prom.region.EXT$region<-as.factor(tasa.prom.region.EXT$region)
tasa.prom.region.EXT
levels(tasa.prom.region.EXT$region)
tasa.prom.region.EXT$region <- factor(tasa.prom.region.EXT$region, levels = c("Magallanes", "Aysén", "Los Lagos","Los Ríos","La Araucanía", "Bio-Bio","Maule","O´Higgins","Valparaiso","Metropolitana",
                                                                            "Isla de Pascua","Coquimbo","Atacama","Antofagasta","Tarapacá","Arica y Parinacota"))

#merge data frames per nacionality

tasa.prom.region.cl$nacionalidad<- "chilenos"
tasa.prom.region.EXT$nacionalidad<-"extranjeros"


tasa.prom.region.nac<-rbind.data.frame(tasa.prom.region.cl,tasa.prom.region.EXT)

tasa.prom.region.nac


pdf(file="Tasa.Prom.nacionalidad.v1.pdf", height=15/2.54, width = 15/2.54)#size in cm
par(bg="white")
t<-ggplot(tasa.prom.region.nac, aes(x=tasa*100, y=region))+
  geom_segment(aes(yend=region),xend=0,colour="grey50")+
  geom_point(size=3,aes(colour=nacionalidad))+
  scale_color_brewer(palette = "Set1",limits=c("chilenos","extranjeros"))+
  theme_bw()+xlab("tasa crecimiento promedio (%)") + ylab("")
tasa<-t+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text(),
           panel.grid.major.y=element_blank(), legend.position = c(0.98,0.55),legend.justification = c(1,0.5))
tasa
dev.off()





# Grouped
pdf(file="Tasa.Prom.nacionalidad.v2.pdf", height=15/2.54, width = 15/2.54)#size in cm
par(bg="white")
t<-ggplot(tasa.prom.region.nac, aes(fill=nacionalidad, y=tasa*100, x=region)) + 
  geom_bar(position="dodge", stat="identity") +xlab("tasa crecimiento promedio (%)") + ylab("región")
t1<-t+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text(),
       panel.grid.major.y=element_blank(), legend.position = c(1,0.55),legend.justification = c(1,0.5))
t2<-t1+coord_flip()
t2
dev.off()




########################### por region y nacionalidad


### vISITAS ANUALES TOTALES

visitas.region<-read.table("visitas.anuales.region.txt",header=T)


tail(visitas.region)


data<-subset(visitas.region,visitas.region$variable == "Ext"|visitas.anuales$variable == "CL")

str(data)
head(data)

length(unique(data$Id2))

# Eliminar unidades con cero visitas en el periodo 1990-2016
#wide.format<-xtabs(visitas.anuales$value~visitas.anuales$Id2+visitas.anuales$year)
#write.table(wide.format, file="visitas_historicasWide.txt")

dat0 <- subset(data,Id2!="100.RN.Katalalixar" & Id2!="109.MN.Laguna_de_los_Cisnes" & Id2!="111.PN.Alberto_de_Agostini" & Id2!="12.PN.Llullaillaco"& Id2!="22.MN.Isla_Cachagua"& Id2!="24.RN.Rio_Blanco"& Id2!="34.PN.Las_Palmas_de_Cocalan" & Id2!="64.PN.VILLARICA.SUR"& Id2!="67.SN.RIO_CRUCES"& Id2!="79.PN.Corcovado"& Id2!="81.RN.Lago_Rosselot"& Id2!="83.RN.Lago_Carlota"&Id2!="85.PN.Isla_Magdalena"&Id2!="86.PN.Isla_Guamblin"&Id2!="88.MN.Cinco_Hermanas"&Id2!="93.RN.Las_Guaitecas"&Id2!="95.RN.LAGO_GENERAL_CARRERA")                                     

dat0$Id2 <- factor(dat0$Id2)     # this is needed to remove   t h e   l e v e l s   o f   the excluded units
length(unique(dat0$Id2))

sum.reg<-aggregate(value~region+variable+year,data=dat0,sum)
is.data.frame(sum.reg)
names(sum.reg)
head(sum.reg)



levels(sum.reg$region)
sum.region$reg <- factor(sum.region$region, levels = c("XV","I","II","III","IV", "IPA","RM","V","VI","VII","VIII","IX","XIV","X","XI","XII"))
levels(sum.reg$region)
levels(sum.reg$region) <- c("Arica y Parinacota","Tarapacá","Antofagasta","Atacama","Coquimbo", "Isla de Pascua","Metropolitana","Valparaiso","O´Higgins","Maule","Bio-Bio","La Araucanía","Los Ríos","Los Lagos","Aysén","Magallanes")
levels(sum.reg$region)


library(ggplot2)

options(scipen=10)

pdf(file="Visitantes.regiones.nacionalidad.pdf", height=16/2.54, width = 30/2.54)#size in cm
par(bg="white")
M<-ggplot(sum.reg, aes(year, value, colour=variable, shape=variable, group=variable))  + geom_line() + geom_point(size=2)+
  xlab("") + ylab("n°de visitantes\n")+scale_x_continuous(breaks=c(1992,2002,2012), labels=c(1992,2002,2012))+theme(text = element_text(size=16),axis.text=element_text(size=16),axis.text.y = element_text())
M<-M+scale_colour_discrete(name="Nacionalidad",breaks=c("CL", "Ext"),labels=c("Chilenos", "Extranjeros"))+scale_shape_discrete(name="Nacionalidad",breaks=c("CL", "Ext"),labels=c("Chilenos", "Extranjeros"))
M+facet_wrap(~region, scales = "free_y")

dev.off()





##############################################
##############################################TO BE COMPLETED!!!
############################################## Crecimientos por unidad

### vISITAS ANUALES TOTALES

visitas.region<-read.table("visitas.anuales.region.txt",header=T)


tail(visitas.region)


visitas.region$Id2 <- factor(visitas.region$Id2, levels = c("1.PN.Lauca", "3.RN.las_Vicunas", "4.MN.Salar_de_Surire","5.PN.Volcan_Isluga","6.RN.Pampa_del_Tamarugal","7.RN.Los_Flamencos","8.MN.La_Portada","9.RN.La_Chimba","11.MN.Paposo_Norte","12.PN.Llullaillaco","13.PN.Pan_de_Azucar","14.PN.Nevado_de_Tres_Cruces", 
                                        "15.PN.Llanos_de_Challe","17.RN.Pinguino_de_Humboldt.2","18.PN.Bosque_Fray_Jorge","19.MN.Pichasca","20.RN.Las_Chinchillas","21.PN.Rapa_Nui"                
                                        ,"22.MN.Isla_Cachagua","23.PN.La_Campana","24.RN.Rio_Blanco","25.RN.Lago_Penuelas","26.RN.El_Yali","27.SN.JARDIN_BOTANICO"         
                                        ,"28.SN.LAGUNA_EL_PERAL","29.PN.Arch_de_Juan_Fernandez","30.RN.Rio_Clarillo","31.MN.El_Morado","32.SN.YERBA_LOCA",              
                                        "33.RN.Rob_del_Cobre_de_Loncha","34.PN.Las_Palmas_de_Cocalan","35.RN.Rio_de_los_Cipreses","36.RN.Laguna_Torca","37.PN.Radal_Siete_Tazas","39.RN.Altos_de_Lircay",         
                                        "40.RN.Federico_Albert","41.RN.Los_Ruiles", "42.RN.Los_Bellotos_del_Melado","43.RN.Los_Queules","44.RN.Los_Huemules_de_Niblinto","45.RN.Nonguen","46.RN.nuble","47.PN.Laguna_del_Laja"         
                                        ,"48.PN.Nahuelbuta","49.RN.Ralco" ,"50.RN.Altos_de_Pemehue","51.RN.Malleco","52.MN.Contulmo",
                                        "53.PN.Tolhuaca","54.RN.Nalcas","55.RN.Malalcahuello","56.RN.Isla_Mocha","57.MN.Cerro_nielol","58.PN.Conguillio","59.RN.Alto_Bio_Bio","60.RN.China_Muerta","61.RN.Villarrica","62.PN.Huerquehue","63.PN.Villarrica" ,
                                        "64.PN.VILLARICA.SUR","66.RN.Mocho-Choshuenco","67.SN.RIO_CRUCES","68.RN.VALDIVIA","69.PN.Alerce_Costero","70.PN.Puyehue","71.PN.Vicente_Perez_Rosales","72.RN.Llanquihue","73.MN.LahuEn_nadi","74.PN.Alerce_Andino","75.PN.Hornopiren"              
                                        ,"76.MN.Islotes_de_Punihuil","77.PN.Chiloe","78.RN.Futaleufu","79.PN.Corcovado","80.RN.Lago_Palena","81.RN.Lago_Rosselot","82.PN.Queulat","83.RN.Lago_Carlota","84.RN.Lago_Las_Torres","85.PN.Isla_Magdalena","86.PN.Isla_Guamblin","87.RN.Trapananda","88.MN.Cinco_Hermanas","89.RN.Rio_Simpson","90.RN.Coyhaique",
                                        "91.MN.Dos_Lagunas" ,"92.RN.Cerro_Castillo","93.RN.Las_Guaitecas","94.AP.CERRO_HUEMULES" ,"95.RN.LAGO_GENERAL_CARRERA","96.RN.MANIHUALES","97.RN.Lago_Jeinimeni","98.PN.Laguna_San_Rafael","99.RN.Lago_Cochrane",                                   
                                        "100.RN.Katalalixar","101.PN.Bernardo_Ohiggins.1","102.PN.Bernardo_Ohiggins.2","103.PN.Torres_del_Paine","104.MN.Cueva_del_Milodon","105.PN.Pali_Aike","106.RN.Alacalufes","107.MN.Los_Pinguinos","108.RN.Magallanes","109.MN.Laguna_de_los_Cisnes","110.RN.Laguna_Parrillar",
                                        "111.PN.Alberto_de_Agostini","113.PN.Cabo_de_Hornos"))         





data<-subset(visitas.region,visitas.region$variable == "Total")

str(data)
head(data)

length(unique(data$Id2))

# Eliminar unidades con cero visitas en el periodo 1990-2016
#wide.format<-xtabs(visitas.anuales$value~visitas.anuales$Id2+visitas.anuales$year)
#write.table(wide.format, file="visitas_historicasWide.txt")

dat0 <- subset(data,Id2!="100.RN.Katalalixar" & Id2!="109.MN.Laguna_de_los_Cisnes" & Id2!="111.PN.Alberto_de_Agostini" & Id2!="12.PN.Llullaillaco"& Id2!="22.MN.Isla_Cachagua"& Id2!="24.RN.Rio_Blanco"& Id2!="34.PN.Las_Palmas_de_Cocalan" & Id2!="64.PN.VILLARICA.SUR"& Id2!="67.SN.RIO_CRUCES"& Id2!="79.PN.Corcovado"& Id2!="81.RN.Lago_Rosselot"& Id2!="83.RN.Lago_Carlota"&Id2!="85.PN.Isla_Magdalena"&Id2!="86.PN.Isla_Guamblin"&Id2!="88.MN.Cinco_Hermanas"&Id2!="93.RN.Las_Guaitecas"&Id2!="95.RN.LAGO_GENERAL_CARRERA")                                     

dat0$Id2 <- factor(dat0$Id2)     # this is needed to remove   t h e   l e v e l s   o f   the excluded units
length(unique(dat0$Id2))

sum.unidad<-aggregate(value~Id2+region+year,data=dat0,sum)
is.data.frame(sum.unidad)
names(sum.unidad)
head(sum.unidad)



levels(sum.unidad$region)
sum.unidad$region <- factor(sum.unidad$region, levels = c("XV","I","II","III","IV", "IPA","RM","V","VI","VII","VIII","IX","XIV","X","XI","XII"))
levels(sum.unidad$region)
levels(sum.unidad$region) <- c("Arica y Parinacota","Tarapacá","Antofagasta","Atacama","Coquimbo", "Isla de Pascua","Metropolitana","Valparaiso","O´Higgins","Maule","Bio-Bio","La Araucanía","Los Ríos","Los Lagos","Aysén","Magallanes")
levels(sum.unidad$region)

head(sum.unidad)

library(xts)

dl<-split(sum.unidad, sum.unidad$Id2)    # we create a list with the data of each region
ml <- lapply(dl,function(x)ts(x$value, start=1990,end=2016, frequency = 1))     # we create a list with time series for each region


library(foreach)

changes.year<-foreach(i=ml,b=dl) %do% mean((diff(i)/b$value[1:26]))
names(changes.year)<-names(ml)

tasa.prom.unidad<-do.call(rbind, lapply(changes.year, data.frame, stringsAsFactors=FALSE))
str(tasa.prom.unidad)

tasa.prom.unidad$unidad<-row.names(tasa.prom.unidad)

colnames(tasa.prom.unidad)<-c("tasa")
names(tasa.prom.unidad)<-c("tasa","unidad")
tasa.prom.unidad$unidad<-as.factor(tasa.prom.unidad$unidad)
tasa.prom.unidad
levels(tasa.prom.unidad$unidad)
#tasa.prom.unidad$unidad <- factor(tasa.prom.unidad$unidad, levels = c(#order of units


####TASA DE CRECIMIENTO ESTANDARIZADA POR REGION


dl<-split(sum.unidad, sum.unidad$Id2)    # we create a list with the data of each region

####percent.Change.list<-lapply(ml,function(x)((diff(x)/index(x)[1:26])))     # we create a community matrix for each region

library(foreach)

me<-foreach(i=dl) %do% lm(i$value~i$year)

res<-foreach(b=me) %do% (coef(b)[2]/max(predict.lm(b)))

names(res)<-names(dl)

length(res)
res


tasa.std.unidad<-do.call(rbind, lapply(res, data.frame, stringsAsFactors=FALSE))
str(tasa.std.unidad)

tasa.std.unidad$unidad<-row.names(tasa.std.unidad)

colnames(tasa.std.unidad)<-c("tasa.std")
names(tasa.std.unidad)<-c("tasa.std","unidad")
tasa.std.unidad$unidad<-as.factor(tasa.std.unidad$unidad)
head(tasa.std.unidad)


write.table(res,file="tasa.std.region.txt")







########## Library xts: NOT USED


#library(xts)

#extract the total visitor value per year across all units of the SNASPE
#data<-sum.anual$value

# Create dates as a Date class object starting from 1990-12-01...NOTED THAT THIS IS AN ARBITRARY DATE AS THE FORMAT NEED A MONTH A DAY IN THE STRUCTURE OF DATE
#dates <- seq(as.Date("1990-12-1"), length = 27, by = "years")

# Use xts() to create visitors
#visitors <- xts(x = data, order.by = dates)




######## BASICS MODEL TO FORECAST

library(forecast)

m1 = meanf(visitors,h=20,level=c(90,95),fan=FALSE,lambda=NULL)
plot(m1) 

m2 = naive(visitors,h=20,level=c(90,95),fan=FALSE,lambda=NULL) 
plot(m2)

m3 = rwf(visitors,h=20,drift=T,level=c(90,95),fan=FALSE,lambda=NULL) 
plot(m3) 

#Measuring accuracy:


accuracy(m1)
accuracy(m2)
accuracy(m3)

#A typical time-series analysis involves below steps:

#1.-Check for identifying under lying patterns - Stationary & non-stationary, seasonality, trend. 
#2.-After the patterns have been identified, if needed apply Transformations to the data - based on Seasonality/trends appeared in the data.
#3.-Apply forecast() the future values using Proper ARIMA model obtained by auto.arima() methods.

#A stationary time series is one whose properties do not depend on the time at which the series is observed. 
#Time series with trends, or with seasonality, are not stationary.


#ADF: The null-hypothesis for an ADF test is that the data are non-stationary. So large p-values are indicative of non-stationarity, and small p-values suggest stationarity. 
#Using the usual 5% threshold, differencing is required if the p-value is greater than 0.05. 


library(tseries)
adf = adf.test(visitors)
adf

#KPSS: Another popular unit root test is the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test. This reverses the hypotheses, so the null-hypothesis is that the data are stationary. 
#In this case, small p-values (e.g., less than 0.05) suggest that differencing is required. 

kpss = kpss.test(visitors)
kpss

#Differencing:
#Based on the unit test results we identify whether the data is stationary or not. If the data is stationary then we choose optimal ARIMA models and forecasts the future intervals. 
#If the data is non- stationary, then we use Differencing - computing the differences between consecutive observations. 
#Use ndiffs(),diff() functions to find the number of times differencing needed for the data &  to difference the data respectively.


ndiffs(visitors)

diff_data<-diff(visitors)

#Now retest for stationarity by applying acf()/kpss() functions if the plots shows us the Stationarity then Go ahead by applying ARIMA Models.


adf2 = adf.test(diff_data)
adf2


#Identify Seasonality/Trend:
#The seasonality in the data can be obtained by the stl()when plotted

Stl = stl(visitors,s.window="periodic")






##################MORE SOFISTICATED MODELS

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.


library(forecast)
library(xts)

m_ets = ets(visitors)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)
title(main = "Exponential State Smoothing")

pdf(file="ETS.1990-2036.pdf")
par(bg="white")
plot(f_ets, xlab='',ylab="Visitantes")
dev.off()


#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


pdf(file="ARIMA.1990-2036.pdf")
par(bg="white")
plot(f_aa, xlab='',ylab="Visitantes")
dev.off()

#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)

pdf(file="TBATS.1990-2036.pdf")
par(bg="white")
plot(f_tbats, xlab='',ylab="Visitantes")
dev.off()

#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="grey",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors)[length(visitors)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=f_aa$mean,
                         visitors_lower=f_aa$lower[,2],
                        visitors_upper=f_aa$upper[,2],
                         date=last_date + seq(1, 20, by=1))

#Finally, we split the date column into separate columns for year and month.




############################################################################################
############################################################################################
########### Proyecciones por unidad
############################################################################################
############################################################################################

#OBSERVATION !!!!!!!

#sOME UNITS SHOWS FLAT FORECAST, That is the predict points are constant across the forecast period. The Package administrartor state this in their web page

## FLAT FORECASTS. This is not a bug. It is telling you something about the time series - namely that there is no trend, no seasonality, 
#and insufficient temporal dynamics to allow the future observations to have different conditional means.

#I discussed this once with another consultant and he told me that he sometimes adds some random noise to his forecasts, 
#just to stop his clients questioning the flat forecast functions. 
#Unfortunately, that increases the forecast error, but he thought it was better to give them what they wanted rather than what was best!(http://robjhyndman.com/hyndsight/flat-forecasts/)

# What the flat-line forecasts indicate is that how these amounts will fluctuate is not predictable based solely on the historic data. 
#The expected range of the fluctuations is shown by the confidence limits (http://www.forecastpro.com/Trends/forecasting101August2012.html)


#We have thye following cases:

#a) Both point forecasts and confidence limits are variable
#b) Despite confidence limits are variable, point forecast are not (Flat point forecast). We will use the uppe limit of the confidence interval because at global level the visitors of the SNASPE increase at rate of 6% yearly  between 1990 to 2016
#c) Both point forecast and confidence limits are flat. To proyect this we will calculate the rate of chang of visitorss for the given region

library(forecast)
library(xts)


visitas.anuales<-read.table("visitas.anuales.txt",header=T)


tail(visitas.anuales)



data<-subset(visitas.anuales,visitas.anuales$variable == "Total")

str(data)
head(data)

length(unique(data$Id2))

# Eliminar unidades con cero visitas en el periodo 1990-2016
#wide.format<-xtabs(visitas.anuales$value~visitas.anuales$Id2+visitas.anuales$year)
#write.table(wide.format, file="visitas_historicasWide.txt")

dat0 <- subset(data,Id2!="100.RN.Katalalixar" & Id2!="109.MN.Laguna_de_los_Cisnes" & Id2!="111.PN.Alberto_de_Agostini" & Id2!="12.PN.Llullaillaco"& Id2!="22.MN.Isla_Cachagua"& Id2!="24.RN.Rio_Blanco"& Id2!="34.PN.Las_Palmas_de_Cocalan" & Id2!="64.PN.VILLARICA.SUR"& Id2!="67.SN.RIO_CRUCES"& Id2!="79.PN.Corcovado"& Id2!="81.RN.Lago_Rosselot"& Id2!="83.RN.Lago_Carlota"&Id2!="85.PN.Isla_Magdalena"&Id2!="86.PN.Isla_Guamblin"&Id2!="88.MN.Cinco_Hermanas"&Id2!="93.RN.Las_Guaitecas"&Id2!="95.RN.LAGO_GENERAL_CARRERA")                                     

dat0$Id2 <- factor(dat0$Id2)     # this is needed to remove   t h e   l e v e l s   o f   the excluded units
length(unique(dat0$Id2))





####################################
# PN Lauca
####################################
Lauca<-subset(dat0,dat0$Id2 == "1.PN.Lauca")

str(Lauca)
Lauca


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.Lauca<-ts(Lauca$value, start=1990,end=2016, frequency = 1)

plot(visitors.Lauca)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.Lauca)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.Lauca)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.Lauca)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.Lauca)[length(visitors.Lauca)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                        visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))





forecast_df
write.table(forecast_df, file="1.PN.Lauca_ARIMA_a.txt")



####################################
# 3.RN.las_Vicunas
####################################
LasVicunas<-subset(dat0,dat0$Id2 == "3.RN.las_Vicunas")

str(LasVicunas)
LasVicunas


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.LasVicunas<-ts(LasVicunas$value, start=1990,end=2016, frequency = 1)

plot(visitors.LasVicunas)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.LasVicunas)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.LasVicunas)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.LasVicunas)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.LasVicunas)[length(visitors.LasVicunas)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))





forecast_df
write.table(forecast_df, file="3.RN.las_Vicunas_ARIMA_a.txt")

####################################
# 4.MN.Salar_de_Surire
####################################

SURIRE<-subset(dat0,dat0$Id2 == "4.MN.Salar_de_Surire")

str(SURIRE)
SURIRE


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.SURIRE<-ts(SURIRE$value, start=1990,end=2016, frequency = 1)

plot(visitors.SURIRE)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.SURIRE)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.SURIRE)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.SURIRE)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.SURIRE)[length(visitors.SURIRE)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_tbats$mean),
                         visitors_lower= round(f_tbats$lower[,2]),
                         visitors_upper=round(f_tbats$upper[,2]),
                         date=last_date + seq(1, 20, by=1))





forecast_df
write.table(forecast_df, file="4.MN.Salar_de_Surire_TBATS_a.txt")



#################################
# 5.PN.Volcan_Isluga . FLAT FORECAST
####################################


ISLUGA<-subset(dat0,dat0$Id2 == "5.PN.Volcan_Isluga")

str(ISLUGA)
ISLUGA


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.ISLUGA<-ts(ISLUGA$value, start=1990,end=2016, frequency = 1)

plot(visitors.ISLUGA)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.ISLUGA)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.ISLUGA)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.ISLUGA)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.ISLUGA)[length(visitors.ISLUGA)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_tbats$mean),
                         visitors_lower= round(f_tbats$lower[,2]),
                         visitors_upper=round(f_tbats$upper[,2]),
                         date=last_date + seq(1, 20, by=1))





forecast_df
write.table(forecast_df, file="5.PN.Volcan_Isluga.Proyeccion_TBATS_c.txt")



####################################
#6.RN.Pampa_del_Tamarugal
####################################


TAMARUGAL<-subset(dat0,dat0$Id2 == "6.RN.Pampa_del_Tamarugal")

str(TAMARUGAL)
TAMARUGAL


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.TAMARUGAL<-ts(TAMARUGAL$value, start=1990,end=2016, frequency = 1)

plot(visitors.TAMARUGAL)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.TAMARUGAL)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.TAMARUGAL)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.TAMARUGAL)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.TAMARUGAL)[length(visitors.TAMARUGAL)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))





forecast_df
write.table(forecast_df, file="6.RN.Pampa_del_Tamarugal.Proyeccion_ARIMA_b.txt")


####################################
#7.RN.Los_Flamencos
####################################



FLAMENCOS<-subset(dat0,dat0$Id2 == "7.RN.Los_Flamencos")

str(FLAMENCOS)
FLAMENCOS


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.FLAMENCOS<-ts(FLAMENCOS$value, start=1990,end=2016, frequency = 1)

plot(visitors.FLAMENCOS)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.FLAMENCOS)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.FLAMENCOS)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.FLAMENCOS)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.FLAMENCOS)[length(visitors.FLAMENCOS)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))





forecast_df
write.table(forecast_df, file="7.RN.Los_Flamencos.Proyeccion_ARIMA_a.txt")



##################################################
#8.MN.La_Portada...
##################################################


LA.PORTADA<-subset(dat0,dat0$Id2 == "8.MN.La_Portada")

str(LA.PORTADA)
LA.PORTADA


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.LA.PORTADA<-ts(LA.PORTADA$value, start=1990,end=2016, frequency = 1)

plot(visitors.LA.PORTADA)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.LA.PORTADA)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.LA.PORTADA)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.LA.PORTADA)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.LA.PORTADA)[length(visitors.LA.PORTADA)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))





forecast_df
write.table(forecast_df, file="8.MN.La_Portada.Proyeccion_ARIMA_a.txt")




##################################################
#9.RN.La_Chimba
##################################################


CHIMBA<-subset(dat0,dat0$Id2 == "9.RN.La_Chimba")

str(CHIMBA)
CHIMBA


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.CHIMBA<-ts(CHIMBA$value, start=1990,end=2016, frequency = 1)

plot(visitors.CHIMBA)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.CHIMBA)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.CHIMBA)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.CHIMBA)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.CHIMBA)[length(visitors.CHIMBA)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))





forecast_df
write.table(forecast_df, file="9.RN.La_Chimba_ARIMA_a.txt")



##################################################
#11.MN.Paposo_Norte
##################################################


PAPOSO<-subset(dat0,dat0$Id2 == "11.MN.Paposo_Norte")

str(PAPOSO)
PAPOSO


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.PAPOSO<-ts(PAPOSO$value, start=1990,end=2016, frequency = 1)

plot(visitors.PAPOSO)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.PAPOSO)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.PAPOSO)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.PAPOSO)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.PAPOSO)[length(visitors.PAPOSO)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))





forecast_df
write.table(forecast_df, file="11.MN.Paposo_Norte.Proyeccion_ARIMA_b.txt")


##################################################
#13.PN.Pan_de_Azucar...FLAT FORECAST
##################################################


PANAZUCAR<-subset(dat0,dat0$Id2 == "13.PN.Pan_de_Azucar")

str(PANAZUCAR)
PANAZUCAR


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.PAZUCAR<-ts(PANAZUCAR$value, start=1990,end=2016, frequency = 1)

plot(visitors.PAZUCAR)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.PAZUCAR)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.PAZUCAR)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.PAZUCAR)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.PAZUCAR)[length(visitors.PAZUCAR)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))





forecast_df
write.table(forecast_df, file="13.PN.Pan_de_Azucar.Proyeccion_ARIMA_b.txt")


##################################################
#14.PN.Nevado_de_Tres_Cruces

##################################################

TRESCRUCES<-subset(dat0,dat0$Id2 == "14.PN.Nevado_de_Tres_Cruces")

str(TRESCRUCES)
TRESCRUCES


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.TRESCRUCES<-ts(TRESCRUCES$value, start=1990,end=2016, frequency = 1)

plot(visitors.TRESCRUCES)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.TRESCRUCES)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.TRESCRUCES)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.TRESCRUCES)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.TRESCRUCES)[length(visitors.TRESCRUCES)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.TRESCRUCES))

forecast_df
write.table(forecast_df, file="14.PN.Nevado_de_Tres_Cruces.Proyeccion_ARIMA_b.txt")






##################################################
#15.PN.Llanos_de_Challe

##################################################

Challes<-subset(dat0,dat0$Id2 == "15.PN.Llanos_de_Challe")

str(Challes)
Challes


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.Challes<-ts(Challes$value, start=1990,end=2016, frequency = 1)

plot(visitors.Challes)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.Challes)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.Challes)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.Challes)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.Challes)[length(visitors.Challes)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.Challes))

forecast_df
write.table(forecast_df, file="15.PN.Llanos_de_Challe.Proyeccion_ARIMA_a.txt")



##################################################
#17.RN.Pinguino_de_Humboldt.2

##################################################

pinguino.humboldt<-subset(dat0,dat0$Id2 == "17.RN.Pinguino_de_Humboldt.2")

str(pinguino.humboldt)
pinguino.humboldt


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

pinguino.humboldt<-ts(pinguino.humboldt$value, start=1990,end=2016, frequency = 1)

plot(pinguino.humboldt)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.Challes)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.Challes)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.Challes)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.Challes)[length(visitors.Challes)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.Challes))

forecast_df
write.table(forecast_df, file="17.RN.Pinguino_de_Humboldt.2.Proyeccion_ARIMA_a.txt")



##################################################
#18.PN.Bosque_Fray_Jorge

##################################################

fray.jorge<-subset(dat0,dat0$Id2 == "18.PN.Bosque_Fray_Jorge")

str(fray.jorge)
fray.jorge


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.jorge<-ts(fray.jorge$value, start=1990,end=2016, frequency = 1)

plot(visitors.jorge)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.jorge)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.jorge)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.jorge)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.jorge)[length(visitors.jorge)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.jorge))

forecast_df
write.table(forecast_df, file="18.PN.Bosque_Fray_Jorge.Proyeccion_ARIMA_a.txt")



##################################################
#19.MN.Pichasca
##################################################

pichasca<-subset(dat0,dat0$Id2 == "19.MN.Pichasca")

str(pichasca)
pichasca


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.pichasca<-ts(pichasca$value, start=1990,end=2016, frequency = 1)

plot(visitors.pichasca)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.pichasca)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.pichasca)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.pichasca)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.pichasca)[length(visitors.pichasca)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_tbats$mean),
                         visitors_lower= round(f_tbats$lower[,2]),
                         visitors_upper=round(f_tbats$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.pichasca))

forecast_df
write.table(forecast_df, file="19.MN.Pichasca.Proyeccion_tbats_a.txt")







##################################################
#20.RN.Las_Chinchillas
##################################################

chinchillas<-subset(dat0,dat0$Id2 == "20.RN.Las_Chinchillas")

str(chinchillas)
chinchillas


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.chinchilla<-ts(chinchillas$value, start=1990,end=2016, frequency = 1)

plot(visitors.chinchilla)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.chinchilla)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.chinchilla)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.chinchilla)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.chinchilla)[length(visitors.chinchilla)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.chinchilla))

forecast_df
write.table(forecast_df, file="20.RN.Las_Chinchillas.Proyeccion_ARIMA_B.txt")




##################################################
#21.PN.Rapa_Nui
##################################################

ipascua<-subset(dat0,dat0$Id2 == "21.PN.Rapa_Nui")

str(ipascua)
ipascua


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.ipascua<-ts(ipascua$value, start=1990,end=2016, frequency = 1)

plot(visitors.ipascua)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.ipascua)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.ipascua)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.ipascua)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.ipascua)[length(visitors.ipascua)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.ipascua))

forecast_df
write.table(forecast_df, file="21.PN.Rapa_Nui.Proyeccion_ARIMA_A.txt")



##################################################
#23.PN.La_Campana
##################################################

CAMPANA<-subset(dat0,dat0$Id2 == "23.PN.La_Campana")

str(CAMPANA)
CAMPANA


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.CAMPANA<-ts(CAMPANA$value, start=1990,end=2016, frequency = 1)

plot(visitors.CAMPANA)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.CAMPANA)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.CAMPANA)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.CAMPANA)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.CAMPANA)[length(visitors.CAMPANA)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.CAMPANA))

forecast_df
write.table(forecast_df, file="23.PN.La_Campana.Proyeccion_ARIMA_B.txt")






##################################################
#25.RN.Lago_Penuelas
##################################################

PENUELAS<-subset(dat0,dat0$Id2 == "25.RN.Lago_Penuelas")

str(PENUELAS)
PENUELAS


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.PENUELAS<-ts(PENUELAS$value, start=1990,end=2016, frequency = 1)

plot(visitors.PENUELAS)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.PENUELAS)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.PENUELAS)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.PENUELAS)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.PENUELAS)[length(visitors.PENUELAS)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.PENUELAS))

forecast_df
write.table(forecast_df, file="25.RN.Lago_Penuelas_ARIMA_C.txt")





##################################################
#26.RN.El_Yali
##################################################

YALI<-subset(dat0,dat0$Id2 == "26.RN.El_Yali")

str(YALI)
YALI


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.YALI<-ts(YALI$value, start=1990,end=2016, frequency = 1)

plot(visitors.YALI)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.YALI)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.YALI)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.YALI)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.YALI)[length(visitors.YALI)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.PENUELAS))

forecast_df
write.table(forecast_df, file="26.RN.El_Yali.Proyeccion_ARIMA_A.txt")

##################################################
#28.SN.LAGUNA_EL_PERAL
##################################################

PERAL<-subset(dat0,dat0$Id2 == "28.SN.LAGUNA_EL_PERAL")

str(PERAL)
PERAL



#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.PERAL<-ts(PERAL$value, start=1990,end=2016, frequency = 1)

plot(visitors.PERAL)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.PERAL)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.PERAL)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.PERAL)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.PERAL)[length(visitors.PERAL)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.PENUELAS))

forecast_df
write.table(forecast_df, file="28.SN.LAGUNA_EL_PERAL.Proyeccion_ARIMA_B.txt")





##################################################
#29.PN.Arch_de_Juan_Fernandez
##################################################

ARCH.JF<-subset(dat0,dat0$Id2 == "29.PN.Arch_de_Juan_Fernandez")

str(ARCH.JF)
ARCH.JF



#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.ARCH.JF<-ts(ARCH.JF$value, start=1990,end=2016, frequency = 1)

plot(visitors.ARCH.JF)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.ARCH.JF)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.ARCH.JF)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.ARCH.JF)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.ARCH.JF)[length(visitors.ARCH.JF)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.PENUELAS))

forecast_df
write.table(forecast_df, file="29.PN.Arch_de_Juan_Fernandez.Proyeccion_ARIMA_B.txt")




##################################################
#30.RN.Rio_Clarillo
##################################################

CLARILLO<-subset(dat0,dat0$Id2 == "30.RN.Rio_Clarillo")

str(CLARILLO)
CLARILLO



#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.CLARILLO<-ts(CLARILLO$value, start=1990,end=2016, frequency = 1)

plot(visitors.CLARILLO)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.CLARILLO)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.CLARILLO)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.CLARILLO)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.CLARILLO)[length(visitors.CLARILLO)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.CLARILLO))

forecast_df
write.table(forecast_df, file="30.RN.Rio_Clarillo.Proyeccion_ARIMA_B.txt")




##################################################
#31.MN.El_Morado
##################################################

MORADO<-subset(dat0,dat0$Id2 == "31.MN.El_Morado")

str(MORADO)
MORADO



#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.MORADO<-ts(MORADO$value, start=1990,end=2016, frequency = 1)

plot(visitors.MORADO)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.MORADO)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.MORADO)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.MORADO)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.MORADO)[length(visitors.MORADO)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.MORADO))

forecast_df
write.table(forecast_df, file="31.MN.El_Morado.Proyeccion_ARIMA_A.txt")





##################################################
#33.RN.Rob_del_Cobre_de_Loncha
##################################################

Loncha<-subset(dat0,dat0$Id2 == "33.RN.Rob_del_Cobre_de_Loncha")

str(Loncha)
Loncha



#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.Loncha<-ts(Loncha$value, start=1990,end=2016, frequency = 1)

plot(visitors.Loncha)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.Loncha)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.Loncha)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.Loncha)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.MORADO)[length(visitors.Loncha)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.Loncha))

forecast_df
write.table(forecast_df, file="33.RN.Rob_del_Cobre_de_Loncha.Proyeccion_ARIMA_B.txt")








##################################################
#35.RN.Rio_de_los_Cipreses
##################################################

CIPRESES<-subset(dat0,dat0$Id2 == "35.RN.Rio_de_los_Cipreses")

str(CIPRESES)
CIPRESES



#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.CIPRESES<-ts(CIPRESES$value, start=1990,end=2016, frequency = 1)

plot(visitors.CIPRESES)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.CIPRESES)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.CIPRESES)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.CIPRESES)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.CIPRESES)[length(visitors.CIPRESES)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.CIPRESES))

forecast_df
write.table(forecast_df, file="35.RN.Rio_de_los_Cipreses.Proyeccion_ARIMA_A.txt")






##################################################
#36.RN.Laguna_Torca
##################################################

TORCA<-subset(dat0,dat0$Id2 == "36.RN.Laguna_Torca")

str(TORCA)
TORCA



#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.TORCA<-ts(TORCA$value, start=1990,end=2016, frequency = 1)

plot(visitors.TORCA)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.TORCA)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.TORCA)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.TORCA)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.TORCA)[length(visitors.TORCA)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.TORCA))

forecast_df
write.table(forecast_df, file="36.RN.Laguna_Torca.Proyeccion_ARIMA_A.txt")





##################################################
#37.PN.Radal_Siete_Tazas
##################################################

SIETE.TAZAS<-subset(dat0,dat0$Id2 == "37.PN.Radal_Siete_Tazas")

str(SIETE.TAZAS)
SIETE.TAZAS


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.SIETE.TAZAS<-ts(SIETE.TAZAS$value, start=1990,end=2016, frequency = 1)

plot(visitors.SIETE.TAZAS)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.SIETE.TAZAS)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.SIETE.TAZAS)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.SIETE.TAZAS)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.SIETE.TAZAS)[length(visitors.SIETE.TAZAS)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.SIETE.TAZAS))

forecast_df
write.table(forecast_df, file="37.PN.Radal_Siete_Tazas.Proyeccion_ARIMA_B.txt")





##################################################
#39.RN.Altos_de_Lircay
##################################################

LIRCAY<-subset(dat0,dat0$Id2 == "39.RN.Altos_de_Lircay")

str(LIRCAY)
LIRCAY


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.LIRCAY<-ts(LIRCAY$value, start=1990,end=2016, frequency = 1)

plot(visitors.LIRCAY)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.LIRCAY)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.LIRCAY)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.LIRCAY)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.LIRCAY)[length(visitors.LIRCAY)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.LIRCAY))

forecast_df
write.table(forecast_df, file="39.RN.Altos_de_Lircay.Proyeccion_ARIMA_A.txt")






##################################################
#40.RN.Federico_Albert
##################################################

F.ALBERT<-subset(dat0,dat0$Id2 == "40.RN.Federico_Albert")

str(F.ALBERT)
F.ALBERT


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.F.ALBERT<-ts(F.ALBERT$value, start=1990,end=2016, frequency = 1)

plot(visitors.F.ALBERT)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.F.ALBERT)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.F.ALBERT)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.F.ALBERT)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.F.ALBERT)[length(visitors.F.ALBERT)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.F.ALBERT))

forecast_df
write.table(forecast_df, file="40.RN.Federico_Albert.Proyeccion_ARIMA_A.txt")




##################################################
#41.RN.Los_Ruiles
##################################################

RUILES<-subset(dat0,dat0$Id2 == "41.RN.Los_Ruiles")

str(RUILES)
RUILES


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.RUILES<-ts(RUILES$value, start=1990,end=2016, frequency = 1)

plot(visitors.RUILES)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.RUILES)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.RUILES)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.RUILES)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.RUILES)[length(visitors.RUILES)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_ets$mean),
                         visitors_lower= round(f_ets$lower[,2]),
                         visitors_upper=round(f_ets$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.RUILES))

forecast_df
write.table(forecast_df, file="41.RN.Los_Ruiles.Proyeccion_ETS_B.txt")




##################################################
#42.RN.Los_Bellotos_del_Melado
##################################################

BELLOTOS<-subset(dat0,dat0$Id2 == "42.RN.Los_Bellotos_del_Melado")

str(BELLOTOS)
BELLOTOS


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.BELLOTOS<-ts(BELLOTOS$value, start=1990,end=2016, frequency = 1)

plot(visitors.BELLOTOS)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.BELLOTOS)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.BELLOTOS)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.BELLOTOS)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.BELLOTOS)[length(visitors.BELLOTOS)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.BELLOTOS))

forecast_df
write.table(forecast_df, file="42.RN.Los_Bellotos_del_Melado.Proyeccion_ARIMA_B.txt")



##################################################
#43.RN.Los_Queules
##################################################

QUEULES<-subset(dat0,dat0$Id2 == "43.RN.Los_Queules")

str(QUEULES)
QUEULES


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.QUEULES<-ts(QUEULES$value, start=1990,end=2016, frequency = 1)

plot(visitors.QUEULES)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.QUEULES)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.QUEULES)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.QUEULES)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.QUEULES)[length(visitors.QUEULES)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.QUEULES))

forecast_df
write.table(forecast_df, file="43.RN.Los_Queules.Proyeccion_ARIMA_B.txt")






##################################################
#44.RN.Los_Huemules_de_Niblinto
##################################################

HUEMULESN<-subset(dat0,dat0$Id2 == "44.RN.Los_Huemules_de_Niblinto")

str(HUEMULESN)
HUEMULESN


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.HUEMULESN<-ts(HUEMULESN$value, start=1990,end=2016, frequency = 1)

plot(visitors.HUEMULESN)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.HUEMULESN)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.HUEMULESN)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.HUEMULESN)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.HUEMULESN)[length(visitors.HUEMULESN)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.HUEMULESN))

forecast_df
write.table(forecast_df, file="44.RN.Los_Huemules_de_Niblinto.Proyeccion_ARIMA_A.txt")






##################################################
#45.RN.Nonguen
##################################################

NONGUEN<-subset(dat0,dat0$Id2 == "45.RN.Nonguen")

str(NONGUEN)
NONGUEN


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.NONGUEN<-ts(NONGUEN$value, start=1990,end=2016, frequency = 1)

plot(visitors.NONGUEN)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.NONGUEN)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.NONGUEN)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.NONGUEN)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.NONGUEN)[length(visitors.NONGUEN)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.NONGUEN))

forecast_df
write.table(forecast_df, file="45.RN.Nonguen.Proyeccion_ARIMA_A.txt")




##################################################
#46.RN.nuble
##################################################

NUBLE<-subset(dat0,dat0$Id2 == "46.RN.nuble")

str(NUBLE)
NUBLE


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.NUBLE<-ts(NUBLE$value, start=1990,end=2016, frequency = 1)

plot(visitors.NUBLE)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.NUBLE)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.NUBLE)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.NUBLE)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.NUBLE)[length(visitors.NUBLE)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.NUBLE))

forecast_df
write.table(forecast_df, file="46.RN.nuble.Proyeccion_ARIMA_A.txt")


##################################################
#47.PN.Laguna_del_Laja
##################################################

LAJA<-subset(dat0,dat0$Id2 == "47.PN.Laguna_del_Laja")

str(LAJA)
LAJA


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.LAJA<-ts(LAJA$value, start=1990,end=2016, frequency = 1)

plot(visitors.LAJA)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.LAJA)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.LAJA)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.LAJA)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.LAJA)[length(visitors.LAJA)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.LAJA))

forecast_df
write.table(forecast_df, file="47.PN.Laguna_del_Laja.Proyeccion_ARIMA_a.txt")




##################################################
#48.PN.Nahuelbuta
##################################################

NAHUELBUTA<-subset(dat0,dat0$Id2 == "48.PN.Nahuelbuta")

str(NAHUELBUTA)
NAHUELBUTA


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.NAHUELBUTA<-ts(NAHUELBUTA$value, start=1990,end=2016, frequency = 1)

plot(visitors.NAHUELBUTA)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.NAHUELBUTA)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.NAHUELBUTA)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.NAHUELBUTA)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.NAHUELBUTA)[length(visitors.NAHUELBUTA)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.NAHUELBUTA))

forecast_df
write.table(forecast_df, file="48.PN.Nahuelbuta.Proyeccion_ARIMA_A.txt")



##################################################
#49.RN.Ralco
##################################################

RALCO<-subset(dat0,dat0$Id2 == "49.RN.Ralco")

str(RALCO)
RALCO


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.RALCO<-ts(RALCO$value, start=1990,end=2016, frequency = 1)

plot(visitors.RALCO)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.RALCO)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.RALCO)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.RALCO)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.RALCO)[length(visitors.RALCO)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.RALCO))

forecast_df
write.table(forecast_df, file="49.RN.Ralco.Proyeccion_ARIMA_a.txt")




##################################################
#50.RN.Altos_de_Pemehue
##################################################

PEMEHUE<-subset(dat0,dat0$Id2 == "50.RN.Altos_de_Pemehue")

str(PEMEHUE)
PEMEHUE


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.PEMEHUE<-ts(PEMEHUE$value, start=1990,end=2016, frequency = 1)

plot(visitors.PEMEHUE)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.PEMEHUE)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.PEMEHUE)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.PEMEHUE)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.PEMEHUE)[length(visitors.PEMEHUE)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.PEMEHUE))

forecast_df
write.table(forecast_df, file="50.RN.Altos_de_Pemehue.Proyeccion_ARIMA_a.txt")




##################################################
#51.RN.Malleco
##################################################

MALLECO<-subset(dat0,dat0$Id2 == "51.RN.Malleco")

str(MALLECO)
MALLECO


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.MALLECO<-ts(MALLECO$value, start=1990,end=2016, frequency = 1)

plot(visitors.MALLECO)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.MALLECO)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.MALLECO)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.MALLECO)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.MALLECO)[length(visitors.MALLECO)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.MALLECO))

forecast_df
write.table(forecast_df, file="51.RN.Malleco.Proyeccion_ARIMA_C.txt")





##################################################
#52.MN.Contulmo
##################################################

contulmo<-subset(dat0,dat0$Id2 == "52.MN.Contulmo")

str(contulmo)
contulmo


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.contulmo<-ts(contulmo$value, start=1990,end=2016, frequency = 1)

plot(visitors.contulmo)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.contulmo)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.contulmo)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.contulmo)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.contulmo)[length(visitors.contulmo)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.contulmo))

forecast_df
write.table(forecast_df, file="52.MN.Contulmo.Proyeccion_ARIMA_b.txt")





##################################################
#53.PN.Tolhuaca
##################################################

tolhuaca<-subset(dat0,dat0$Id2 == "53.PN.Tolhuaca")

str(tolhuaca)
tolhuaca


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.tolhuaca<-ts(tolhuaca$value, start=1990,end=2016, frequency = 1)

plot(visitors.tolhuaca)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.tolhuaca)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.tolhuaca)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.tolhuaca)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.tolhuaca)[length(visitors.tolhuaca)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.tolhuaca))

forecast_df
write.table(forecast_df, file="53.PN.Tolhuaca.Proyeccion_ARIMA_a.txt")






##################################################
#54.RN.Nalcas
##################################################

nalcas<-subset(dat0,dat0$Id2 == "54.RN.Nalcas")

str(nalcas)
nalcas


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.nalcas<-ts(nalcas$value, start=1990,end=2016, frequency = 1)

plot(visitors.nalcas)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.nalcas)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.nalcas)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.nalcas)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.nalcas)[length(visitors.nalcas)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.nalcas))

forecast_df
write.table(forecast_df, file="54.RN.Nalcas.Proyeccion_Visitantes.ARIMA_c.txt")



##################################################
#55.RN.Malalcahuello
##################################################

malalcahuello<-subset(dat0,dat0$Id2 == "55.RN.Malalcahuello")

str(malalcahuello)
malalcahuello


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.malalcahuello<-ts(malalcahuello$value, start=1990,end=2016, frequency = 1)

plot(visitors.malalcahuello)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.malalcahuello)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.malalcahuello)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.malalcahuello)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.malalcahuello)[length(visitors.malalcahuello)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.malalcahuello))

forecast_df
write.table(forecast_df, file="55.RN.Malalcahuello.Proyeccion_Visitantes_ARIMA_A.txt")




##################################################
#56.RN.Isla_Mocha
##################################################

mocha<-subset(dat0,dat0$Id2 == "56.RN.Isla_Mocha")

str(mocha)
mocha


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.mocha<-ts(mocha$value, start=1990,end=2016, frequency = 1)

plot(visitors.mocha)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.mocha)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.mocha)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.mocha)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.mocha)[length(visitors.mocha)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.mocha))

forecast_df
write.table(forecast_df, file="56.RN.Isla_Mocha.Proyeccion_Visitantes_ARIMA.txt")



##################################################
#57.MN.Cerro_nielol
##################################################

nielol<-subset(dat0,dat0$Id2 == "57.MN.Cerro_nielol")

str(nielol)
nielol


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.nielol<-ts(nielol$value, start=1990,end=2016, frequency = 1)

plot(visitors.nielol)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.nielol)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.nielol)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.nielol)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.nielol)[length(visitors.nielol)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.nielol))

forecast_df
write.table(forecast_df, file="57.MN.Cerro_nielol.Proyeccion_Visitantes_ARIMA.A.txt")



##################################################
#58.PN.Conguillio
##################################################

conguillio<-subset(dat0,dat0$Id2 == "58.PN.Conguillio")

str(conguillio)
conguillio


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.conguillio<-ts(conguillio$value, start=1990,end=2016, frequency = 1)

plot(visitors.conguillio)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.conguillio)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.conguillio)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.conguillio)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.conguillio)[length(visitors.conguillio)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.conguillio))

forecast_df
write.table(forecast_df, file="58.PN.Conguillio.Proyeccion_Visitantes_ARIMA_B.txt")





##################################################
#59.RN.Alto_Bio_Bio
##################################################

A.biobio<-subset(dat0,dat0$Id2 == "59.RN.Alto_Bio_Bio")

str(A.biobio)
A.biobio


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.A.biobio<-ts(A.biobio$value, start=1990,end=2016, frequency = 1)

plot(visitors.A.biobio)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.A.biobio)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.A.biobio)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.A.biobio)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.A.biobio)[length(visitors.A.biobio)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.A.biobio))

forecast_df
write.table(forecast_df, file="59.RN.Alto_Bio_Bio.Proyeccion_Visitantes_ARIMA_B.txt")




##################################################
#60.RN.China_Muerta
##################################################

china.m<-subset(dat0,dat0$Id2 == "60.RN.China_Muerta")

str(china.m)
china.m


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.china.m<-ts(china.m$value, start=1990,end=2016, frequency = 1)

plot(visitors.china.m)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.china.m)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.china.m)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.china.m)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.china.m)[length(visitors.china.m)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.china.m))

forecast_df
write.table(forecast_df, file="60.RN.China_Muerta.Proyeccion_Visitantes_ARIMA.C.txt")



##################################################
#61.RN.Villarrica
##################################################

villarica<-subset(dat0,dat0$Id2 == "61.RN.Villarrica")

str(villarica)
villarica


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.villarica<-ts(villarica$value, start=1990,end=2016, frequency = 1)

plot(visitors.villarica)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.villarica)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.villarica)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.villarica)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.villarica)[length(visitors.villarica)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.villarica))

forecast_df
write.table(forecast_df, file="61.RN.Villarrica.Proyeccion_Visitantes_ARIMA_b.txt")




##################################################
#62.PN.Huerquehue
##################################################

HUERQUEHUE<-subset(dat0,dat0$Id2 == "62.PN.Huerquehue")

str(HUERQUEHUE)
HUERQUEHUE


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.HUERQUEHUE<-ts(HUERQUEHUE$value, start=1990,end=2016, frequency = 1)

plot(visitors.HUERQUEHUE)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.HUERQUEHUE)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.HUERQUEHUE)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.HUERQUEHUE)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.HUERQUEHUE)[length(visitors.HUERQUEHUE)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.HUERQUEHUE))

forecast_df
write.table(forecast_df, file="62.PN.Huerquehue.Proyeccion_Visitantes_A.txt")




##################################################
#63.PN.Villarrica
##################################################

PN.Villarrica<-subset(dat0,dat0$Id2 == "63.PN.Villarrica")

str(PN.Villarrica)
PN.Villarrica


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.PN.Villarrica<-ts(PN.Villarrica$value, start=1990,end=2016, frequency = 1)

plot(visitors.PN.Villarrica)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.PN.Villarrica)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.PN.Villarrica)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.PN.Villarrica)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.PN.Villarrica)[length(visitors.PN.Villarrica)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.PN.Villarrica))

forecast_df
write.table(forecast_df, file="63.PN.Villarrica.Proyeccion_Visitantes.B.txt")


##################################################
#66.RN.Mocho-Choshuenco
##################################################

RN.Choshuenco<-subset(dat0,dat0$Id2 =="66.RN.Mocho-Choshuenco")

str(RN.Choshuenco)
RN.Choshuenco


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.RN.Choshuenco<-ts(RN.Choshuenco$value, start=1990,end=2016, frequency = 1)

plot(visitors.RN.Choshuenco)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.RN.Choshuenco)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.RN.Choshuenco)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.RN.Choshuenco)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.RN.Choshuenco)[length(visitors.RN.Choshuenco)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.RN.Choshuenco))

forecast_df
write.table(forecast_df, file="66.RN.Mocho-Choshuenco.Proyeccion_Visitantes.A.txt")

##################################################
#69.PN.Alerce_Costero
##################################################

PN.ACostero<-subset(dat0,dat0$Id2 =="69.PN.Alerce_Costero")

str(PN.ACostero)
PN.ACostero


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.PN.ACostero<-ts(PN.ACostero$value, start=1990,end=2016, frequency = 1)

plot(visitors.PN.ACostero)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.PN.ACostero)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.PN.ACostero)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.PN.ACostero)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.PN.ACostero)[length(visitors.PN.ACostero)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.PN.ACostero))

forecast_df
write.table(forecast_df, file="69.PN.Alerce_Costero.Proyeccion_Visitantes.A.txt")




##################################################
#70.PN.Puyehue
##################################################

PN.Puyehue<-subset(dat0,dat0$Id2 =="70.PN.Puyehue")

str(PN.Puyehue)
PN.Puyehue


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.PN.Puyehue<-ts(PN.Puyehue$value, start=1990,end=2016, frequency = 1)

plot(visitors.PN.Puyehue)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.PN.Puyehue)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.PN.Puyehue)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.PN.Puyehue)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.PN.Puyehue)[length(visitors.PN.Puyehue)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.PN.Puyehue))

forecast_df
write.table(forecast_df, file="70.PN.Puyehue.Proyeccion_Visitantes_ARIMA.B.txt")



##################################################
#71.PN.Vicente_Perez_Rosales
##################################################

VPRosales<-subset(dat0,dat0$Id2 =="71.PN.Vicente_Perez_Rosales")

str(VPRosales)
VPRosales


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.VPRosales<-ts(VPRosales$value, start=1990,end=2016, frequency = 1)

plot(visitors.VPRosales)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.VPRosales)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.VPRosales)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.VPRosales)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.VPRosales)[length(visitors.VPRosales)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.VPRosales))

forecast_df
write.table(forecast_df, file="71.PN.Vicente_Perez_Rosales.Proyeccion_Visitantes_ARIMA.A.txt")






##################################################
#72.RN.Llanquihue
##################################################

Llanquihue<-subset(dat0,dat0$Id2 =="72.RN.Llanquihue")

str(Llanquihue)
Llanquihue


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.Llanquihue<-ts(Llanquihue$value, start=1990,end=2016, frequency = 1)

plot(visitors.Llanquihue)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.Llanquihue)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.Llanquihue)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.Llanquihue)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.Llanquihue)[length(visitors.Llanquihue)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.Llanquihue))

forecast_df
write.table(forecast_df, file="72.RN.Llanquihue.Proyeccion_Visitantes_arima.b.txt")



##################################################
#73.MN.LahuEn_nadi
##################################################

lahuenn<-subset(dat0,dat0$Id2 =="73.MN.LahuEn_nadi")

str(lahuenn)
lahuenn


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.lahuenn<-ts(lahuenn$value, start=1990,end=2016, frequency = 1)

plot(visitors.lahuenn)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.lahuenn)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.lahuenn)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.lahuenn)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.lahuenn)[length(visitors.lahuenn)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.lahuenn))

forecast_df
write.table(forecast_df, file="73.MN.LahuEn_nadi.Proyeccion_Visitantes_ARIMA.B.txt")





##################################################
#74.PN.Alerce_Andino
##################################################

AlerceA<-subset(dat0,dat0$Id2 =="74.PN.Alerce_Andino")

str(AlerceA)
AlerceA


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.AlerceA<-ts(AlerceA$value, start=1990,end=2016, frequency = 1)

plot(visitors.AlerceA)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.AlerceA)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.AlerceA)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.AlerceA)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.AlerceA)[length(visitors.AlerceA)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.AlerceA))

forecast_df
write.table(forecast_df, file="74.PN.Alerce_Andino.Proyeccion_Visitantes_ARIMA.A.txt")





##################################################
#75.PN.Hornopiren
##################################################

Hornopiren<-subset(dat0,dat0$Id2 =="75.PN.Hornopiren")

str(Hornopiren)
Hornopiren


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.Hornopiren<-ts(Hornopiren$value, start=1990,end=2016, frequency = 1)

plot(visitors.Hornopiren)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.Hornopiren)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.Hornopiren)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.Hornopiren)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.Hornopiren)[length(visitors.Hornopiren)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.Hornopiren))

forecast_df
write.table(forecast_df, file="75.PN.Hornopiren.Proyeccion_Visitantes_ARIMA.a.txt")




##################################################
#76.MN.Islotes_de_Punihuil
##################################################

IslotesPunihuil<-subset(dat0,dat0$Id2 =="76.MN.Islotes_de_Punihuil")

str(IslotesPunihuil)
IslotesPunihuil


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.IslotesPunihuil<-ts(IslotesPunihuil$value, start=1990,end=2016, frequency = 1)

plot(visitors.IslotesPunihuil)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.IslotesPunihuil)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.IslotesPunihuil)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.IslotesPunihuil)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.IslotesPunihuil)[length(visitors.IslotesPunihuil)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.IslotesPunihuil))

forecast_df
write.table(forecast_df, file="76.MN.Islotes_de_Punihuil.Proyeccion_Visitantes-arima.b.txt")



##################################################
#77.PN.Chiloe
##################################################


PNChiloe<-subset(dat0,dat0$Id2 =="77.PN.Chiloe")

str(PNChiloe)
PNChiloe


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.PNChiloe<-ts(PNChiloe$value, start=1990,end=2016, frequency = 1)

plot(visitors.PNChiloe)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.PNChiloe)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.PNChiloe)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.PNChiloe)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.IslotesPunihuil)[length(visitors.IslotesPunihuil)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.PNChiloe))

forecast_df
write.table(forecast_df, file="77.PN.Chiloe.Proyeccion_Visitantes.arima.a.txt")





##################################################
#78.RN.Futaleufu
##################################################


Futaleufu<-subset(dat0,dat0$Id2 =="78.RN.Futaleufu")

str(Futaleufu)
Futaleufu


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.Futaleufu<-ts(Futaleufu$value, start=1990,end=2016, frequency = 1)

plot(visitors.Futaleufu)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.Futaleufu)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.Futaleufu)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.Futaleufu)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.Futaleufu)[length(visitors.Futaleufu)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.Futaleufu))

forecast_df
write.table(forecast_df, file="78.RN.Futaleufu.Proyeccion_Visitantes-arima.A.txt")



##################################################
#82.PN.Queulat
##################################################


Queulat<-subset(dat0,dat0$Id2 =="82.PN.Queulat")

str(Queulat)
Queulat


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.Queulat<-ts(Queulat$value, start=1990,end=2016, frequency = 1)

plot(visitors.Queulat)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.Queulat)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.Queulat)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.Queulat)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.Queulat)[length(visitors.Queulat)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.Queulat))

forecast_df
write.table(forecast_df, file="82.PN.Queulat.Proyeccion_Visitantes.arima.a.txt")





##################################################
#84.RN.Lago_Las_Torres
##################################################


LLasTorres<-subset(dat0,dat0$Id2 =="84.RN.Lago_Las_Torres")

str(LLasTorres)
LLasTorres


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.LLasTorres<-ts(LLasTorres$value, start=1990,end=2016, frequency = 1)

plot(visitors.LLasTorres)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.LLasTorres)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.LLasTorres)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.LLasTorres)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.LLasTorres)[length(visitors.LLasTorres)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.LLasTorres))

forecast_df
write.table(forecast_df, file="84.RN.Lago_Las_Torres.Proyeccion_Visitantes.Arima.b.txt")





##################################################
#87.RN.Trapananda
##################################################


Trapananda<-subset(dat0,dat0$Id2 =="87.RN.Trapananda")

str(Trapananda)
Trapananda


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.Trapananda<-ts(Trapananda$value, start=1990,end=2016, frequency = 1)

plot(visitors.Trapananda)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.Trapananda)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.Trapananda)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.Trapananda)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.Trapananda)[length(visitors.Trapananda)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.Trapananda))

forecast_df
write.table(forecast_df, file="87.RN.Trapananda.Proyeccion_Visitantes.arima.c.txt")


##################################################
#89.RN.Rio_Simpson
##################################################


RioSimpson<-subset(dat0,dat0$Id2 =="89.RN.Rio_Simpson")

str(RioSimpson)
RioSimpson


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.RioSimpson<-ts(RioSimpson$value, start=1990,end=2016, frequency = 1)

plot(visitors.RioSimpson)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.RioSimpson)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.RioSimpson)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.RioSimpson)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.RioSimpson)[length(visitors.RioSimpson)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.RioSimpson))

forecast_df
write.table(forecast_df, file="89.RN.Rio_Simpson.Proyeccion_Visitantes_arima.b.txt")



##################################################
#90.RN.Coyhaique
##################################################

Coyhaique<-subset(dat0,dat0$Id2 =="90.RN.Coyhaique")

str(Coyhaique)
Coyhaique


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.Coyhaique<-ts(Coyhaique$value, start=1990,end=2016, frequency = 1)

plot(visitors.Coyhaique)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.Coyhaique)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.Coyhaique)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.Coyhaique)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.Coyhaique)[length(visitors.Coyhaique)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_tbats$mean),
                         visitors_lower= round(f_tbats$lower[,2]),
                         visitors_upper=round(f_tbats$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.Coyhaique))

forecast_df
write.table(forecast_df, file="90.RN.Coyhaique.Proyeccion_Visitantes_tbats.b.txt")


##################################################
#91.MN.Dos_Lagunas
##################################################

DOS.LAGUNAS<-subset(dat0,dat0$Id2 =="91.MN.Dos_Lagunas")

str(DOS.LAGUNAS)
DOS.LAGUNAS


#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.DOS.LAGUNAS<-ts(DOS.LAGUNAS$value, start=1990,end=2016, frequency = 1)

plot(visitors.DOS.LAGUNAS)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.DOS.LAGUNAS)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.DOS.LAGUNAS)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.DOS.LAGUNAS)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.DOS.LAGUNAS)[length(visitors.DOS.LAGUNAS)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.DOS.LAGUNAS))

forecast_df
write.table(forecast_df, file="91.MN.DOS.LAGUNAS.Proyeccion_Visitantes_arima.b.txt")




##################################################
#92.RN.Cerro_Castillo
##################################################

C.CASTILLO<-subset(dat0,dat0$Id2 =="92.RN.Cerro_Castillo")

str(C.CASTILLO)
C.CASTILLO

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.C.CASTILLO<-ts(C.CASTILLO$value, start=1990,end=2016, frequency = 1)

plot(visitors.C.CASTILLO)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.C.CASTILLO)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.C.CASTILLO)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.C.CASTILLO)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.DOS.LAGUNAS)[length(visitors.C.CASTILLO)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_tbats$mean),
                         visitors_lower= round(f_tbats$lower[,2]),
                         visitors_upper=round(f_tbats$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.C.CASTILLO))

forecast_df
write.table(forecast_df, file="92.RN.Cerro_Castillo.TBATS.Proyeccion_Visitantes.b.txt")






##################################################
#96.RN.MANIHUALES
##################################################

MANIHUALES<-subset(dat0,dat0$Id2 =="96.RN.MANIHUALES")

str(MANIHUALES)
MANIHUALES

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.MANIHUALES<-ts(MANIHUALES$value, start=1990,end=2016, frequency = 1)

plot(visitors.MANIHUALES)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.MANIHUALES)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.MANIHUALES)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.MANIHUALES)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.MANIHUALES)[length(visitors.MANIHUALES)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa $mean),
                         visitors_lower= round(f_aa $lower[,2]),
                         visitors_upper=round(f_aa $upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.MANIHUALES))

forecast_df
write.table(forecast_df, file="96.RN.MANIHUALES.Proyeccion_Visitantes.ARIMA.C.txt")






##################################################
#97.RN.Lago_Jeinimeni
##################################################

jeinimeni<-subset(dat0,dat0$Id2 =="97.RN.Lago_Jeinimeni")

str(jeinimeni)
jeinimeni

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.jeinimeni<-ts(jeinimeni$value, start=1990,end=2016, frequency = 1)

plot(visitors.jeinimeni)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.jeinimeni)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.jeinimeni)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.jeinimeni)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.jeinimeni)[length(visitors.jeinimeni)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa $mean),
                         visitors_lower= round(f_aa $lower[,2]),
                         visitors_upper=round(f_aa $upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.jeinimeni))

forecast_df
write.table(forecast_df, file="97.RN.Lago_Jeinimeni.Proyeccion_Visitantes.ARIMA.B.txt")

##################################################
#98.PN.Laguna_San_Rafael
##################################################

sanrafael<-subset(dat0,dat0$Id2 =="98.PN.Laguna_San_Rafael")

str(sanrafael)
sanrafael

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.sanrafael<-ts(sanrafael$value, start=1990,end=2016, frequency = 1)

plot(visitors.sanrafael)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.sanrafael)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.sanrafael)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.sanrafael)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.sanrafael)[length(visitors.sanrafael)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_tbats$mean),
                         visitors_lower= round(f_tbats$lower[,2]),
                         visitors_upper=round(f_tbats$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.sanrafael))

forecast_df
write.table(forecast_df, file="98.PN.Laguna_San_Rafael.Proyeccion_TBATS.B.txt")




##################################################
#99.RN.Lago_Cochrane
##################################################

Lago_Cochrane<-subset(dat0,dat0$Id2 =="99.RN.Lago_Cochrane")

str(Lago_Cochrane)
Lago_Cochrane

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.Lago_Cochrane<-ts(Lago_Cochrane$value, start=1990,end=2016, frequency = 1)

plot(visitors.Lago_Cochrane)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.Lago_Cochrane)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.Lago_Cochrane)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.Lago_Cochrane)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.Lago_Cochrane)[length(visitors.Lago_Cochrane)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.Lago_Cochrane))

forecast_df
write.table(forecast_df, file="99.RN.Lago_Cochrane.Proyeccion_ARIMA.A.txt")



##################################################
#101.PN.Bernardo_Ohiggins.1
##################################################

BOH1<-subset(dat0,dat0$Id2 =="101.PN.Bernardo_Ohiggins.1")

str(BOH1)
BOH1

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.BOH1<-ts(BOH1$value, start=1990,end=2016, frequency = 1)

plot(visitors.BOH1)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.BOH1)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.BOH1)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.BOH1)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.BOH1)[length(visitors.BOH1)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.BOH1))

forecast_df
write.table(forecast_df, file="101.PN.Bernardo_Ohiggins.1.Proyeccion_ARIMA.C.txt")


##################################################
#102.PN.Bernardo_Ohiggins.2
##################################################

BOH2<-subset(dat0,dat0$Id2 =="102.PN.Bernardo_Ohiggins.2")

str(BOH2)
BOH2

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.BOH2<-ts(BOH2$value, start=1990,end=2016, frequency = 1)

plot(visitors.BOH2)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.BOH2)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.BOH2)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.BOH2)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.BOH2)[length(visitors.BOH2)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.BOH2))

forecast_df
write.table(forecast_df, file="102.PN.Bernardo_Ohiggins.2.Proyeccion_ARIMA.A.txt")


##################################################
#103.PN.Torres_del_Paine
##################################################

TPAINE<-subset(dat0,dat0$Id2 =="103.PN.Torres_del_Paine")

str(TPAINE)
TPAINE

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.TPAINE<-ts(TPAINE$value, start=1990,end=2016, frequency = 1)

plot(visitors.TPAINE)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.TPAINE)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.TPAINE)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.TPAINE)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.TPAINE)[length(visitors.TPAINE)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.TPAINE))

forecast_df
write.table(forecast_df, file="103.PN.Torres_del_Paine.Proyeccion_ARIMA_A.txt")

##################################################
#104.MN.Cueva_del_Milodon
##################################################

milodon<-subset(dat0,dat0$Id2 =="104.MN.Cueva_del_Milodon")

str(milodon)
milodon

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.milodon<-ts(milodon$value, start=1990,end=2016, frequency = 1)

plot(visitors.milodon)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.milodon)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.milodon)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.milodon)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.milodon)[length(visitors.milodon)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.milodon))

forecast_df
write.table(forecast_df, file="104.MN.Cueva_del_Milodon.Proyeccion_ARIMA.A.txt")





##################################################
#105.PN.Pali_Aike
##################################################

Pali_Aike<-subset(dat0,dat0$Id2 =="105.PN.Pali_Aike")

str(Pali_Aike)
Pali_Aike

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.Pali_Aike<-ts(Pali_Aike$value, start=1990,end=2016, frequency = 1)

plot(visitors.Pali_Aike)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.Pali_Aike)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.Pali_Aike)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.Pali_Aike)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.Pali_Aike)[length(visitors.Pali_Aike)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.Pali_Aike))

forecast_df
write.table(forecast_df, file="105.PN.Pali_Aike.Proyeccion_ARIMA_A.txt")



##################################################
#106.RN.Alacalufes
##################################################

Alacalufes<-subset(dat0,dat0$Id2 =="106.RN.Alacalufes")

str(Alacalufes)
Alacalufes

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.Alacalufes<-ts(Alacalufes$value, start=1990,end=2016, frequency = 1)

plot(visitors.Alacalufes)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.Alacalufes)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.Alacalufes)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.Alacalufes)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.Alacalufes)[length(visitors.Alacalufes)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.Alacalufes))

forecast_df
write.table(forecast_df, file="106.RN.Alacalufes.Proyeccion_ARIMA.B.txt")


##################################################
#107.MN.Los_Pinguinos
##################################################

Los_Pinguinos<-subset(dat0,dat0$Id2 =="107.MN.Los_Pinguinos")

str(Los_Pinguinos)
Los_Pinguinos

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.Los_Pinguinos<-ts(Los_Pinguinos$value, start=1990,end=2016, frequency = 1)

plot(visitors.Los_Pinguinos)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.Los_Pinguinos)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.Los_Pinguinos)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.Los_Pinguinos)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.Los_Pinguinos)[length(visitors.Los_Pinguinos)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.Los_Pinguinos))

forecast_df
write.table(forecast_df, file="107.MN.Los_Pinguinos.ARIMA.A.txt")




##################################################
#108.RN.Magallanes
##################################################

RN.Magallanes<-subset(dat0,dat0$Id2 =="108.RN.Magallanes")

str(RN.Magallanes)
RN.Magallanes

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.RN.Magallanes<-ts(RN.Magallanes$value, start=1990,end=2016, frequency = 1)

plot(visitors.RN.Magallanes)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.RN.Magallanes)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.RN.Magallanes)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.RN.Magallanes)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.RN.Magallanes)[length(visitors.RN.Magallanes)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.RN.Magallanes))

forecast_df
write.table(forecast_df, file="108.RN.Magallanes.Proyecciones.Arima_b.txt")




##################################################
#110.RN.Laguna_Parrillar
##################################################

Laguna_Parrillar<-subset(dat0,dat0$Id2 =="110.RN.Laguna_Parrillar")

str(Laguna_Parrillar)
Laguna_Parrillar

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.Laguna_Parrillar<-ts(Laguna_Parrillar$value, start=1990,end=2016, frequency = 1)

plot(visitors.Laguna_Parrillar)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.Laguna_Parrillar)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.Laguna_Parrillar)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.Laguna_Parrillar)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.Laguna_Parrillar)[length(visitors.Laguna_Parrillar)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.Laguna_Parrillar))

forecast_df
write.table(forecast_df, file="110.RN.Laguna_Parrillar.ProyeccionesArima_a.txt")





##################################################
#113.PN.Cabo_de_Hornos
##################################################

Cabo_de_Hornos<-subset(dat0,dat0$Id2 =="113.PN.Cabo_de_Hornos")

str(Cabo_de_Hornos)
Cabo_de_Hornos

#mAKE VISITOR VARIABLE A TIME SERIE OBJECT

visitors.Cabo_de_Hornos<-ts(Cabo_de_Hornos$value, start=1990,end=2016, frequency = 1)

plot(visitors.Cabo_de_Hornos)

#Model 1: Exponential State Smoothing

#The ets() function in the forecast package fits exponential state smoothing (ETS) models. 
#This function automically optimizes the choice of model and necessary parameters. 
#All you have to do is providing it with a time series.
#Let's use it and then make a forecast for the next 24 months.

m_ets = ets(visitors.Cabo_de_Hornos)
f_ets = forecast(m_ets, h=20) # forecast 24 months into the future
plot(f_ets)




#Model 2: ARIMA

#The auto.arima() function provides another modeling method. 
#More info on the ARIMA model can be found here. 
#The auto.arima() function automatically searches for the best model and optimizes the parameters. 
#Using the auto.arima() is almost always better than calling the Arima() function directly.

#Let's give it a shot.

m_aa = auto.arima(visitors.Cabo_de_Hornos)

acf(resid(m_aa))# los residuales parecen bien y no existe autocorrelacion salvo lag1 que es normal

f_aa = forecast(m_aa, h=20)
plot(f_aa)


#These confidence intervals seem bit smaller than those for the ETS model. Maybe this is because of a better fit to the data, but let's train a third model before doing a model comparison.
#Model 3: TBATS

#The last model I'm going to train is a TBATS model. 
#This model is designed for use when there are multiple cyclic patterns (e.g. daily, weekly and yearly patterns) in a single time series.
#Maybe it will be able to detect complicated patterns in our time series.

m_tbats = tbats(visitors.Cabo_de_Hornos)
f_tbats = forecast(m_tbats, h=20)
plot(f_tbats)


#Now we have three models that all seem to give reasonable predictions. Let's compare them to see which is performing the best.
#Model comparison

#I'm going to use AIC to compare the different models. AIC is common method for determining how well a model fits the data, while penalizing more complex models. The model with the smallest AIC is the best fitting model.

barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")


#We see that the ARIMA model performs the best. So, let's go ahead and turn our interactive R code into an R recipe that can be built into our DSS workflow.

#But before we can do this, we have to turn the output of forecast() into a data.frame, so that we can store it in DSS.

#First, I'm going to find the last date for which we have a measurement. 

last_date = index(visitors.Cabo_de_Hornos)[length(visitors.Cabo_de_Hornos)]

#Then, I'm going to create data.frame with the prediction for each year. 
#I'm also going to include the lower and upper bounds of the predictions, and the date. 
#Since we're representing dates by the year, each month is 1/12 of a year.


forecast_df = data.frame(visitors_predicted=round(f_aa$mean),
                         visitors_lower= round(f_aa$lower[,2]),
                         visitors_upper=round(f_aa$upper[,2]),
                         date=last_date + seq(1, 20, by=1))



diff(log(visitors.Cabo_de_Hornos))

forecast_df
write.table(forecast_df, file="113.PN.Cabo_de_Hornos.ProyeccionesArima_b.txt")
