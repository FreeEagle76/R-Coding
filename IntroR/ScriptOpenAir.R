######################################################################################
#Corso/Addestramento "Introduzione al linguaggio R" - Alessandria 22-24 Novembre 2016
#Scordino Pasquale
######################################################################################

library(openair)
getwd()
#setwd("/home/pasquale/Documenti/LavoroArpa/Data_Analysis/CorsoIntroR/dataset/datasetAlessandriaAsti")
setwd("/media/pasquale/Volume/DocumentiPasquale/LavoroARPA/CorsoIntroR/dataset/datasetAlessandriaAsti")

#Carico dati in R e formazione di dei dataframe

HCl<- read.csv("SM.A.MU.NUOVO.HCL_20160501000100_20160531235900.csv",
              header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE)

HF<- read.csv("SM.A.MU.NUOVO.HF_20160501000100_20160531235900.csv",
             header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE)

C2F4<- read.csv("SM.A.MU.C2F4_20160501000100_20160531235900.csv",
                header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE)


  
#Preparazione dei dataframe

#Sostituzione delle etichette delle colonne
names(HCl)<-c("date", "Ore", "HCl")

#Creazione unica colonna data ora 
HCl$date<-paste(HCl$date, HCl$Ore)
HCl$date<-as.POSIXct(HCl$date, format="%d/%m/%Y %H.%M.%S")

#Eliminazione colonna che non interessa
HCl$Ore<- NULL
#oppure
HCl<-within(HCl, rm(Ore))

#Creazione di un dataframe con le medie orarie
HourHCl<-timeAverage(HCl, avg.time = "hour")
#Creazione di un dataframe con le medie giornaliere
DayHCl<-timeAverage(HCl, avg.time = "day")

#Analisi esplorativa
summary(HCl)
summary(HF)
summary(C2F4)

#Estrazione dato max trovato nella summary
C2F4[which.max(C2F4$mg.m3),]
plot(C2F4$mg.m3)

C2F4[which(C2F4$mg.m3 > 0),] #Probabile taratura analizzatore???

#Alcune Funzioni grafiche del package "openair"
timePlot(HCl, pollutant = "HCl", 
         main="TimePlot dati grezzi HCl - Maggio 2016", ylab="µg/m³")
timePlot(HourHCl, pollutant = "HCl", 
         main="TimePlot dati orari HCl - Maggio 2016", ylab="µg/m³")
timePlot(DayHCl, pollutant = "HCl", 
         main="TimePlot dati giornalieri HCl - Maggio 2016", ylab="µg/m³")

summaryPlot(HourHCl, pollutant = "HCl", 
            period = "months", 
            main = "SummaryPlot medie orarie di HCl in µg/m³ - Maggio 2016")

timeVariation(HourHCl, pollutant = "HCl", cols = "orange", 
              main="Analisi andamento medie orarie di HCl in µg/m³ - Maggio 2016")

#######################################################################################
#
#Analisi Dataset interno alla libreria openair
library("openair")
data(mydata)
str(mydata)

head(mydata)
names(mydata)

summary(mydata)

summaryPlot(mydata)
timePlot(mydata, pollutant = c("pm10", "pm25"), group = TRUE)
scatterPlot(mydata, "pm10", "pm25")
timeVariation(mydata, pollutant = "pm10")

#Codice per estrarre grafici singoli da timevariation plot
myplot<-timeVariation(mydata, pollutant = "pm10")
plot(myplot, subset="day.hour")

#Calcolo e inserimento nel df mydata della colonna media mobile 8 ore per l'ozono
mydata <- rollingMean(mydata, pollutant = "o3", width = 8, new.name =
                        "rollingo3", data.thresh = 75, align = "right")

polarPlot(mydata, pollutant = c("pm10", "pm25"), main="Polarplot PM")


# basic plot
polarPlot(mydata, pollutant = "nox")

# polarPlots by year on same scale
polarPlot(mydata, pollutant = "so2", type = "year", main = "polarPlot of so2")

# set minimum number of bins to be used to see if pattern remains similar
polarPlot(mydata, pollutant = "nox", min.bin = 3)

# plot by day of the week
polarPlot(mydata, pollutant = "pm10", type = "weekday")

# show the 95% confidence intervals in the surface fitting
polarPlot(mydata, pollutant = "so2", uncertainty = TRUE)

######################################################################################

#Analisi regressione lineare

head(mydata)

plot(mydata$co, mydata$nox)

scatterPlot(mydata, "co", "nox") # funzione di openair
corPlot(mydata) #funzione di openair

modello<-lm(mydata$nox~mydata$co)
summary(modello)

plot(mydata$co, mydata$nox)
abline(modello$coefficients, col="red")

scatterPlot(mydata, "co", "nox", linear = T) # funzione di openair
#######################################################################################
