######################################################################################
#Corso/Addestramento "Introduzione al linguaggio R" - Alessandria 22-24 Novembre 2016
#Scordino Pasquale
######################################################################################

#Preparazione aria di lavoro (workspace) e 
#cartella di lavoro (workdirectory)
ls()
getwd()
#setwd("/home/pasquale/Documenti/LavoroArpa/Data_Analysis/CorsoIntroR/dataset/datasetAlessandriaAsti")
setwd("/media/pasquale/Volume/Documenti/DocumentiPasquale/LavoroARPA/CorsoIntroR/dataset/datasetAlessandriaAsti/")

#Carico librerie aggiuntive
library(dplyr)     # manipolazione dati
library(lubridate) # manipolazione date e ora
library(openair)   # elaborazioni dati QA

#Carico dati in R e formo un dataframe
BAU<-read.csv("BAU.csv",
              header = TRUE, sep = ",", stringsAsFactors = FALSE)

#Manipolo il dataframe per renderlo fruibile alle funzioni che applicherò

#Preparo il vettore Data da inserire al dataframe
start <- as.POSIXct("2009-01-01")
end <- as.POSIXct("2015-12-01")
Date<-seq(from=start, by="month", to=end)

BAU$date<- Date # inserimento del vettore Date nel df
BAU$Prog. <- NULL # rimozione della colonna Prog.
BAU$Anno<- as.factor(BAU$Anno) # trasformazione della colonna Anno in variabile categoriale
BAU$Mese<- as.factor(BAU$Mese)
levels(BAU$Mese) # controllo dei livelli della colonna Mese
#BAU$date<-as.POSIXct(BAU$date, format = "%Y-%m-%d")

#Statistica descrittiva
summary(BAU)

#Calcolo delle medie annuali
tapply(BAU$Media_NO2, BAU$Anno, mean)
tapply(BAU$Media_PM10, BAU$Anno, mean)

#Plot delle due variabili NO2 e PM10
plot(BAU$Media_NO2, type = "l", ylim =c(0,150), 
     main = "Plot delle variabili NO2(nera) e PM10(blu) - Dataset BAU 2009-2015", ylab = "µg/m³")
lines(BAU$Media_PM10, col= "blue")

#Trasformazione della variabile NO2 in modo da approssimare una distrivuzione normale
par(mfrow=c(2,2))
hist(BAU$Media_NO2, probability = T, col = "lightblue", border = "orange")
lines(density(BAU$Media_NO2), col="red")
hist(log10(BAU$Media_NO2), probability = T, col = "lightblue", border = "orange")
lines(density(log10(BAU$Media_NO2)), col="red")
qqnorm(BAU$Media_NO2, col = "lightblue", lwd="3")
qqline(BAU$Media_NO2, col = "red")
qqnorm(log10(BAU$Media_NO2), col = "lightblue", lwd="3")
qqline(log10(BAU$Media_NO2), col = "red")

#Analisi serie temporale con libreria "openair"

#Analisi con destagionalizzazione
TrendNO2_deS<-TheilSen(BAU, pollutant = "Media_NO2", deseason=T, 
                       main = "Dataset BAU - Analisi del trend")

#Estrazione dei dati dall'analisi di trend TheilSen "openair"
NO2Slope<-TrendNO2_deS$data$res2$slope
NO2Lower<-TrendNO2_deS$data$res2$lower
NO2Upper<-TrendNO2_deS$data$res2$upper
TableNO2_Trend<-data.frame(NO2Slope,NO2Lower,NO2Upper)

#Analisi senza destagionalizzazione
TheilSen(BAU, pollutant = "Media_NO2")

#Funzione per la visualizzazione cromatica dei valori
trendLevel(BAU, pollutant = "Media_NO2", main = "Dataset BAU 2009-2015")


#Controllo dispersione dati e outlier
BoxPlotNO2_Anni<-boxplot(BAU$Media_NO2~BAU$Anno, col = "lightblue", 
        border= "red", main="Boxplot medie mensili NO2 BAU",
        ylab="µg/m³", outcol="red", outbg="red", outpch=21)

#Estrazione dell'outlier per identificazione
BoxPlotNO2_Anni$out
BAU[BAU$Media_NO2 == 78,]

BoxPlotNO2_Mesi<-boxplot(BAU$Media_NO2~BAU$Mese, col = "lightblue", 
           border= "red", main="Boxplot medie mensili NO2 BAU",
           ylab="µg/m³", outcol="red", outbg="red", outpch=21)

#Estrazione dell'outlier per identificazione
BoxPlotNO2_Mesi$out
BAU[BAU$Media_NO2 == 65,]

#Analisi serie temporale con libreria "Trend" senza bootstrap
library(trend)
BAUtsNO2<-ts(BAU$Media_NO2, 2009, frequency = 12)
plot(BAUtsNO2)
res <- smk.test(BAUtsNO2)
summary.trend.test(res)
sea.sens.slope(BAUtsNO2)

# Analisi Serie Temporale con libreria standard di R
BAUtsNO2<-ts(BAU$Media_NO2, 2009, frequency = 12)
par(mfrow=c(2,1))
plot.ts(BAU$Media_NO2)
plot.ts(BAU$Media_PM10)
plot(stl(BAUtsNO2, s.window="periodic"))
######################################################################################