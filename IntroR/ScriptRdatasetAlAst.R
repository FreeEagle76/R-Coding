######################################################################################
#Corso/Addestramento "Introduzione al linguaggio R" - Alessandria 22-24 Novembre 2016
#Scordino Pasquale
######################################################################################

#Preparazione aria di lavoro (workspace) e 
#cartella di lavoro (workdirectory)
ls()
getwd()
#setwd("/home/pasquale/Documenti/LavoroArpa/Data_Analysis/CorsoIntroR/dataset/datasetAlessandriaAsti")
setwd("/media/pasquale/Volume/DocumentiPasquale/LavoroARPA/CorsoIntroR/dataset/datasetAlessandriaAsti")

#Carico dati in R e formazione di un dataframe
HCl<-read.csv("HCL_052016.csv",
              header = TRUE, sep = ";", dec = ",", stringsAsFactors = FALSE)

#Carico librerie aggiuntive
library(dplyr)     # manipolazione dati
library(lubridate) # manipolazione date e ora

#Sostituzione delle etichette delle colonne
names(HCl)<-c("Data", "Ore", "Conc")

#Creazione unica colonna data ora 
HCl$Data<-paste(HCl$Data, HCl$Ore)
HCl$Data<-as.POSIXct(HCl$Data, format="%d/%m/%Y %H.%M.%S")

#Aggiunta di colonne scorporando la data "libreria lubridate"
HCl$Mese<- month(HCl$Data, label = TRUE)
HCl$Giorno<- day(HCl$Data)
HCl$GiornoW<- wday(HCl$Data, label = TRUE)
HCl$Ora<- hour(HCl$Data)
HCl$Minuti<- minute(HCl$Data)
HCl$Secondi<- second(HCl$Data)
HCl<-within(HCl, rm(Ore))

#Ciclo per sostituire le ore in formato da 0 a 24
#for (i in 1: length(HCl$Ora)) {
#  if (HCl$Ora[i] == 0) 
#    HCl$Ora[i]<- 24
#}

#Statistica esporativa dei dati e plot dati grezzi
summary(HCl$Conc)
length(HCl$Conc)

plot(HCl$Data, HCl$Conc, type = "l", col="red", lwd=2, 
     main = "Grafico esplorativo dati grezzi HCl - Maggio 2016",
     ylab = "µg/m³", xlab = "", sub = "Numero di dati = 44544")

boxplot(HCl$Conc, col="yellow", xlab="µg/m³", main="Boxplot dati HCl - Maggio 2016",
        horizontal = TRUE)

hist(HCl$Conc, probability=TRUE, col = "lightblue", 
     main = "Histogram of HCl - Maggio 2016", xlab = "concetrazioni in µg/m³")
lines(density(HCl$Conc),col="red", lwd="2")

#Test normalità
library(nortest)
lillie.test(HCl$Conc)

#Test outlier
library(outliers)

outlier(HCl$Conc)
outlier(HCl$Conc, opposite = T)

dixon.test(HCl$Conc)
grubbs.test(HCl$Conc, type = 11)


#Calcolo medie orarie con la funzione tapply
HourMean<-tapply(HCl$Conc, list(HCl$Giorno,HCl$Ora), mean)

#Creazione di un vettore vuoto
vec<-  c()

#Ciclo per le righe della matrice ed inserirle in una lista
for (i in 1:31) {
     vec[[i]]<- HourMean[i,]
}

#Inserimento degli elementi della lista in una matrice
df<-do.call(cbind, vec)


#Ciclo per trasformare la matrice dei dati orari in un vettore
v<- c(NULL)
for (i in 1:length(df[1,])) {
  v<-append(v, as.vector(df[,i]), after = (24*(i-1)))
}

#Elaborazioni
MediaOraria<- tapply(HCl$Conc, list(HCl$Giorno,HCl$Ora), mean)
GiornoMedio <- aggregate(Conc ~ Ora, HCl, mean)
MediaGiornaliera <- aggregate(Conc ~ Giorno, HCl, mean)
SettimanaMedia <- aggregate(Conc ~ GiornoW, HCl, mean)
MediaMensile<- mean(HCl$Conc)

#Creazione del dataframe delle medie orarie

start <- as.POSIXct("2016-05-01")
interval <- 60
end <- start + as.difftime(31, units="days")-1
Date<-seq(from=start, by=interval*60, to=end)

dfMediaOraria<-data.frame(Date, v)
names(dfMediaOraria)<- c("Date", "ConcHCl")

dfMediaOrariaMatrix<-as.matrix(dfMediaOraria)
dfMediaOrariaTS<-ts(dfMediaOraria$ConcHCl, frequency = 24, start = 1)

#Tabelle

Dir.Tabelle<-"/media/pasquale/Volume/DocumentiPasquale/LavoroARPA/CorsoIntroR/tabelle/"
#Dir.Tabelle<-"/home/pasquale/Documenti/LavoroArpa/Data_Analysis/CorsoIntroR/tabelle/"

write.csv(dfMediaOraria, paste0(Dir.Tabelle,"MediaOrariaHCl.csv"), row.names = FALSE)
write.csv(GiornoMedio, paste0(Dir.Tabelle,"GiornoMedioHCl.csv"), row.names = FALSE)


#Elaborazioni grafiche

plot(dfMediaOrariaTS, main = "Grafico delle medie orarie di HCl - Maggio 2016",
        ylab= "µg/m³", col="blue", lwd=2)
abline(h=NULL,v= 1:32, lty = 3, col = "gray")
abline(1.5,0, col="orange")
text(10, 1.54, "Soglia", col = "red")
points(1+91/24, dfMediaOrariaTS[91], col="red", pch= "*", cex= 2.7)
arrows((1+91/24)+2, (dfMediaOrariaTS[91]),(1+91/24)+0.2 ,dfMediaOrariaTS[91], 
       col = "green", lwd = 3, lty = 1, angle = 15)
text((1+91/24)+6, (dfMediaOrariaTS[91]), 
     paste("Valore massimo:", max(dfMediaOrariaTS), "µg/m³", sep = " "))
  
        
plot(GiornoMedio, type="l", col="brown", lwd=2, 
     main="Grafico giornata media concentrazioni di HCl - Maggio 2016",
     ylab="µg/m³")



plot(MediaGiornaliera, type="l", col="red", 
     main= "Grafico delle medie giornaliere di HCl - Maggio 2016", 
           ylab= "µg/m³", lwd=2)

barplot(SettimanaMedia$Conc, 	
        names.arg = c("Dom", "Lun", "Mar", "Mer", "Gio", "Ven", "Sab"),
        main = "Barplot Concentrazioni di HCl in µg/m³", 
        ylab = "µg/m³", col = "orange", sub = "Settimana media mese di maggio", 
        ylim = c(0, 1.5))


######################################################################################


